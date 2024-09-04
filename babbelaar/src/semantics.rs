// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{borrow::Cow, collections::{HashMap, HashSet}, fmt::Display, rc::Rc};

use strum::AsRefStr;
use thiserror::Error;

use crate::{statement::VariableStatement, Attribute, BiExpression, Builtin, BuiltinFunction, BuiltinType, Expression, FileLocation, FileRange, ForStatement, FunctionCallExpression, FunctionStatement, IfStatement, MethodCallExpression, OptionExt, Parameter, ParseTree, PostfixExpression, PostfixExpressionKind, PrimaryExpression, Ranged, ReturnStatement, Statement, StatementKind, Structure, StructureInstantiationExpression, TemplateStringExpressionPart, Type, TypeSpecifier};

#[derive(Debug)]
pub struct SemanticAnalyzer<'source_code> {
    pub context: SemanticContext<'source_code>,
    diagnostics: Vec<SemanticDiagnostic<'source_code>>,
}

impl<'source_code> SemanticAnalyzer<'source_code> {
    #[must_use]
    pub fn new() -> Self {
        Self {
            context: SemanticContext::new(),
            diagnostics: Vec::new(),
        }
    }

    pub fn analyze_tree(&mut self, tree: &'source_code ParseTree<'source_code>) {
        // TODO: fix this.
        self.analyze_statements(tree.structures());
        self.analyze_statements(tree.functions());
        self.analyze_statements(tree.statements());
    }

    fn analyze_statements(&mut self, statements: &'source_code [Statement<'source_code>]) {
        for statement in statements {
            let StatementKind::Structure(structure) = &statement.kind else {
                continue;
            };

            self.analyze_structure(statement, structure);
        }

        for statement in statements {
            if let StatementKind::Function(function) = &statement.kind {
                self.context.push_function(SemanticFunction {
                    name: function.name,
                    parameters: &function.parameters,
                });
            }
        }

        for statement in statements {
            self.analyze_statement(statement);
        }
    }

    fn analyze_expression(&mut self, expression: &'source_code Expression<'source_code>) -> SemanticType<'source_code> {
        match expression {
            Expression::BiExpression(bi) => self.analyze_bi_expression(bi),
            Expression::Postfix(postfix) => self.analyze_postfix_expression(postfix),
            Expression::Primary(primary) => self.analyze_primary_expression(primary),
        }
    }

    fn analyze_function(&mut self, function: &'source_code FunctionStatement<'source_code>) {
        self.context.push_scope(function.range.start());

        for param in &function.parameters {
            let typ = self.resolve_type(&param.ty);
            self.context.scope.last_mut().unwrap().locals.insert(&param.name, SemanticLocal {
                kind: SemanticLocalKind::Parameter,
                declaration_range: param.name.range(),
                typ,
            });
        }

        self.analyze_statements(function.body.as_inner_slice());

        self.context.pop_scope(function.range.end());
    }

    pub fn analyze_statement(&mut self, statement: &'source_code Statement<'source_code>) {
        self.analyze_attributes_for_statement(statement);

        match &statement.kind {
            StatementKind::Expression(expr) => {
                self.analyze_expression(expr);
            }
            StatementKind::For(statement) => self.analyze_for_statement(statement),
            StatementKind::Function(function) => self.analyze_function(function),
            StatementKind::If(statement) => self.analyze_if_statement(statement),
            StatementKind::Return(function) => self.analyze_return_statement(function),
            StatementKind::Structure(..) => (),
            StatementKind::Variable(variable) => self.analyze_variable_statement(variable),
        }
    }

    fn analyze_for_statement(&mut self, statement: &'source_code ForStatement<'source_code>) {
        let scope = self.context.push_scope(statement.keyword.start());
        scope.locals.insert(&statement.iterator_name, SemanticLocal {
            kind: SemanticLocalKind::Iterator,
            declaration_range: statement.iterator_name.range(),
            typ: SemanticType::Builtin(BuiltinType::G32),
        });

        self.analyze_statements(&statement.body);

        self.context.pop_scope(statement.file_range.end());
    }

    fn analyze_if_statement(&mut self, statement: &'source_code IfStatement<'source_code>) {
        self.context.push_scope(statement.condition.range().start());

        self.analyze_expression(&statement.condition);

        self.analyze_statements(&statement.body);

        self.context.pop_scope(statement.range.end());
    }

    fn analyze_return_statement(&mut self, statement: &'source_code ReturnStatement<'source_code>) {
        if let Some(expression) = &statement.expression {
            self.analyze_expression(expression);
        }

        // TODO analyze return type
    }

    fn analyze_structure(&mut self, statement: &'source_code Statement<'source_code>, structure: &'source_code Structure<'source_code>) {
        let fields = structure.fields.iter().map(|x| SemanticField {
            attributes: &x.attributes,
            name: x.name,
            ty: self.resolve_type(&x.ty),
        }).collect();

        let structure = Rc::new(SemanticStructure {
            attributes: &statement.attributes,
            name: structure.name,
            fields,
        });

        self.context.push_structure(Rc::clone(&structure));

        let mut names = HashSet::new();
        for field in &structure.fields {
            if !names.insert(field.name.value()) {
                self.diagnostics.push(SemanticDiagnostic {
                    range: field.name.range(),
                    kind: SemanticDiagnosticKind::DuplicateFieldName { name: field.name.value() },
                });
            }

            self.analyze_attributes_for_field(field);
        }

        // TODO
    }

    fn analyze_variable_statement(&mut self, statement: &'source_code VariableStatement<'source_code>) {
        let typ = self.analyze_expression(&statement.expression);
        self.context.scope.last_mut().unwrap().locals.insert(&statement.name, SemanticLocal {
            kind: SemanticLocalKind::Variable,
            declaration_range: statement.name.range(),
            typ,
        });
    }

    fn analyze_bi_expression(&mut self, expression: &'source_code BiExpression<'source_code>) -> SemanticType<'source_code> {
        let lhs_type = self.analyze_expression(&expression.lhs);
        let rhs_type = self.analyze_expression(&expression.rhs);

        if lhs_type != rhs_type {
            self.diagnostics.push(SemanticDiagnostic {
                range: expression.operator.range(),
                kind: SemanticDiagnosticKind::IncompatibleTypes {
                    lhs_type: lhs_type.clone(),
                    rhs_type,
                }
            });
        }

        lhs_type
    }

    fn analyze_function_call_expression(&mut self, lhs: SemanticType<'source_code>, expression: &'source_code FunctionCallExpression<'source_code>) -> SemanticType<'source_code> {
        let function_name = match lhs {
            SemanticType::Builtin(builtin) => builtin.name(),
            SemanticType::Custom(custom) => &custom.name,
            SemanticType::Function(func) => func.name.value(),
            SemanticType::FunctionReference(func) => func.name(),
        };

        let Some(function) = self.find_function(&function_name) else {
            self.diagnostics.push(SemanticDiagnostic {
                range: expression.token_left_paren,
                kind: SemanticDiagnosticKind::InvalidFunctionReference { name: &function_name }
            });
            return SemanticType::Builtin(BuiltinType::Null);
        };

        let param_count = function.typ.parameter_count().expect("This reference to be a function, verified by self.find_function()");
        let arg_count = expression.arguments.len();

        if param_count > arg_count {
            self.diagnostics.push(SemanticDiagnostic {
                range: expression.token_right_paren,
                kind: SemanticDiagnosticKind::TooFewArguments { function_name, param_count, arg_count },
            });
        }

        if param_count < arg_count {
            self.diagnostics.push(SemanticDiagnostic {
                range: expression.token_right_paren,
                kind: SemanticDiagnosticKind::TooManyArguments { function_name, param_count, arg_count },
            });
        }

        for (arg_idx, arg) in expression.arguments.iter().enumerate() {
            let argument_type = self.analyze_expression(arg);
            let function = self.find_function(&function_name).unwrap();
            let parameter_type = self.resolve_parameter_type(&function, arg_idx);
            if argument_type != parameter_type {
                self.diagnostics.push(SemanticDiagnostic {
                    range: arg.range(),
                    kind: SemanticDiagnosticKind::IncompatibleArgumentParameterType {
                        argument_type,
                        parameter_type,
                    },
                })
            }
        }

        SemanticType::Builtin(BuiltinType::Null)
    }

    fn analyze_primary_expression(&mut self, expression: &'source_code PrimaryExpression<'source_code>) -> SemanticType<'source_code> {
        match expression {
            PrimaryExpression::Boolean(..) => {
                SemanticType::Builtin(BuiltinType::Bool)
            }

            PrimaryExpression::IntegerLiteral(..) => {
                SemanticType::Builtin(BuiltinType::G32)
            }

            PrimaryExpression::StringLiteral(..) => {
                SemanticType::Builtin(BuiltinType::Slinger)
            }

            PrimaryExpression::StructureInstantiation(structure) => self.analyze_structure_instantiation(structure),

            PrimaryExpression::TemplateString { parts } => {
                for part in parts {
                    match part {
                        TemplateStringExpressionPart::Expression(expr) => {
                            self.analyze_expression(expr);
                            // TODO analyze string convertible.
                        }

                        _ => (),
                    }
                }

                SemanticType::Builtin(BuiltinType::Slinger)
            }

            PrimaryExpression::Reference(reference) => {
                let Some(local) = self.find_local_by_name(|name| &name == reference.value()) else {
                    self.diagnostics.push(SemanticDiagnostic {
                        range: reference.range(),
                        kind: SemanticDiagnosticKind::InvalidIdentifierReference { identifier: &reference }
                    });
                    return SemanticType::Builtin(BuiltinType::Null);
                };

                let local_reference = SemanticReference {
                    local_name: reference.value(),
                    local_kind: local.kind,
                    declaration_range: local.declaration_range,
                    typ: local.typ.clone(),
                };

                let typ = local.typ.clone();
                if let Some(tracker) = &mut self.context.definition_tracker {
                    tracker.insert(reference.range(), local_reference);
                }

                typ
            }

            PrimaryExpression::Parenthesized(expr) => self.analyze_expression(expr),
        }
    }

    fn analyze_structure_instantiation(&mut self, instantiation: &'source_code StructureInstantiationExpression<'source_code>) -> SemanticType<'source_code> {
        let ty = self.resolve_type_by_name(&instantiation.name);
        let SemanticType::Custom(structure) = &ty else {
            return SemanticType::null();
        };

        let all_valid_fields: HashMap<&str, &SemanticField<'source_code>> = structure.fields.iter().map(|x| (*x.name.value(), x)).collect();
        let mut fields_left = all_valid_fields.clone();

        for field_instantiation in &instantiation.fields {
            let name = &field_instantiation.name;
            match fields_left.remove(name.value()) {
                Some(field) => {
                    let declaration_type = &field.ty;
                    let definition_type = self.analyze_expression(&field_instantiation.value);
                    if declaration_type != &definition_type {
                        self.diagnostics.push(SemanticDiagnostic {
                            range: name.range(),
                            kind: SemanticDiagnosticKind::IncompatibleFieldTypes {
                                struct_name: structure.name.value(),
                                field_name: name.value(),
                                declaration_type: declaration_type.to_string(),
                                definition_type: definition_type.to_string(),
                            },
                        });
                    }
                }
                None => {
                    if all_valid_fields.contains_key(name.value()) {
                        self.diagnostics.push(SemanticDiagnostic {
                            range: name.range(),
                            kind: SemanticDiagnosticKind::DuplicateFieldInstantiation { name: name.value() },
                        });
                    } else {
                        self.diagnostics.push(SemanticDiagnostic {
                            range: name.range(),
                            kind: SemanticDiagnosticKind::InvalidFieldInstantiation {
                                struct_name: structure.name.value(),
                                field_name: name.value()
                            },
                        });
                    }
                }
            }
        }

        ty
    }

    fn find_local_by_name<'this, P>(&'this self, predicate: P) -> Option<&SemanticLocal<'source_code>>
            where P: Fn(&str) -> bool {
        for scope in self.context.scope.iter().rev() {
            for (local_name, local) in &scope.locals {
                if predicate(local_name) {
                    return Some(local);
                }
            }
        }

        None
    }

    pub fn find_type_of_local(&self, name: &str) -> Option<SemanticType<'source_code>> {
        for scope in self.context.scope.iter().rev() {
            for (local_name, local) in &scope.locals {
                if *local_name == name {
                    return Some(local.typ.clone());
                }
            }
        }

        None
    }

    pub fn find_function_by_name<'this, P>(&'this self, predicate: P) -> Option<SemanticReference<'source_code>>
            where P: Fn(&str) -> bool {
        for scope in self.context.scope.iter().rev() {
            for (name, func) in &scope.locals {
                if !func.kind.is_function() {
                    continue;
                }

                if predicate(name) {
                    return Some(SemanticReference {
                        local_name: name,
                        local_kind: SemanticLocalKind::FunctionReference,
                        declaration_range: func.typ.declaration_range(),
                        typ: func.typ.clone(),
                    });
                }
            }
        }

        None
    }

    pub fn find_function<'this>(&'this self, name: &str) -> Option<SemanticReference<'source_code>> {
        self.find_function_by_name(|func| func == name)
    }

    pub fn find_reference(&self, range: FileRange) -> Option<SemanticReference<'source_code>> {
        self.context.definition_tracker.as_ref()?.get(&range).cloned()
    }

    pub fn find_reference_at(&self, location: FileLocation) -> Option<(FileRange, SemanticReference<'source_code>)> {
        for (range, reference) in self.context.definition_tracker.as_ref()? {
            if range.contains(location) {
                return Some((range.clone(), reference.clone()));
            }
        }

        None
    }

    #[must_use]
    pub fn into_diagnostics(self) -> Vec<SemanticDiagnostic<'source_code>> {
        self.diagnostics
    }

    #[must_use]
    pub fn resolve_type(&mut self, ty: &'source_code Ranged<Type<'source_code>>) -> SemanticType<'source_code> {
        match ty.specifier.value() {
            TypeSpecifier::BuiltIn(ty) => SemanticType::Builtin(*ty),
            TypeSpecifier::Custom { name } => self.resolve_type_by_name(name),
        }
    }

    #[must_use]
    fn resolve_type_by_name(&mut self, name: &Ranged<&'source_code str>) -> SemanticType<'source_code> {
        for scope in self.context.scope.iter().rev() {
            if let Some(structure) = scope.structures.get(name.value()) {
                return SemanticType::Custom(Rc::clone(structure));
            }
        }

        self.diagnostics.push(SemanticDiagnostic {
            range: name.range(),
            kind: SemanticDiagnosticKind::UnknownType { name: &name },
        });

        SemanticType::Builtin(BuiltinType::Null)
    }

    pub fn scopes_surrounding<F>(&self, location: FileLocation, mut f: F)
            where F: FnMut(&SemanticScope<'_>) {
        for scope in &self.context.previous_scopes {
            if scope.range.contains(location) {
                f(scope);
            }
        }

        for scope in &self.context.scope {
            if scope.range.contains(location) {
                f(scope);
            }
        }
    }

    fn resolve_parameter_type<'this>(&'this mut self, function: &SemanticReference<'source_code>, arg_idx: usize) -> SemanticType<'source_code> {
        match &function.typ {
            SemanticType::Builtin(..) => todo!(),
            SemanticType::Custom(..) => todo!(),
            SemanticType::Function(func) => {
                self.resolve_type(&func.parameters[arg_idx].ty)
            }
            SemanticType::FunctionReference(FunctionReference::Builtin(func)) => {
                SemanticType::Builtin(func.parameters[arg_idx].typ)
            }
            SemanticType::FunctionReference(FunctionReference::Custom(func)) => {
                self.resolve_type(&func.parameters[arg_idx].ty)
            }
        }
    }

    fn analyze_postfix_expression(&mut self, postfix: &'source_code PostfixExpression<'source_code>) -> SemanticType<'source_code> {
        let lhs = self.analyze_expression(&postfix.lhs);
        match &postfix.kind {
            PostfixExpressionKind::Call(call) => self.analyze_function_call_expression(lhs, call),
            PostfixExpressionKind::Member(member) => self.analyze_member_expression(lhs, *member),
            PostfixExpressionKind::MethodCall(method) => self.analyze_method_expression(lhs, method),
        }
    }

    fn analyze_member_expression(&mut self, typ: SemanticType<'source_code>, member: Ranged<&'source_code str>) -> SemanticType<'source_code> {
        if let SemanticType::Custom(structure) = &typ {
            for field in &structure.fields {
                if field.name.value() == member.value() {
                    return field.ty.clone();
                }
            }
        }

        self.diagnostics.push(SemanticDiagnostic {
            range: member.range(),
            kind: SemanticDiagnosticKind::InvalidMember { typ, name: member.value() }
        });

        SemanticType::null()
    }

    fn analyze_method_expression(&mut self, typ: SemanticType<'source_code>, expression: &MethodCallExpression<'source_code>) -> SemanticType<'source_code> {
        match typ {
            SemanticType::Builtin(builtin) => {
                for method in builtin.methods() {
                    if method.name == *expression.method_name {
                        return SemanticType::Builtin(method.return_type);
                    }
                }

                self.diagnostics.push(SemanticDiagnostic {
                    range: expression.method_name.range(),
                    kind: SemanticDiagnosticKind::InvalidMethod { typ, name: expression.method_name.value() }
                });

                SemanticType::null()
            }

            SemanticType::Custom(..) => {
                self.diagnostics.push(SemanticDiagnostic {
                    range: expression.method_name.range(),
                    kind: SemanticDiagnosticKind::InvalidMethod { typ, name: expression.method_name.value() }
                });

                SemanticType::null()
            }

            SemanticType::Function(..) | SemanticType::FunctionReference(..) => {
                self.diagnostics.push(SemanticDiagnostic {
                    range: expression.method_name.range(),
                    kind: SemanticDiagnosticKind::FunctionCannotHaveMethod { typ, name: expression.method_name.value() }
                });

                SemanticType::null()
            }
        }
    }

    fn analyze_attributes_for_statement(&mut self, statement: &Statement<'source_code>) {
        for attribute in &statement.attributes {
            self.diagnostics.push(SemanticDiagnostic {
                range: attribute.name.range(),
                kind: SemanticDiagnosticKind::UnknownAttribute { name: &attribute.name },
            });
        }
    }

    fn analyze_attributes_for_field(&mut self, field: &SemanticField<'source_code>) {
        for attribute in field.attributes {
            self.diagnostics.push(SemanticDiagnostic {
                range: attribute.name.range(),
                kind: SemanticDiagnosticKind::UnknownAttribute { name: &attribute.name },
            });
        }
    }
}

#[derive(Debug, Clone)]
pub struct SemanticDiagnostic<'source_code> {
    pub range: FileRange,
    pub kind: SemanticDiagnosticKind<'source_code>,
}

#[derive(Debug, Clone, Error, AsRefStr)]
pub enum SemanticDiagnosticKind<'source_code> {
    #[error("Functie `{name}` bestaat niet.")]
    InvalidFunctionReference { name: &'source_code str },

    #[error("Kon waarde `{identifier}` niet vinden binnen deze scoop.")]
    InvalidIdentifierReference { identifier: &'source_code str },

    #[error("Te weinig argument gegeven aan functie `{function_name}` ({arg_count} gegeven maar {param_count} verwacht)")]
    TooFewArguments {
        function_name: &'source_code str,
        param_count: usize,
        arg_count: usize,
    },

    #[error("Te veel argument gegeven aan functie `{function_name}` ({arg_count} gegeven maar {param_count} verwacht)")]
    TooManyArguments {
        function_name: &'source_code str,
        param_count: usize,
        arg_count: usize,
    },

    #[error("Type `{name}` is onbekend")]
    UnknownType {
        name: &'source_code str,
    },

    #[error("Types `{lhs_type}` en `{rhs_type}` zijn niet gelijksoortig.")]
    IncompatibleTypes {
        lhs_type: SemanticType<'source_code>,
        rhs_type: SemanticType<'source_code>,
    },

    #[error("Ongeldig argument gegeven voor functie: argument van type `{argument_type}` is niet gelijksoortig met parameter van type `{parameter_type}`.")]
    IncompatibleArgumentParameterType {
        argument_type: SemanticType<'source_code>,
        parameter_type: SemanticType<'source_code>,
    },

    #[error("Lid `{name}` bestaat niet binnen type `{typ}`")]
    InvalidMember {
        typ: SemanticType<'source_code>,
        name: &'source_code str,
    },

    #[error("Methode `{name}` bestaat niet binnen type `{typ}`")]
    InvalidMethod {
        typ: SemanticType<'source_code>,
        name: &'source_code str,
    },

    #[error("Type `{typ}` is een functie, en kan geen methodes bevatten.")]
    FunctionCannotHaveMethod {
        typ: SemanticType<'source_code>,
        name: &'source_code str,
    },

    #[error("Attribuut `{name}` is onbekend")]
    UnknownAttribute { name: &'source_code str, },

    #[error("Veldnaam `{name}` wordt meerdere keren gebruikt")]
    DuplicateFieldName { name: &'source_code str },

    #[error("Veld met naam `{name}` wordt meerdere keren een waarde toegekend")]
    DuplicateFieldInstantiation { name: &'source_code str },

    #[error("Structuur `{struct_name}` heeft geen veld genaamd `{field_name}`")]
    InvalidFieldInstantiation {
        struct_name: &'source_code str,
        field_name: &'source_code str,
    },

    #[error("Ongeldige waarde gegeven voor veld `{field_name}` in structuur `{struct_name}`. Veldtype `{declaration_type}` is niet gelijksoortig met definitie van `{definition_type}`.")]
    IncompatibleFieldTypes {
        struct_name: &'source_code str,
        field_name: &'source_code str,
        declaration_type: String,
        definition_type: String,
    },
}

impl<'source_code> SemanticDiagnosticKind<'source_code> {
    pub fn name(&self) -> &str {
        self.as_ref()
    }
}

#[derive(Debug)]
pub struct SemanticContext<'source_code> {
    scope: Vec<SemanticScope<'source_code>>,
    pub previous_scopes: Vec<SemanticScope<'source_code>>,

    pub definition_tracker: Option<HashMap<FileRange, SemanticReference<'source_code>>>,
}

impl<'source_code> SemanticContext<'source_code> {
    pub fn new() -> Self {
        Self {
            scope: vec![
                SemanticScope::new_top_level(),
            ],
            previous_scopes: Vec::new(),
            definition_tracker: Some(HashMap::new()),
        }
    }

    pub fn push_scope(&mut self, start: FileLocation) -> &mut SemanticScope<'source_code> {
        self.scope.push(SemanticScope {
            range: FileRange::new(start, start),
            locals: HashMap::new(),
            structures: HashMap::new(),
        });
        self.scope.last_mut().expect("we just pushed a scope")
    }

    fn push_function(&mut self, function: SemanticFunction<'source_code>) {
        self.scope.last_mut().unwrap().locals.insert(
            &function.name,
            SemanticLocal {
                kind: SemanticLocalKind::Function,
                declaration_range: function.name.range(),
                typ: SemanticType::Function(function),
            }
        );
    }

    fn push_structure(&mut self, structure: Rc<SemanticStructure<'source_code>>) {
        self.scope.last_mut().unwrap().structures.insert(structure.name.value(), structure);
    }

    fn pop_scope(&mut self, location: FileLocation) {
        let mut scope = self.scope.pop().unwrap();

        scope.range = FileRange::new(scope.range.start(), location);

        self.previous_scopes.push(scope);
    }
}

#[derive(Debug)]
pub struct SemanticScope<'source_code> {
    range: FileRange,
    pub locals: HashMap<&'source_code str, SemanticLocal<'source_code>>,
    pub structures: HashMap<&'source_code str, Rc<SemanticStructure<'source_code>>>,
}

impl<'source_code> SemanticScope<'source_code> {
    fn new_top_level() -> Self {
        let mut this = Self {
            range: FileRange::new(FileLocation::default(), FileLocation::new(usize::MAX, usize::MAX, usize::MAX)),
            structures: HashMap::new(),
            locals: HashMap::default(),
        };

        for func in Builtin::FUNCTIONS {
            this.locals.insert(&func.name, SemanticLocal {
                kind: SemanticLocalKind::Function,
                declaration_range: FileRange::default(),
                typ: SemanticType::FunctionReference(FunctionReference::Builtin(func)),
            });
        }

        this
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SemanticFunction<'source_code> {
    name: Ranged<&'source_code str>,
    parameters: &'source_code [Parameter<'source_code>],
    // type ...
}

impl<'source_code> Display for SemanticFunction<'source_code> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)?;
        f.write_str("())")
    }
}

impl<'source_code> PartialEq for SemanticFunction<'source_code> {
    fn eq(&self, other: &Self) -> bool {
        self.name.value() == other.name.value()
    }
}

#[derive(Debug)]
pub struct SemanticLocal<'source_code> {
    pub kind: SemanticLocalKind,
    pub declaration_range: FileRange,
    pub typ: SemanticType<'source_code>,
    // type ...
}

#[derive(Debug, Clone, PartialEq)]
pub struct SemanticReference<'source_code> {
    pub local_name: &'source_code str,
    pub local_kind: SemanticLocalKind,
    pub declaration_range: FileRange,
    pub typ: SemanticType<'source_code>,
}

impl<'source_code> SemanticReference<'source_code> {
    pub fn function_name(&self) -> &'source_code str {
        match &self.typ {
            SemanticType::Builtin(..) => todo!(),
            SemanticType::Custom(..) => todo!(),
            SemanticType::Function(func) => func.name.value(),
            SemanticType::FunctionReference(func) => func.name(),
        }
    }

    pub fn documentation(&self) -> Option<&'source_code str> {
        match &self.typ {
            SemanticType::Builtin(builtin) => Some(builtin.documentation()),
            SemanticType::Custom(..) => None,
            SemanticType::Function(..) => None,
            SemanticType::FunctionReference(func) => func.documentation(),
        }
    }

    pub fn inline_detail(&self) -> Option<&'source_code str> {
        match &self.typ {
            SemanticType::Builtin(builtin) => Some(builtin.inline_detail()),
            SemanticType::Custom(..) => None,
            SemanticType::Function(..) => None,
            SemanticType::FunctionReference(func) => func.inline_detail(),
        }
    }

    pub fn lsp_completion(&self) -> Cow<'source_code, str> {
        match &self.typ {
            SemanticType::Builtin(builtin) => Cow::Borrowed(builtin.name()),
            SemanticType::Custom(custom) => Cow::Borrowed(&custom.name),
            SemanticType::Function(func) => Cow::Owned(format!("{}($1);$0", func.name.value())),
            SemanticType::FunctionReference(func) => func.lsp_completion(),
        }
    }

    pub fn hover(&self) -> String {
        match self.local_kind {
            SemanticLocalKind::Function | SemanticLocalKind::FunctionReference => {
                format!("```bab\nfunctie {}(..)\n```", self.local_name)
            }

            _ => {
                format!("```bab\n{}: {}\n```", self.local_name, self.typ)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct SemanticField<'source_code> {
    pub attributes: &'source_code [Attribute<'source_code>],
    pub name: Ranged<&'source_code str>,
    pub ty: SemanticType<'source_code>,
}

#[derive(Debug, Clone)]
pub struct SemanticStructure<'source_code> {
    pub attributes: &'source_code [Attribute<'source_code>],
    pub name: Ranged<&'source_code str>,
    pub fields: Vec<SemanticField<'source_code>>,
}

impl<'source_code> Display for SemanticStructure<'source_code> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)
    }
}

impl<'source_code> PartialEq for SemanticStructure<'source_code> {
    fn eq(&self, other: &Self) -> bool {
        self.name.range() == other.name.range() && self.name.value() == other.name.value()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SemanticType<'source_code> {
    Builtin(BuiltinType),
    Custom(Rc<SemanticStructure<'source_code>>),
    Function(SemanticFunction<'source_code>),
    FunctionReference(FunctionReference<'source_code>),
}

impl<'source_code> SemanticType<'source_code> {
    #[must_use]
    pub fn null() -> Self {
        Self::Builtin(BuiltinType::Null)
    }

    pub fn declaration_range(&self) -> FileRange {
        match self {
            Self::Builtin(..) => FileRange::default(),
            Self::Custom(custom) => custom.name.range(),
            Self::Function(func) => func.name.range(),
            Self::FunctionReference(func) => func.declaration_range(),
        }
    }

    pub fn parameter_count(&self) -> Option<usize> {
        match self {
            Self::Builtin(..) => None,
            Self::Custom(..) => None,
            Self::Function(func) => Some(func.parameters.len()),
            Self::FunctionReference(func) => Some(func.parameter_count()),
        }
    }
}

impl<'source_code> Display for SemanticType<'source_code> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Builtin(typ) => typ.fmt(f),
            Self::Custom(custom) => custom.fmt(f),
            Self::Function(func) => func.fmt(f),
            Self::FunctionReference(func) => func.fmt(f),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SemanticLocalKind {
    Parameter,
    Iterator,
    Function,
    FunctionReference,
    Variable,
}

impl SemanticLocalKind {
    #[must_use]
    pub const fn is_function(&self) -> bool {
        match self {
            Self::Parameter => false,
            Self::Iterator => false,
            Self::Function => true,
            Self::FunctionReference => true,
            Self::Variable => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionReference<'source_code> {
    Builtin(&'static BuiltinFunction),
    Custom(SemanticFunction<'source_code>),
}

impl<'source_code> Display for FunctionReference<'source_code> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Builtin(func) => func.fmt(f),
            Self::Custom(func) => func.fmt(f),
        }
    }
}

impl<'source_code> FunctionReference<'source_code> {
    #[must_use]
    pub fn parameter_count(&self) -> usize {
        match self {
            Self::Builtin(func) => func.parameters.len(),
            Self::Custom(func) => func.parameters.len(),
        }
    }

    #[must_use]
    pub fn name(&self) -> &'source_code str {
        match self {
            Self::Builtin(func) => func.name,
            Self::Custom(func) => func.name.value(),
        }
    }

    #[must_use]
    pub fn documentation(&self) -> Option<&'source_code str> {
        match self {
            Self::Builtin(func) => Some(func.documentation),
            Self::Custom(..) => None,
        }
    }

    #[must_use]
    pub fn lsp_completion(&self) -> Cow<'source_code, str> {
        if let Some(completion) = self.lsp_completion_raw() {
            return Cow::Borrowed(completion);
        }

        let mut insert = format!("{}(", self.name());

        for i in 0..self.parameter_count() {
            let comma = if i == 0 { "" } else { ", " };
            insert += &format!("{comma}${}", i + 1);
        }

        insert += ");$0";
        Cow::Owned(insert)
    }

    #[must_use]
    fn lsp_completion_raw(&self) -> Option<&'source_code str> {
        match self {
            Self::Builtin(func) => func.lsp_completion,
            Self::Custom(..) => None,
        }
    }

    #[must_use]
    fn declaration_range(&self) -> FileRange {
        match self {
            Self::Builtin(..) => FileRange::default(),
            Self::Custom(func) => func.name.range(),
        }
    }

    #[must_use]
    fn inline_detail(&self) -> Option<&'source_code str> {
        match self {
            Self::Builtin(func) => Some(func.inline_detail),
            Self::Custom(..) => None,
        }
    }
}
