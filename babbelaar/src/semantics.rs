// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::{HashMap, HashSet}, fmt::Display, rc::Rc};

use log::warn;
use strum::AsRefStr;
use thiserror::Error;

use crate::{statement::VariableStatement, Attribute, BabString, BabbelaarCodeAction, BabbelaarCodeActionType, BiExpression, Builtin, BuiltinFunction, BuiltinType, Expression, FileEdit, FileLocation, FileRange, ForStatement, FunctionCallExpression, FunctionStatement, IfStatement, IntoBabString, Keyword, MethodCallExpression, OptionExt, Parameter, ParseTree, PostfixExpression, PostfixExpressionKind, PrimaryExpression, Ranged, ReturnStatement, SourceCode, Statement, StatementKind, StrExt, StrIterExt, Structure, StructureInstantiationExpression, TemplateStringExpressionPart, Type, TypeSpecifier};

#[derive(Debug)]
pub struct SemanticAnalyzer {
    pub context: SemanticContext,
    pub source_code: SourceCode,
    diagnostics: Vec<SemanticDiagnostic>,
}

impl SemanticAnalyzer {
    #[must_use]
    pub fn new(source_code: SourceCode) -> Self {
        Self {
            context: SemanticContext::new(),
            diagnostics: Vec::new(),
            source_code,
        }
    }

    pub fn analyze_tree(&mut self, tree: &ParseTree) {
        // TODO: fix this.
        self.analyze_statements(tree.structures());
        self.analyze_statements(tree.functions());
        self.analyze_statements(tree.statements());
    }

    fn analyze_statements(&mut self, statements: &[Statement]) {
        for statement in statements {
            let StatementKind::Structure(structure) = &statement.kind else {
                continue;
            };

            self.analyze_structure(statement, structure);
        }

        for statement in statements {
            if let StatementKind::Function(function) = &statement.kind {
                self.context.push_function(SemanticFunction {
                    name: function.name.clone(),
                    parameters: function.parameters.clone(),
                });
            }
        }

        for statement in statements {
            self.analyze_statement(statement);
        }
    }

    fn analyze_expression(&mut self, expression: &Ranged<Expression>) -> SemanticValue {
        match expression.value() {
            Expression::BiExpression(bi) => self.analyze_bi_expression(bi),
            Expression::Postfix(postfix) => self.analyze_postfix_expression(postfix),
            Expression::Primary(primary) => self.analyze_primary_expression(primary, expression.range()),
        }
    }

    fn analyze_function(&mut self, function: &FunctionStatement, this: Option<SemanticType>) {
        self.context.push_function_scope(function.range.start(), this);

        for param in &function.parameters {
            let typ = self.resolve_type(&param.ty);

            if let Some(tracker) = &mut self.context.definition_tracker {
                tracker.insert(param.ty.range(), SemanticReference {
                    local_name: param.ty.specifier.name(),
                    local_kind: SemanticLocalKind::StructureReference,
                    declaration_range: typ.declaration_range(),
                    typ: typ.clone(),
                });
            }

            self.context.push_local(&param.name, SemanticLocal {
                kind: SemanticLocalKind::Parameter,
                declaration_range: param.name.range(),
                typ,
            });
        }

        self.analyze_statements(function.body.as_inner_slice());

        self.context.pop_scope(function.range.end());
    }

    pub fn analyze_statement(&mut self, statement: &Statement) {
        self.analyze_attributes_for_statement(statement);

        match &statement.kind {
            StatementKind::Expression(expr) => {
                if let SemanticUsage::Pure(pure) = self.analyze_expression(expr).usage {
                    let diag = SemanticDiagnostic::new(expr.range(), SemanticDiagnosticKind::UnusedPureValue)
                        .warn()
                        .with_action(BabbelaarCodeAction::new(
                            BabbelaarCodeActionType::RemovePureStatement,
                            vec![
                                FileEdit::new(
                                    expr.range().as_full_line(),
                                    String::new()
                                )
                            ]
                        ));

                    let diag = match pure {
                        PureValue::FieldReference { declaration, name } => diag
                            .with_related(SemanticRelatedInformation::new(
                                declaration,
                                SemanticRelatedMessage::UsageOfPureValueField { name },
                            )),
                        _ => diag,
                    };

                    self.diagnostics.push(diag);
                }
            }
            StatementKind::Assignment(assign) => {
                self.analyze_assignment_destination(assign.range(), &assign.dest);
                self.analyze_expression(&assign.expression);
            }
            StatementKind::For(statement) => self.analyze_for_statement(statement),
            StatementKind::Function(function) => self.analyze_function(function, None),
            StatementKind::If(statement) => self.analyze_if_statement(statement),
            StatementKind::Return(function) => self.analyze_return_statement(function),
            StatementKind::Structure(..) => (),
            StatementKind::Variable(variable) => self.analyze_variable_statement(variable),
        }
    }

    fn analyze_assignment_destination(&mut self, range: FileRange, expression: &Expression) {
        match expression {
            Expression::Primary(PrimaryExpression::Reference(..)) => return,

            Expression::Postfix(postfix) => {
                if let PostfixExpressionKind::Member(..) = postfix.kind {
                    return;
                }
            }

            _ => (),
        }

        self.diagnostics.push(SemanticDiagnostic::new(
            range,
            SemanticDiagnosticKind::ExpressionCannotBeUsedAsAssignmentDestination {
                expression: expression.clone(),
            }
        ));
    }

    fn analyze_for_statement(&mut self, statement: &ForStatement) {
        let scope = self.context.push_block_scope(statement.keyword.start());
        scope.locals.insert(statement.iterator_name.value().clone(), SemanticLocal {
            kind: SemanticLocalKind::Iterator,
            declaration_range: statement.iterator_name.range(),
            typ: SemanticType::Builtin(BuiltinType::G32),
        });

        self.analyze_statements(&statement.body);

        self.context.pop_scope(statement.file_range.end());
    }

    fn analyze_if_statement(&mut self, statement: &IfStatement) {
        self.context.push_block_scope(statement.condition.range().start());

        self.analyze_expression(&statement.condition);

        self.analyze_statements(&statement.body);

        self.context.pop_scope(statement.range.end());
    }

    fn analyze_return_statement(&mut self, statement: &ReturnStatement) {
        if let Some(expression) = &statement.expression {
            self.analyze_expression(expression);
        }

        // TODO analyze return type
    }

    fn analyze_structure(&mut self, statement: &Statement, structure: &Structure) {
        let fields = structure.fields.iter().map(|x| SemanticField {
            attributes: x.attributes.clone(),
            name: x.name.clone(),
            ty: self.resolve_type(&x.ty),
        }).collect();

        let methods = structure.methods.iter().map(|x| SemanticMethod {
            function: SemanticFunction {
                name: x.function.name.clone(),
                parameters: x.function.parameters.clone(),
            }
        }).collect();

        let semantic_structure = Rc::new(SemanticStructure {
            attributes: statement.attributes.clone(),
            name: structure.name.clone(),
            left_curly_range: structure.left_curly_range,
            fields,
            methods,
        });

        self.context.push_structure(Rc::clone(&semantic_structure));

        let mut names = HashSet::new();
        for field in &semantic_structure.fields {
            if !names.insert(field.name.value()) {
                self.diagnostics.push(SemanticDiagnostic::new(
                    field.name.range(),
                    SemanticDiagnosticKind::DuplicateFieldName { name: field.name.value().clone() },
                ));
            }

            self.analyze_attributes_for_field(field);
        }

        let this_type = Some(SemanticType::Custom(Rc::clone(&semantic_structure)));

        let mut names = HashSet::new();
        for method in &structure.methods {
            if !names.insert(method.function.name.value()) {
                self.diagnostics.push(SemanticDiagnostic::new(
                    method.function.name.range(),
                    SemanticDiagnosticKind::DuplicateMethodName { name: method.function.name.value().clone(), structure: structure.name.value().clone() },
                ));
            }

            self.analyze_function(&method.function, this_type.clone());
        }
    }

    fn analyze_variable_statement(&mut self, statement: &VariableStatement) {
        let typ = self.analyze_expression(&statement.expression).ty;
        self.context.push_local(&statement.name, SemanticLocal {
            kind: SemanticLocalKind::Variable,
            declaration_range: statement.name.range(),
            typ,
        });
    }

    fn analyze_bi_expression(&mut self, expression: &BiExpression) -> SemanticValue {
        let lhs_type = self.analyze_expression(&expression.lhs).ty;
        let rhs_type = self.analyze_expression(&expression.rhs).ty;

        if lhs_type != rhs_type {
            self.diagnostics.push(SemanticDiagnostic::new(
                expression.operator.range(),
                SemanticDiagnosticKind::IncompatibleTypes {
                    lhs_type: lhs_type.clone(),
                    rhs_type,
                }
            ));
        }

        SemanticValue {
            ty: lhs_type,
            usage: SemanticUsage::Pure(PureValue::Operator {
                operator_range: expression.operator.range(),
            }),
        }
    }

    fn analyze_function_call_expression(&mut self, lhs: SemanticType, expression: &FunctionCallExpression) -> SemanticValue {
        let function_name = match lhs {
            SemanticType::Builtin(builtin) => builtin.name(),
            SemanticType::Custom(custom) => custom.name.value().clone(),
            SemanticType::Function(func) => func.name.value().clone(),
            SemanticType::FunctionReference(func) => func.name(),
        };

        let Some(function) = self.find_function(&function_name) else {
            self.diagnostics.push(SemanticDiagnostic::new(
                expression.token_left_paren,
                SemanticDiagnosticKind::InvalidFunctionReference { name: function_name.clone() }
            ));
            return SemanticValue::null();
        };

        self.analyze_function_parameters(function_name, function, expression);

        SemanticValue::null()
    }

    fn analyze_function_parameters(
        &mut self,
        function_name: BabString,
        function: SemanticReference,
        expression: &FunctionCallExpression,
    ) {
        let function_hint = SemanticRelatedInformation::new(
            function.declaration_range,
            SemanticRelatedMessage::FunctionDefinedHere { name: function_name.clone() }
        );

        let param_count = function.typ.parameter_count().expect("This reference to be a function, verified by self.find_function()");
        let arg_count = expression.arguments.len();

        if param_count > arg_count {
            self.diagnostics.push(SemanticDiagnostic::new(
                expression.token_right_paren,
                SemanticDiagnosticKind::TooFewArguments { function_name: function_name.clone(), param_count, arg_count },
            ).with_related(function_hint.clone()));
        }

        if param_count < arg_count {
            self.diagnostics.push(SemanticDiagnostic::new(
                expression.token_right_paren,
                SemanticDiagnosticKind::TooManyArguments { function_name: function_name.clone(), param_count, arg_count },
            ).with_related(function_hint.clone()));
            return;
        }

        for (arg_idx, arg) in expression.arguments.iter().enumerate() {
            let argument_type = self.analyze_expression(arg).ty;

            let Some(parameter_type) = self.resolve_parameter_type(&function, arg_idx) else {
                warn!("Cannot check type of parameter with index {arg_idx}");
                break;
            };

            if argument_type != parameter_type {
                let param_hint = self.resolve_parameter_name(&function, arg_idx)
                    .map(|x| SemanticRelatedInformation::new(
                        x.range(),
                        SemanticRelatedMessage::ParameterDeclaredHere { name: x.value().clone()}
                    ));

                self.diagnostics.push(SemanticDiagnostic::new(
                    arg.range(),
                    SemanticDiagnosticKind::IncompatibleArgumentParameterType {
                        argument_type,
                        parameter_type,
                    },
                ).with_related(param_hint).with_related(function_hint.clone()))
            }
        }
    }

    fn analyze_primary_expression(&mut self, expression: &PrimaryExpression, range: FileRange) -> SemanticValue {
        let ty = match expression {
            PrimaryExpression::Boolean(..) => {
                SemanticType::Builtin(BuiltinType::Bool)
            }

            PrimaryExpression::IntegerLiteral(..) => {
                SemanticType::Builtin(BuiltinType::G32)
            }

            PrimaryExpression::StringLiteral(..) => {
                SemanticType::Builtin(BuiltinType::Slinger)
            }

            PrimaryExpression::ReferenceThis => {
                let Some(scope) = self.context.scope.last() else {
                    log::warn!("How come we have no scope?");
                    debug_assert!(false);
                    return SemanticValue {
                        ty: SemanticType::null(),
                        usage: SemanticUsage::Indifferent
                    };
                };

                if let Some(this) = &scope.this {
                    if let Some(tracker) = &mut self.context.definition_tracker {
                        tracker.insert(range, SemanticReference {
                            local_name: BabString::new_static(Keyword::Dit.as_ref()),
                            local_kind: SemanticLocalKind::ReferenceThis,
                            declaration_range: this.declaration_range(),
                            typ: this.clone(),
                        });
                    }

                    this.clone()
                } else {
                    let diag = SemanticDiagnostic::new(range, SemanticDiagnosticKind::ThisOutsideStructure)
                        .with_related(SemanticRelatedInformation::new(scope.range, SemanticRelatedMessage::WerkwijzeNotInsideStructuur));
                    self.diagnostics.push(diag);
                    SemanticType::null()
                }
            }

            PrimaryExpression::StructureInstantiation(structure) => self.analyze_structure_instantiation(structure).ty,

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
                let Some(local) = self.find_local_by_name(|name| name == reference.value()) else {
                    self.diagnostics.push(SemanticDiagnostic::new(
                        reference.range(),
                        SemanticDiagnosticKind::InvalidIdentifierReference { identifier: reference.value().clone() }
                    ));
                    return SemanticValue::null();
                };

                let local_reference = SemanticReference {
                    local_name: reference.value().clone(),
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

            PrimaryExpression::Parenthesized(expr) => return self.analyze_expression(expr),
        };

        SemanticValue {
            ty,
            usage: SemanticUsage::Pure(PureValue::ConstantValue),
        }
    }

    fn analyze_structure_instantiation(&mut self, instantiation: &StructureInstantiationExpression) -> SemanticValue {
        let ty = self.resolve_type_by_name(&instantiation.name);
        let SemanticType::Custom(structure) = &ty else {
            return SemanticValue::null();
        };

        let struct_hint = SemanticRelatedInformation::new(
            structure.name.range(),
            SemanticRelatedMessage::StructureDefinedHere { name: structure.name.value().clone() }
        );

        let all_valid_fields: HashMap<&BabString, &SemanticField> = structure.fields.iter().map(|x| (x.name.value(), x)).collect();
        let mut fields_left = all_valid_fields.clone();

        if let Some(tracker) = &mut self.context.definition_tracker {
            tracker.insert(instantiation.name.range(), SemanticReference {
                local_name: instantiation.name.value().clone(),
                local_kind: SemanticLocalKind::StructureReference,
                declaration_range: structure.name.range(),
                typ: SemanticType::Custom(Rc::clone(&structure)),
            });
        }

        for field_instantiation in &instantiation.fields {
            let name = &field_instantiation.name;
            match fields_left.remove(name.value()) {
                Some(field) => {
                    let declaration_type = &field.ty;
                    let definition_type = self.analyze_expression(&field_instantiation.value).ty;
                    if declaration_type != &definition_type {
                        let action = self.try_create_conversion_action(declaration_type, &definition_type, field_instantiation);

                        self.diagnostics.push(SemanticDiagnostic::new(
                            field_instantiation.value.range(),
                            SemanticDiagnosticKind::IncompatibleFieldTypes {
                                struct_name: structure.name.value().clone(),
                                field_name: name.value().clone(),
                                declaration_type: declaration_type.to_string(),
                                definition_type: definition_type.to_string(),
                            },
                        ).with_related(struct_hint.clone()).with_action(action));
                    }

                    if let Some(tracker) = &mut self.context.definition_tracker {
                        tracker.insert(field_instantiation.name.range(), SemanticReference {
                            local_name: field.name.value().clone(),
                            local_kind: SemanticLocalKind::FieldReference,
                            declaration_range: field.name.range(),
                            typ: field.ty.clone(),
                        });
                    }
                }
                None => {
                    if let Some(duplicate_field) = all_valid_fields.get(name.value()) {
                        let field_def_hint = SemanticRelatedInformation::new(
                            duplicate_field.name.range(),
                            SemanticRelatedMessage::DuplicateFieldFirstUse { name: duplicate_field.name.value().clone() }
                        );

                        let diag = SemanticDiagnostic::new(
                            name.range(),
                            SemanticDiagnosticKind::DuplicateFieldInstantiation { name: name.value().clone()},
                        );

                        self.diagnostics.push(diag.with_related(field_def_hint).with_related(struct_hint.clone()));
                    } else {
                        let definition_type = self.analyze_expression(&field_instantiation.value).ty;
                        let create_field_action = self.create_action_create_field(&structure, &field_instantiation.name, definition_type);

                        self.diagnostics.push(
                            SemanticDiagnostic::new(
                                name.range(),
                                SemanticDiagnosticKind::InvalidFieldInstantiation {
                                    struct_name: structure.name.value().clone(),
                                    field_name: name.value().clone()
                                },
                            )
                            .with_action(create_field_action)
                            .with_related(struct_hint.clone()),
                        );
                    }
                }
            }
        }

        if !fields_left.is_empty() {
            let names = fields_left.keys().map(|x| x.as_str()).join("`, `");
            self.diagnostics.push(SemanticDiagnostic::new(instantiation.range, SemanticDiagnosticKind::MissingFieldInitializers {
                names,
                field_word: if fields_left.len() == 1 {
                    "Het veld"
                } else {
                    "De velden"
                },
            }));
        }

        SemanticValue {
            ty,
            usage: SemanticUsage::Pure(PureValue::ConstantValue),
        }
    }

    fn find_local_by_name<'this, P>(&'this self, predicate: P) -> Option<&'this SemanticLocal>
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

    pub fn find_type_of_local(&self, name: &str) -> Option<SemanticType> {
        for scope in self.context.scope.iter().rev() {
            for (local_name, local) in &scope.locals {
                if *local_name == name {
                    return Some(local.typ.clone());
                }
            }
        }

        None
    }

    pub fn find_function_by_name<'this, P>(&'this self, predicate: P) -> Option<SemanticReference>
            where P: Fn(&str) -> bool {
        for scope in self.context.scope.iter().rev() {
            for (name, func) in &scope.locals {
                if !func.kind.is_function() {
                    continue;
                }

                if predicate(name) {
                    return Some(SemanticReference {
                        local_name: name.clone(),
                        local_kind: SemanticLocalKind::FunctionReference,
                        declaration_range: func.typ.declaration_range(),
                        typ: func.typ.clone(),
                    });
                }
            }
        }

        None
    }

    pub fn find_function<'this>(&'this self, name: &str) -> Option<SemanticReference> {
        self.find_function_by_name(|func| func == name)
    }

    pub fn find_reference(&self, range: FileRange) -> Option<SemanticReference> {
        self.context.definition_tracker.as_ref()?.get(&range).cloned()
    }

    #[must_use]
    pub fn find_declaration_range_at(&self, location: FileLocation) -> Option<(FileRange, SemanticReference)> {
        if let Some(tracker) = &self.context.declaration_tracker {
            if let Some(reference) = tracker.iter().find(|x| x.declaration_range.contains(location)).cloned() {
                return Some((reference.declaration_range, reference));
            }
        }

        for (range, reference) in self.context.definition_tracker.as_ref()? {
            if range.contains(location) {
                return Some((*range, reference.clone()));
            }

            if reference.declaration_range.contains(location) {
                return Some((reference.declaration_range, reference.clone()));
            }
        }

        None
    }

    pub fn find_reference_at(&self, location: FileLocation) -> Option<(FileRange, SemanticReference)> {
        for (range, reference) in self.context.definition_tracker.as_ref()? {
            if range.contains(location) {
                return Some((range.clone(), reference.clone()));
            }
        }

        None
    }

    #[must_use]
    pub fn find_references_of(&self, declaration_range: FileRange) -> Option<HashSet<FileRange>> {
        let mut result = HashSet::new();

        for (range, reference) in self.context.definition_tracker.as_ref()? {
            if reference.declaration_range == declaration_range {
                result.insert(*range);
            }
        }

        Some(result)
    }

    #[must_use]
    pub fn into_diagnostics(self) -> Vec<SemanticDiagnostic> {
        self.diagnostics
    }

    #[must_use]
    pub fn resolve_type(&mut self, ty: &Ranged<Type>) -> SemanticType {
        match ty.specifier.value() {
            TypeSpecifier::BuiltIn(ty) => SemanticType::Builtin(*ty),
            TypeSpecifier::Custom { name } => self.resolve_type_by_name(name),
        }
    }

    #[must_use]
    fn resolve_type_by_name(&mut self, name: &Ranged<BabString>) -> SemanticType {
        for scope in self.context.scope.iter().rev() {
            if let Some(structure) = scope.structures.get(name.value()) {
                return SemanticType::Custom(Rc::clone(structure));
            }
        }

        self.diagnostics.push(SemanticDiagnostic::new(
            name.range(),
            SemanticDiagnosticKind::UnknownType { name: name.value().clone() },
        ));

        SemanticType::Builtin(BuiltinType::Null)
    }

    pub fn scopes_surrounding<F>(&self, location: FileLocation, mut f: F)
            where F: FnMut(&SemanticScope) {
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

    fn resolve_parameter_name<'this>(&'this mut self, function: &SemanticReference, arg_idx: usize) -> Option<Ranged<BabString>> {
        match &function.typ {
            SemanticType::Builtin(..) => todo!(),
            SemanticType::Custom(..) => todo!(),
            SemanticType::Function(func) => {
                Some(func.parameters[arg_idx].name.clone())
            }
            SemanticType::FunctionReference(FunctionReference::Builtin(..)) => {
                None
            }
            SemanticType::FunctionReference(FunctionReference::Custom(func)) => {
                Some(func.name.clone())
            }
        }
    }

    fn resolve_parameter_type<'this>(&'this mut self, function: &SemanticReference, arg_idx: usize) -> Option<SemanticType> {
        Some(match &function.typ {
            SemanticType::Builtin(..) => todo!(),
            SemanticType::Custom(..) => todo!(),
            SemanticType::Function(func) => {
                self.resolve_type(&func.parameters.get(arg_idx)?.ty)
            }
            SemanticType::FunctionReference(FunctionReference::Builtin(func)) => {
                SemanticType::Builtin(func.parameters.get(arg_idx)?.typ)
            }
            SemanticType::FunctionReference(FunctionReference::Custom(func)) => {
                self.resolve_type(&func.parameters.get(arg_idx)?.ty)
            }
        })
    }

    fn analyze_postfix_expression(&mut self, postfix: &PostfixExpression) -> SemanticValue {
        let lhs = self.analyze_expression(&postfix.lhs).ty;
        match &postfix.kind {
            PostfixExpressionKind::Call(call) => self.analyze_function_call_expression(lhs, call),
            PostfixExpressionKind::Member(member) => self.analyze_member_expression(lhs, member),
            PostfixExpressionKind::MethodCall(method) => self.analyze_method_expression(lhs, method),
        }
    }

    fn analyze_member_expression(&mut self, typ: SemanticType, member: &Ranged<BabString>) -> SemanticValue {
        let SemanticType::Custom(structure) = &typ else {
            self.diagnostics.push(SemanticDiagnostic::new(
                member.range(),
                SemanticDiagnosticKind::InvalidMember { typ, name: member.value().clone() }
            ));

            return SemanticValue::null()
        };

        for field in &structure.fields {
            if field.name.value() == member.value() {
                if let Some(tracker) = &mut self.context.definition_tracker {
                    tracker.insert(member.range(), SemanticReference {
                        local_name: member.value().clone(),
                        local_kind: SemanticLocalKind::FieldReference,
                        declaration_range: field.name.range(),
                        typ: field.ty.clone(),
                    });
                }

                return SemanticValue {
                    ty: field.ty.clone(),
                    usage: SemanticUsage::Pure(PureValue::FieldReference {
                        declaration: field.name.range(),
                        name: field.name.value().clone(),
                    }),
                };
            }
        }

        let struct_hint = SemanticRelatedInformation::new(
            structure.name.range(),
            SemanticRelatedMessage::StructureDefinedHere { name: structure.name.value().clone() }
        );

        let diag = SemanticDiagnostic::new(
            member.range(),
            SemanticDiagnosticKind::InvalidMember { typ, name: member.value().clone() }
        );

        self.diagnostics.push(diag.with_related(struct_hint));

        SemanticValue::null()
    }

    fn analyze_method_expression(&mut self, typ: SemanticType, expression: &MethodCallExpression) -> SemanticValue {
        match typ {
            SemanticType::Builtin(builtin) => {
                for method in builtin.methods() {
                    if *expression.method_name == method.name {
                        return SemanticValue {
                            ty: SemanticType::Builtin(method.return_type),
                            usage: if method.must_use { SemanticUsage::Pure(PureValue::ReturnValue) } else { SemanticUsage::Indifferent },
                        };
                    }
                }

                self.diagnostics.push(SemanticDiagnostic::new(
                    expression.method_name.range(),
                    SemanticDiagnosticKind::InvalidMethod { typ, name: expression.method_name.value().clone()}
                ));

                SemanticValue::null()
            }

            SemanticType::Custom(ref custom) => {
                for method in &custom.methods {
                    if *method.name() == *expression.method_name {
                        let local_reference = SemanticReference {
                            local_name: method.name().clone(),
                            local_kind: SemanticLocalKind::Method,
                            declaration_range: method.function.name.range(),
                            typ: SemanticType::FunctionReference(FunctionReference::Custom(method.function.clone())), // is this okay?
                        };

                        if let Some(tracker) = &mut self.context.definition_tracker {
                            tracker.insert(expression.method_name.range(), local_reference.clone());
                        }

                        self.analyze_function_parameters(method.name().clone(), local_reference, &expression.call);

                        return SemanticValue {
                            ty: method.return_type(),
                            usage: method.return_type_usage(),
                        };
                    }
                }

                let struct_hint = SemanticRelatedInformation::new(
                    custom.name.range(),
                    SemanticRelatedMessage::StructureDefinedHere { name: custom.name.value().clone() }
                );

                let diag = SemanticDiagnostic::new(
                    expression.method_name.range(),
                    SemanticDiagnosticKind::InvalidMethod { typ, name: expression.method_name.value().clone() }
                );

                self.diagnostics.push(diag.with_related(struct_hint));

                SemanticValue::null()
            }

            SemanticType::Function(..) | SemanticType::FunctionReference(..) => {
                self.diagnostics.push(SemanticDiagnostic::new(
                    expression.method_name.range(),
                    SemanticDiagnosticKind::FunctionCannotHaveMethod { typ, name: expression.method_name.value().clone() }
                ));

                // default SemanticUsage of functions should be default-must_use
                SemanticValue::null()
            }
        }
    }

    fn analyze_attributes_for_statement(&mut self, statement: &Statement) {
        for attribute in &statement.attributes {
            self.diagnostics.push(SemanticDiagnostic::new(
                attribute.name.range(),
                SemanticDiagnosticKind::UnknownAttribute { name: attribute.name.value().clone() },
            ));
        }
    }

    fn analyze_attributes_for_field(&mut self, field: &SemanticField) {
        for attribute in &field.attributes {
            self.diagnostics.push(SemanticDiagnostic::new(
                attribute.name.range(),
                SemanticDiagnosticKind::UnknownAttribute { name: attribute.name.value().clone() },
            ));
        }
    }

    fn try_create_conversion_action(
        &self,
        declaration_type: &SemanticType,
        definition_type: &SemanticType,
        field_instantiation: &crate::FieldInstantiation,
    ) -> Option<BabbelaarCodeAction> {
        let SemanticType::Builtin(declaration_type) = declaration_type else { return None };
        let SemanticType::Builtin(definition_type) = definition_type else { return None };

        if *declaration_type == BuiltinType::G32 && *definition_type == BuiltinType::Slinger {
            if let Expression::Primary(PrimaryExpression::StringLiteral(literal)) = field_instantiation.value.value().clone(){
                if let Ok(value) = literal.trim().parse::<isize>() {
                    return Some(BabbelaarCodeAction::new(
                        BabbelaarCodeActionType::ChangeStringToNumber { number: value, },
                        vec![
                            FileEdit::new(
                                field_instantiation.value.range(),
                                value.to_string(),
                            )
                        ]
                    ));
                }
            }
        }

        None
    }

    #[must_use]
    fn create_action_create_field(&self, structure: &SemanticStructure, name: &str, ty: SemanticType) -> BabbelaarCodeAction {
        let (add_location, add_text) = if let Some(last) = structure.fields.last() {
            let indent = self.source_code.indentation_at(last.name.range().start()).unwrap_or_default();
            let location = FileLocation::new(0, last.name.range().start().line(), usize::MAX);

            (location, format!("\n{indent}veld {name}: {ty},"))
        } else {
            let indent = self.source_code.indentation_at(structure.name.range().start()).unwrap_or_default();

            (structure.left_curly_range.end(), format!("\n{indent}    veld {name}: {ty},"))
        };

        BabbelaarCodeAction::new(
            BabbelaarCodeActionType::CreateField { name: name.to_string() },
            vec![FileEdit::new(add_location.as_zero_range(), add_text)]
        )
    }
}

#[derive(Debug, Clone)]
pub struct SemanticDiagnostic {
    range: FileRange,
    kind: SemanticDiagnosticKind,
    severity: SemanticDiagnosticSeverity,
    related: Vec<SemanticRelatedInformation>,
    actions: Vec<BabbelaarCodeAction>,
}

impl SemanticDiagnostic {
    #[must_use]
    pub fn new(range: FileRange, kind: SemanticDiagnosticKind) -> Self {
        Self {
            range,
            kind,
            severity: SemanticDiagnosticSeverity::Error,
            related: Vec::new(),
            actions: Vec::new(),
        }
    }

    #[must_use]
    pub fn range(&self) -> FileRange {
        self.range
    }

    #[must_use]
    pub fn kind(&self) -> &SemanticDiagnosticKind {
        &self.kind
    }

    #[must_use]
    pub fn related_info(&self) -> &[SemanticRelatedInformation] {
        &self.related
    }

    #[must_use]
    fn with_related(mut self, info: impl Into<Option<SemanticRelatedInformation>>) -> Self {
        if let Some(info) = info.into() {
            self.related.push(info);
        }

        self
    }

    #[must_use]
    pub fn actions(&self) -> &[BabbelaarCodeAction] {
        &self.actions
    }

    fn with_action(mut self, action: impl Into<Option<BabbelaarCodeAction>>) -> Self {
        if let Some(action) = action.into() {
            self.actions.push(action);
        }

        self
    }

    #[must_use]
    pub fn severity(&self) -> SemanticDiagnosticSeverity {
        self.severity
    }

    #[must_use]
    fn warn(mut self) -> Self {
        self.severity = SemanticDiagnosticSeverity::Warning;
        self
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SemanticDiagnosticSeverity {
    Error,
    Warning,
}

impl Display for SemanticDiagnosticSeverity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Error => f.write_str("Fout"),
            Self::Warning => f.write_str("Waarschuwing"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SemanticRelatedInformation {
    range: FileRange,
    message: SemanticRelatedMessage,
}

impl SemanticRelatedInformation {
    #[must_use]
    pub fn new(range: FileRange, message: SemanticRelatedMessage) -> Self {
        Self {
            range,
            message: message.into(),
        }
    }

    #[must_use]
    pub fn range(&self) -> FileRange {
        self.range
    }

    #[must_use]
    pub fn message(&self) -> &SemanticRelatedMessage {
        &self.message
    }
}

#[derive(Debug, Clone, Error, AsRefStr)]
pub enum SemanticRelatedMessage {
    #[error("`{name}` is hier voor het eerst geïnitialiseerd")]
    DuplicateFieldFirstUse { name: BabString },

    #[error("veld `{name}` is hier gedefinieerd")]
    FieldDefinedHere { name: BabString },

    #[error("werkwijze `{name}` is hier gedefinieerd")]
    FunctionDefinedHere { name: BabString },

    #[error("parameter `{name}` is hier gedeclareerd")]
    ParameterDeclaredHere { name: BabString },

    #[error("structuur `{name}` is hier gedefinieerd")]
    StructureDefinedHere { name: BabString },

    #[error("werkwijze zit niet in een structuur")]
    WerkwijzeNotInsideStructuur,

    #[error("los gebruik van het veld `{name}` heeft geen effect")]
    UsageOfPureValueField { name: BabString },
}

#[derive(Debug, Clone, Error, AsRefStr)]
pub enum SemanticDiagnosticKind {
    #[error("Expressie is geen geldige toewijzing: `{expression:?}`")]
    ExpressionCannotBeUsedAsAssignmentDestination {
        expression: Expression,
    },

    #[error("Werkwijze `{name}` bestaat niet.")]
    InvalidFunctionReference { name: BabString },

    #[error("Kon waarde `{identifier}` niet vinden binnen deze scoop.")]
    InvalidIdentifierReference { identifier: BabString },

    #[error("Te weinig argumenten gegeven aan werkwijze `{function_name}` ({arg_count} gegeven maar {param_count} verwacht)")]
    TooFewArguments {
        function_name: BabString,
        param_count: usize,
        arg_count: usize,
    },

    #[error("Te veel argumenten gegeven aan werkwijze `{function_name}` ({arg_count} gegeven maar {param_count} verwacht)")]
    TooManyArguments {
        function_name: BabString,
        param_count: usize,
        arg_count: usize,
    },

    #[error("Type `{name}` is onbekend")]
    UnknownType {
        name: BabString,
    },

    #[error("Types `{lhs_type}` en `{rhs_type}` zijn niet gelijksoortig.")]
    IncompatibleTypes {
        lhs_type: SemanticType,
        rhs_type: SemanticType,
    },

    #[error("Ongeldig argument gegeven voor werkwijze: argument van type `{argument_type}` is niet gelijksoortig met parameter van type `{parameter_type}`.")]
    IncompatibleArgumentParameterType {
        argument_type: SemanticType,
        parameter_type: SemanticType,
    },

    #[error("Lid `{name}` bestaat niet binnen type `{typ}`")]
    InvalidMember {
        typ: SemanticType,
        name: BabString,
    },

    #[error("Methode `{name}` bestaat niet binnen type `{typ}`")]
    InvalidMethod {
        typ: SemanticType,
        name: BabString,
    },

    #[error("Type `{typ}` is een werkwijze, en kan geen methodes bevatten.")]
    FunctionCannotHaveMethod {
        typ: SemanticType,
        name: BabString,
    },

    #[error("Attribuut `{name}` is onbekend")]
    UnknownAttribute { name: BabString, },

    #[error("Veldnaam `{name}` wordt meerdere keren gebruikt")]
    DuplicateFieldName { name: BabString },

    #[error("Werkwijzenaam `{name}` in structuur `{structure}` wordt meerdere keren gebruikt")]
    DuplicateMethodName { name: BabString, structure: BabString },

    #[error("Veld met naam `{name}` wordt meerdere keren een waarde toegekend")]
    DuplicateFieldInstantiation { name: BabString },

    #[error("Structuur `{struct_name}` heeft geen veld genaamd `{field_name}`")]
    InvalidFieldInstantiation {
        struct_name: BabString,
        field_name: BabString,
    },

    #[error("Ongeldige waarde gegeven voor veld `{field_name}` in structuur `{struct_name}`. Veldtype `{declaration_type}` is niet gelijksoortig met definitie van `{definition_type}`.")]
    IncompatibleFieldTypes {
        struct_name: BabString,
        field_name: BabString,
        declaration_type: String,
        definition_type: String,
    },

    #[error("Pure waarde ongebruikt. Stelling heeft geen gevolg.")]
    UnusedPureValue,

    #[error("`dit` kan uitsluitend gebruikt worden binnen een `structuur`")]
    ThisOutsideStructure,

    #[error("{field_word} `{names}` ontbreken een toewijzing")]
    MissingFieldInitializers {
        names: String,
        field_word: &'static str,
    },
}

impl SemanticDiagnosticKind {
    pub fn name(&self) -> &str {
        self.as_ref()
    }
}

#[derive(Debug)]
pub struct SemanticContext {
    scope: Vec<SemanticScope>,
    pub previous_scopes: Vec<SemanticScope>,

    pub definition_tracker: Option<HashMap<FileRange, SemanticReference>>,
    pub declaration_tracker: Option<Vec<SemanticReference>>,
}

impl SemanticContext {
    pub fn new() -> Self {
        Self {
            scope: vec![
                SemanticScope::new_top_level(),
            ],
            previous_scopes: Vec::new(),
            definition_tracker: Some(HashMap::new()),
            declaration_tracker: Some(Vec::new()),
        }
    }

    pub fn push_function_scope(&mut self, start: FileLocation, this: Option<SemanticType>) -> &mut SemanticScope {
        self.scope.push(SemanticScope {
            range: FileRange::new(start, start),
            locals: HashMap::new(),
            structures: HashMap::new(),
            this,
        });
        self.scope.last_mut().expect("we just pushed a scope")
    }

    pub fn push_block_scope(&mut self, start: FileLocation) -> &mut SemanticScope {
        let this = self.scope.last().and_then(|x| x.this.clone());
        self.scope.push(SemanticScope {
            range: FileRange::new(start, start),
            locals: HashMap::new(),
            structures: HashMap::new(),
            this,
        });
        self.scope.last_mut().expect("we just pushed a scope")
    }

    fn push_function(&mut self, function: SemanticFunction) {
        if let Some(tracker) = &mut self.declaration_tracker {
            tracker.push(SemanticReference {
                local_name: function.name.value().clone(),
                local_kind: SemanticLocalKind::Function,
                declaration_range: function.name.range(),
                typ: SemanticType::Function(function.clone()),
            });
        }

        self.scope.last_mut().unwrap().locals.insert(
            function.name.value().clone(),
            SemanticLocal {
                kind: SemanticLocalKind::Function,
                declaration_range: function.name.range(),
                typ: SemanticType::Function(function),
            }
        );
    }

    fn push_structure(&mut self, structure: Rc<SemanticStructure>) {
        if let Some(tracker) = &mut self.declaration_tracker {
            tracker.push(SemanticReference {
                local_name: structure.name.value().clone(),
                local_kind: SemanticLocalKind::StructureReference,
                declaration_range: structure.name.range(),
                typ: SemanticType::Custom(Rc::clone(&structure)),
            });

            for method in &structure.methods {
                tracker.push(SemanticReference {
                    local_name: method.name().clone(),
                    local_kind: SemanticLocalKind::Method,
                    declaration_range: method.function.name.range(),
                    typ: SemanticType::FunctionReference(FunctionReference::Custom(method.function.clone())), // is this okay?
                });
            }

            for field in &structure.fields {
                tracker.push(SemanticReference {
                    local_name: field.name.value().clone(),
                    local_kind: SemanticLocalKind::FieldReference,
                    declaration_range: field.name.range(),
                    typ: field.ty.clone(),
                });
            }
        }

        self.scope.last_mut().unwrap().structures.insert(structure.name.value().clone(), structure);
    }

    fn pop_scope(&mut self, location: FileLocation) {
        debug_assert!(self.scope.len() > 1);
        let mut scope = self.scope.pop().unwrap();

        scope.range = FileRange::new(scope.range.start(), location);

        self.previous_scopes.push(scope);
    }

    fn push_local(&mut self, name: &Ranged<BabString>, local: SemanticLocal) {
        if let Some(tracker) = &mut self.declaration_tracker {
            tracker.push(SemanticReference {
                local_name: name.value().clone(),
                local_kind: local.kind,
                declaration_range: name.range(),
                typ: local.typ.clone(),
            });
        }

        self.scope.last_mut().as_mut().unwrap().locals.insert(name.value().clone(), local);
    }
}

#[derive(Debug)]
pub struct SemanticScope {
    range: FileRange,
    pub locals: HashMap<BabString, SemanticLocal>,
    pub structures: HashMap<BabString, Rc<SemanticStructure>>,
    pub this: Option<SemanticType>,
}

impl SemanticScope {
    fn new_top_level() -> Self {
        let mut this = Self {
            range: FileRange::new(FileLocation::default(), FileLocation::new(usize::MAX, usize::MAX, usize::MAX)),
            structures: HashMap::new(),
            locals: HashMap::default(),
            this: None,
        };

        for func in Builtin::FUNCTIONS {
            this.locals.insert(BabString::new_static(func.name), SemanticLocal {
                kind: SemanticLocalKind::Function,
                declaration_range: FileRange::default(),
                typ: SemanticType::FunctionReference(FunctionReference::Builtin(func)),
            });
        }

        this
    }
}

#[derive(Debug, Clone)]
pub struct SemanticFunction {
    pub name: Ranged<BabString>,
    pub parameters: Vec<Parameter>,
    // type ...
}

impl Display for SemanticFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("werkwijze ")?;
        f.write_str(&self.name)?;
        f.write_str("()")
    }
}

impl PartialEq for SemanticFunction {
    fn eq(&self, other: &Self) -> bool {
        self.name.value() == other.name.value()
    }
}

#[derive(Debug, Clone)]
pub struct SemanticLocal {
    pub kind: SemanticLocalKind,
    pub declaration_range: FileRange,
    pub typ: SemanticType,
    // type ...
}

#[derive(Debug, Clone, PartialEq)]
pub struct SemanticReference {
    pub local_name: BabString,
    pub local_kind: SemanticLocalKind,
    pub declaration_range: FileRange,
    pub typ: SemanticType,
}

impl SemanticReference {
    pub fn function_name(&self) -> BabString {
        match &self.typ {
            SemanticType::Builtin(..) => todo!(),
            SemanticType::Custom(..) => todo!(),
            SemanticType::Function(func) => BabString::clone(&func.name),
            SemanticType::FunctionReference(func) => func.name(),
        }
    }

    pub fn documentation(&self) -> Option<BabString> {
        match &self.typ {
            SemanticType::Builtin(builtin) => Some(builtin.documentation().into_bab_string()),
            SemanticType::Custom(..) => None,
            SemanticType::Function(..) => None,
            SemanticType::FunctionReference(func) => func.documentation(),
        }
    }

    pub fn inline_detail(&self) -> Option<BabString> {
        match &self.typ {
            SemanticType::Builtin(builtin) => Some(builtin.inline_detail()),
            SemanticType::Custom(..) => None,
            SemanticType::Function(..) => None,
            SemanticType::FunctionReference(func) => func.inline_detail(),
        }
    }

    pub fn lsp_completion(&self) -> BabString {
        match &self.typ {
            SemanticType::Builtin(builtin) => builtin.name(),
            SemanticType::Custom(custom) => BabString::clone(&custom.name),
            SemanticType::Function(func) => BabString::new(format!("{}($1);$0", func.name.value())),
            SemanticType::FunctionReference(func) => func.lsp_completion(),
        }
    }

    pub fn hover(&self) -> String {
        match self.local_kind {
            SemanticLocalKind::Function | SemanticLocalKind::FunctionReference => {
                format!("```babbelaar\nwerkwijze {}(..)\n```", self.local_name)
            }

            SemanticLocalKind::FieldReference => {
                format!("```babbelaar\nveld {}: {}\n```", self.local_name, self.typ)
            }

            SemanticLocalKind::StructureReference => {
                let mut fields = String::new();

                if let SemanticType::Custom(typ) = &self.typ {
                    for field in &typ.fields {
                        fields += &format!("\n    veld {}: {}", field.name.value(), field.ty);
                    }
                }

                format!("```babbelaar\nstructuur {} {{{fields}\n}}\n```", self.local_name)
            }

            SemanticLocalKind::Variable => {
                format!("```babbelaar\nstel {}: {}\n```", self.local_name, self.typ)
            }

            _ => {
                format!("```babbelaar\n{}: {}\n```", self.local_name, self.typ)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct SemanticField {
    pub attributes: Vec<Attribute>,
    pub name: Ranged<BabString>,
    pub ty: SemanticType,
}

#[derive(Debug, Clone)]
pub struct SemanticMethod {
    pub function: SemanticFunction,
}

impl SemanticMethod {
    #[must_use]
    fn name(&self) -> &BabString {
        &self.function.name
    }

    #[must_use]
    fn return_type(&self) -> SemanticType {
        SemanticType::null()
    }

    #[must_use]
    fn return_type_usage(&self) -> SemanticUsage {
        SemanticUsage::Indifferent
    }
}

#[derive(Debug, Clone)]
pub struct SemanticStructure {
    pub attributes: Vec<Attribute>,
    pub name: Ranged<BabString>,
    pub left_curly_range: FileRange,
    pub fields: Vec<SemanticField>,
    pub methods: Vec<SemanticMethod>,
}

impl Display for SemanticStructure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)
    }
}

impl PartialEq for SemanticStructure {
    fn eq(&self, other: &Self) -> bool {
        self.name.range() == other.name.range() && self.name.value() == other.name.value()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SemanticType {
    Builtin(BuiltinType),
    Custom(Rc<SemanticStructure>),
    Function(SemanticFunction),
    FunctionReference(FunctionReference),
}

impl SemanticType {
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

    /// A hint to a name that could be used as the name for a field or value.
    #[must_use]
    pub fn value_or_field_name_hint(&self) -> BabString {
        match self {
            Self::Builtin(BuiltinType::Slinger) => BabString::new_static("tekst"),
            Self::Builtin(BuiltinType::G32) => BabString::new_static("getal"),
            Self::Builtin(builtin) => builtin.name(),
            Self::Custom(custom) => custom.name.value().clone(),

            Self::Function(..) => BabString::empty(),
            Self::FunctionReference(..) => BabString::empty(),
        }
    }

    /// A hint to a value that could be used as the default value.
    #[must_use]
    pub fn default_value_hint(&self) -> &str {
        match self {
            Self::Builtin(BuiltinType::Slinger) => "\"\"",
            Self::Builtin(BuiltinType::G32) => "0",
            Self::Builtin(BuiltinType::Bool) => "onwaar",

            _ => "",
        }
    }

    #[must_use]
    pub fn name(&self) -> BabString {
        match self {
            Self::Builtin(builtin) => builtin.name().into(),
            Self::Custom(custom) => custom.name.value().clone(),
            Self::Function(func) => func.name.value().clone(),
            Self::FunctionReference(func) => func.name(),
        }
    }
}

impl Display for SemanticType {
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
    FieldReference,
    StructureReference,
    Iterator,
    Function,
    FunctionReference,
    Variable,
    Method,
    ReferenceThis,
}

impl SemanticLocalKind {
    #[must_use]
    pub const fn is_function(&self) -> bool {
        match self {
            Self::Parameter => false,
            Self::FieldReference => false,
            Self::StructureReference => true,
            Self::Iterator => false,
            Self::Function => true,
            Self::FunctionReference => true,
            Self::Variable => false,
            Self::Method => true,
            Self::ReferenceThis => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionReference {
    Builtin(&'static BuiltinFunction),
    Custom(SemanticFunction),
}

impl Display for FunctionReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Builtin(func) => func.fmt(f),
            Self::Custom(func) => func.fmt(f),
        }
    }
}

impl FunctionReference {
    #[must_use]
    pub fn parameter_count(&self) -> usize {
        match self {
            Self::Builtin(func) => func.parameters.len(),
            Self::Custom(func) => func.parameters.len(),
        }
    }

    #[must_use]
    pub fn name(&self) -> BabString {
        match self {
            Self::Builtin(func) => BabString::new_static(func.name),
            Self::Custom(func) => func.name.value().to_owned(),
        }
    }

    #[must_use]
    pub fn documentation(&self) -> Option<BabString> {
        match self {
            Self::Builtin(func) => Some(BabString::new_static(func.documentation)),
            Self::Custom(..) => None,
        }
    }

    #[must_use]
    pub fn lsp_completion(&self) -> BabString {
        if let Some(completion) = self.lsp_completion_raw() {
            return completion.into();
        }

        let mut insert = format!("{}(", self.name());

        for i in 0..self.parameter_count() {
            let comma = if i == 0 { "" } else { ", " };
            insert += &format!("{comma}${}", i + 1);
        }

        insert += ");$0";
        insert.into()
    }

    #[must_use]
    fn lsp_completion_raw(&self) -> Option<BabString> {
        match self {
            Self::Builtin(func) => func.lsp_completion.map(|x| BabString::new_static(x)),
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
    fn inline_detail(&self) -> Option<BabString> {
        match self {
            Self::Builtin(func) => Some(BabString::new_static(func.inline_detail)),
            Self::Custom(..) => None,
        }
    }
}

#[derive(Debug)]
pub struct SemanticValue {
    ty: SemanticType,
    usage: SemanticUsage,
}

impl SemanticValue {
    #[must_use]
    fn null() -> Self {
        Self {
            ty: SemanticType::null(),
            usage: SemanticUsage::Indifferent,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SemanticUsage {
    Indifferent,
    Pure(PureValue),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PureValue {
    ConstantValue,
    FieldReference {
        declaration: FileRange,
        name: BabString,
    },
    Operator {
        operator_range: FileRange,
    },
    ReturnValue,
}
