// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{borrow::Cow, collections::{HashMap, HashSet}, fmt::Display, rc::Rc};

use log::warn;
use strum::AsRefStr;
use thiserror::Error;

use crate::{statement::VariableStatement, Attribute, BabbelaarCodeAction, BabbelaarCodeActionType, BiExpression, Builtin, BuiltinFunction, BuiltinType, Expression, FileEdit, FileLocation, FileRange, ForStatement, FunctionCallExpression, FunctionStatement, IfStatement, Keyword, MethodCallExpression, OptionExt, Parameter, ParseTree, PostfixExpression, PostfixExpressionKind, PrimaryExpression, Ranged, ReturnStatement, Statement, StatementKind, StrExt, StrIterExt, Structure, StructureInstantiationExpression, TemplateStringExpressionPart, Type, TypeSpecifier};

#[derive(Debug)]
pub struct SemanticAnalyzer<'source_code> {
    pub context: SemanticContext<'source_code>,
    diagnostics: Vec<SemanticDiagnostic<'source_code>>,
    source_code: &'source_code str,
}

impl<'source_code> SemanticAnalyzer<'source_code> {
    #[must_use]
    pub fn new(source_code: &'source_code str,) -> Self {
        Self {
            context: SemanticContext::new(),
            diagnostics: Vec::new(),
            source_code,
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

    fn analyze_expression(&mut self, expression: &'source_code Ranged<Expression<'source_code>>) -> SemanticValue<'source_code> {
        match expression.value() {
            Expression::BiExpression(bi) => self.analyze_bi_expression(bi),
            Expression::Postfix(postfix) => self.analyze_postfix_expression(postfix),
            Expression::Primary(primary) => self.analyze_primary_expression(primary, expression.range()),
        }
    }

    fn analyze_function(&mut self, function: &'source_code FunctionStatement<'source_code>, this: Option<SemanticType<'source_code>>) {
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

    fn analyze_assignment_destination(&mut self, range: FileRange, expression: &'source_code Expression<'source_code>) {
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
                expression,
            }
        ));
    }

    fn analyze_for_statement(&mut self, statement: &'source_code ForStatement<'source_code>) {
        let scope = self.context.push_block_scope(statement.keyword.start());
        scope.locals.insert(&statement.iterator_name, SemanticLocal {
            kind: SemanticLocalKind::Iterator,
            declaration_range: statement.iterator_name.range(),
            typ: SemanticType::Builtin(BuiltinType::G32),
        });

        self.analyze_statements(&statement.body);

        self.context.pop_scope(statement.file_range.end());
    }

    fn analyze_if_statement(&mut self, statement: &'source_code IfStatement<'source_code>) {
        self.context.push_block_scope(statement.condition.range().start());

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

        let methods = structure.methods.iter().map(|x| SemanticMethod {
            function: SemanticFunction {
                name: x.function.name,
                parameters: &x.function.parameters,
            }
        }).collect();

        let semantic_structure = Rc::new(SemanticStructure {
            attributes: &statement.attributes,
            name: structure.name,
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
                    SemanticDiagnosticKind::DuplicateFieldName { name: field.name.value() },
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
                    SemanticDiagnosticKind::DuplicateMethodName { name: method.function.name.value(), structure: &structure.name },
                ));
            }

            self.analyze_function(&method.function, this_type.clone());
        }
    }

    fn analyze_variable_statement(&mut self, statement: &'source_code VariableStatement<'source_code>) {
        let typ = self.analyze_expression(&statement.expression).ty;
        self.context.scope.last_mut().unwrap().locals.insert(&statement.name, SemanticLocal {
            kind: SemanticLocalKind::Variable,
            declaration_range: statement.name.range(),
            typ,
        });
    }

    fn analyze_bi_expression(&mut self, expression: &'source_code BiExpression<'source_code>) -> SemanticValue<'source_code> {
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

    fn analyze_function_call_expression(&mut self, lhs: SemanticType<'source_code>, expression: &'source_code FunctionCallExpression<'source_code>) -> SemanticValue<'source_code> {
        let function_name = match lhs {
            SemanticType::Builtin(builtin) => builtin.name(),
            SemanticType::Custom(custom) => &custom.name,
            SemanticType::Function(func) => func.name.value(),
            SemanticType::FunctionReference(func) => func.name(),
        };

        let Some(function) = self.find_function(&function_name) else {
            self.diagnostics.push(SemanticDiagnostic::new(
                expression.token_left_paren,
                SemanticDiagnosticKind::InvalidFunctionReference { name: &function_name }
            ));
            return SemanticValue::null();
        };


        self.analyze_function_parameters(&function_name, function, expression);

        SemanticValue::null()
    }

    fn analyze_function_parameters(
        &mut self,
        function_name: &'source_code str,
        function: SemanticReference<'source_code>,
        expression: &'source_code FunctionCallExpression<'source_code>,
    ) {
        let function_hint = SemanticRelatedInformation::new(
            function.declaration_range,
            SemanticRelatedMessage::FunctionDefinedHere { name: function_name }
        );

        let param_count = function.typ.parameter_count().expect("This reference to be a function, verified by self.find_function()");
        let arg_count = expression.arguments.len();

        if param_count > arg_count {
            self.diagnostics.push(SemanticDiagnostic::new(
                expression.token_right_paren,
                SemanticDiagnosticKind::TooFewArguments { function_name, param_count, arg_count },
            ).with_related(function_hint.clone()));
        }

        if param_count < arg_count {
            self.diagnostics.push(SemanticDiagnostic::new(
                expression.token_right_paren,
                SemanticDiagnosticKind::TooManyArguments { function_name, param_count, arg_count },
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
                        SemanticRelatedMessage::ParameterDeclaredHere { name: x.value() }
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

    fn analyze_primary_expression(&mut self, expression: &'source_code PrimaryExpression<'source_code>, range: FileRange) -> SemanticValue<'source_code> {
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
                            local_name: Keyword::Dit.as_ref(),
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
                let Some(local) = self.find_local_by_name(|name| &name == reference.value()) else {
                    self.diagnostics.push(SemanticDiagnostic::new(
                        reference.range(),
                        SemanticDiagnosticKind::InvalidIdentifierReference { identifier: &reference }
                    ));
                    return SemanticValue::null();
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

            PrimaryExpression::Parenthesized(expr) => return self.analyze_expression(expr),
        };

        SemanticValue {
            ty,
            usage: SemanticUsage::Pure(PureValue::ConstantValue),
        }
    }

    fn analyze_structure_instantiation(&mut self, instantiation: &'source_code StructureInstantiationExpression<'source_code>) -> SemanticValue<'source_code> {
        let ty = self.resolve_type_by_name(&instantiation.name);
        let SemanticType::Custom(structure) = &ty else {
            return SemanticValue::null();
        };

        let struct_hint = SemanticRelatedInformation::new(
            structure.name.range(),
            SemanticRelatedMessage::StructureDefinedHere { name: &structure.name }
        );

        let all_valid_fields: HashMap<&str, &SemanticField<'source_code>> = structure.fields.iter().map(|x| (*x.name.value(), x)).collect();
        let mut fields_left = all_valid_fields.clone();

        if let Some(tracker) = &mut self.context.definition_tracker {
            tracker.insert(instantiation.name.range(), SemanticReference {
                local_name: &instantiation.name,
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
                                struct_name: structure.name.value(),
                                field_name: name.value(),
                                declaration_type: declaration_type.to_string(),
                                definition_type: definition_type.to_string(),
                            },
                        ).with_related(struct_hint.clone()).with_action(action));
                    }

                    if let Some(tracker) = &mut self.context.definition_tracker {
                        tracker.insert(field_instantiation.name.range(), SemanticReference {
                            local_name: &field.name,
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
                            SemanticRelatedMessage::DuplicateFieldFirstUse { name: &duplicate_field.name }
                        );

                        let diag = SemanticDiagnostic::new(
                            name.range(),
                            SemanticDiagnosticKind::DuplicateFieldInstantiation { name: name.value() },
                        );

                        self.diagnostics.push(diag.with_related(field_def_hint).with_related(struct_hint.clone()));
                    } else {
                        let definition_type = self.analyze_expression(&field_instantiation.value).ty;
                        let create_field_action = self.create_action_create_field(&structure, &field_instantiation.name, definition_type);

                        self.diagnostics.push(
                            SemanticDiagnostic::new(
                                name.range(),
                                SemanticDiagnosticKind::InvalidFieldInstantiation {
                                    struct_name: structure.name.value(),
                                    field_name: name.value()
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
            let names = fields_left.keys().map(|x| *x).join("`, `");
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

    fn find_local_by_name<'this, P>(&'this self, predicate: P) -> Option<&'this SemanticLocal<'source_code>>
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

    #[must_use]
    pub fn find_declaration_range_at(&self, location: FileLocation) -> Option<FileRange> {
        for (range, reference) in self.context.definition_tracker.as_ref()? {
            if range.contains(location) {
                return Some(*range);
            }

            if reference.declaration_range.contains(location) {
                return Some(reference.declaration_range);
            }
        }

        None
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

        self.diagnostics.push(SemanticDiagnostic::new(
            name.range(),
            SemanticDiagnosticKind::UnknownType { name: &name },
        ));

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

    fn resolve_parameter_name<'this>(&'this mut self, function: &SemanticReference<'source_code>, arg_idx: usize) -> Option<Ranged<&'source_code str>> {
        match &function.typ {
            SemanticType::Builtin(..) => todo!(),
            SemanticType::Custom(..) => todo!(),
            SemanticType::Function(func) => {
                Some(Ranged::new(func.parameters[arg_idx].name.range(), &func.parameters[arg_idx].name))
            }
            SemanticType::FunctionReference(FunctionReference::Builtin(..)) => {
                None
            }
            SemanticType::FunctionReference(FunctionReference::Custom(func)) => {
                Some(func.name)
            }
        }
    }

    fn resolve_parameter_type<'this>(&'this mut self, function: &SemanticReference<'source_code>, arg_idx: usize) -> Option<SemanticType<'source_code>> {
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

    fn analyze_postfix_expression(&mut self, postfix: &'source_code PostfixExpression<'source_code>) -> SemanticValue<'source_code> {
        let lhs = self.analyze_expression(&postfix.lhs).ty;
        match &postfix.kind {
            PostfixExpressionKind::Call(call) => self.analyze_function_call_expression(lhs, call),
            PostfixExpressionKind::Member(member) => self.analyze_member_expression(lhs, *member),
            PostfixExpressionKind::MethodCall(method) => self.analyze_method_expression(lhs, method),
        }
    }

    fn analyze_member_expression(&mut self, typ: SemanticType<'source_code>, member: Ranged<&'source_code str>) -> SemanticValue<'source_code> {
        let SemanticType::Custom(structure) = &typ else {
            self.diagnostics.push(SemanticDiagnostic::new(
                member.range(),
                SemanticDiagnosticKind::InvalidMember { typ, name: member.value() }
            ));

            return SemanticValue::null()
        };

        for field in &structure.fields {
            if field.name.value() == member.value() {
                if let Some(tracker) = &mut self.context.definition_tracker {
                    tracker.insert(member.range(), SemanticReference {
                        local_name: &member,
                        local_kind: SemanticLocalKind::FieldReference,
                        declaration_range: field.name.range(),
                        typ: field.ty.clone(),
                    });
                }

                return SemanticValue {
                    ty: field.ty.clone(),
                    usage: SemanticUsage::Pure(PureValue::FieldReference {
                        declaration: field.name.range(),
                        name: field.name.value(),
                    }),
                };
            }
        }

        let struct_hint = SemanticRelatedInformation::new(
            structure.name.range(),
            SemanticRelatedMessage::StructureDefinedHere { name: &structure.name }
        );

        let diag = SemanticDiagnostic::new(
            member.range(),
            SemanticDiagnosticKind::InvalidMember { typ, name: member.value() }
        );

        self.diagnostics.push(diag.with_related(struct_hint));

        SemanticValue::null()
    }

    fn analyze_method_expression(&mut self, typ: SemanticType<'source_code>, expression: &'source_code MethodCallExpression<'source_code>) -> SemanticValue<'source_code> {
        match typ {
            SemanticType::Builtin(builtin) => {
                for method in builtin.methods() {
                    if method.name == *expression.method_name {
                        return SemanticValue {
                            ty: SemanticType::Builtin(method.return_type),
                            usage: if method.must_use { SemanticUsage::Pure(PureValue::ReturnValue) } else { SemanticUsage::Indifferent },
                        };
                    }
                }

                self.diagnostics.push(SemanticDiagnostic::new(
                    expression.method_name.range(),
                    SemanticDiagnosticKind::InvalidMethod { typ, name: expression.method_name.value() }
                ));

                SemanticValue::null()
            }

            SemanticType::Custom(ref custom) => {
                for method in &custom.methods {
                    if method.name() == *expression.method_name {
                        let local_reference = SemanticReference {
                            local_name: method.name(),
                            local_kind: SemanticLocalKind::Method,
                            declaration_range: method.function.name.range(),
                            typ: SemanticType::FunctionReference(FunctionReference::Custom(method.function)), // is this okay?
                        };

                        if let Some(tracker) = &mut self.context.definition_tracker {
                            tracker.insert(expression.method_name.range(), local_reference.clone());
                        }

                        self.analyze_function_parameters(method.name(), local_reference, &expression.call);

                        return SemanticValue {
                            ty: method.return_type(),
                            usage: method.return_type_usage(),
                        };
                    }
                }

                let struct_hint = SemanticRelatedInformation::new(
                    custom.name.range(),
                    SemanticRelatedMessage::StructureDefinedHere { name: &custom.name }
                );

                let diag = SemanticDiagnostic::new(
                    expression.method_name.range(),
                    SemanticDiagnosticKind::InvalidMethod { typ, name: expression.method_name.value() }
                );

                self.diagnostics.push(diag.with_related(struct_hint));

                SemanticValue::null()
            }

            SemanticType::Function(..) | SemanticType::FunctionReference(..) => {
                self.diagnostics.push(SemanticDiagnostic::new(
                    expression.method_name.range(),
                    SemanticDiagnosticKind::FunctionCannotHaveMethod { typ, name: expression.method_name.value() }
                ));

                // default SemanticUsage of functions should be default-must_use
                SemanticValue::null()
            }
        }
    }

    fn analyze_attributes_for_statement(&mut self, statement: &Statement<'source_code>) {
        for attribute in &statement.attributes {
            self.diagnostics.push(SemanticDiagnostic::new(
                attribute.name.range(),
                SemanticDiagnosticKind::UnknownAttribute { name: &attribute.name },
            ));
        }
    }

    fn analyze_attributes_for_field(&mut self, field: &SemanticField<'source_code>) {
        for attribute in field.attributes {
            self.diagnostics.push(SemanticDiagnostic::new(
                attribute.name.range(),
                SemanticDiagnosticKind::UnknownAttribute { name: &attribute.name },
            ));
        }
    }

    fn try_create_conversion_action(
        &self,
        declaration_type: &SemanticType<'source_code>,
        definition_type: &SemanticType<'source_code>,
        field_instantiation: &crate::FieldInstantiation<'source_code>,
    ) -> Option<BabbelaarCodeAction> {
        let SemanticType::Builtin(declaration_type) = declaration_type else { return None };
        let SemanticType::Builtin(definition_type) = definition_type else { return None };

        if *declaration_type == BuiltinType::G32 && *definition_type == BuiltinType::Slinger {
            if let Expression::Primary(PrimaryExpression::StringLiteral(literal)) = field_instantiation.value.value() {
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
    fn create_action_create_field(&self, structure: &SemanticStructure<'_>, name: &str, ty: SemanticType<'_>) -> BabbelaarCodeAction {
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
pub struct SemanticDiagnostic<'source_code> {
    range: FileRange,
    kind: SemanticDiagnosticKind<'source_code>,
    severity: SemanticDiagnosticSeverity,
    related: Vec<SemanticRelatedInformation<'source_code>>,
    actions: Vec<BabbelaarCodeAction>,
}

impl<'source_code> SemanticDiagnostic<'source_code> {
    #[must_use]
    pub fn new(range: FileRange, kind: SemanticDiagnosticKind<'source_code>) -> Self {
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
    pub fn kind(&self) -> &SemanticDiagnosticKind<'source_code> {
        &self.kind
    }

    #[must_use]
    pub fn related_info(&self) -> &[SemanticRelatedInformation<'source_code>] {
        &self.related
    }

    #[must_use]
    fn with_related(mut self, info: impl Into<Option<SemanticRelatedInformation<'source_code>>>) -> Self {
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
pub struct SemanticRelatedInformation<'source_code> {
    range: FileRange,
    message: SemanticRelatedMessage<'source_code>,
}

impl<'source_code> SemanticRelatedInformation<'source_code> {
    #[must_use]
    pub fn new(range: FileRange, message: SemanticRelatedMessage<'source_code>) -> Self {
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
    pub fn message(&self) -> &SemanticRelatedMessage<'source_code> {
        &self.message
    }
}

#[derive(Debug, Clone, Error, AsRefStr)]
pub enum SemanticRelatedMessage<'source_code> {
    #[error("`{name}` is hier voor het eerst geïnitialiseerd")]
    DuplicateFieldFirstUse { name: &'source_code str },

    #[error("veld `{name}` is hier gedefinieerd")]
    FieldDefinedHere { name: &'source_code str },

    #[error("werkwijze `{name}` is hier gedefinieerd")]
    FunctionDefinedHere { name: &'source_code str },

    #[error("parameter `{name}` is hier gedeclareerd")]
    ParameterDeclaredHere { name: &'source_code str },

    #[error("structuur `{name}` is hier gedefinieerd")]
    StructureDefinedHere { name: &'source_code str },

    #[error("werkwijze zit niet in een structuur")]
    WerkwijzeNotInsideStructuur,

    #[error("los gebruik van het veld `{name}` heeft geen effect")]
    UsageOfPureValueField { name: &'source_code str },
}

#[derive(Debug, Clone, Error, AsRefStr)]
pub enum SemanticDiagnosticKind<'source_code> {
    #[error("Expressie is geen geldige toewijzing: `{expression:?}`")]
    ExpressionCannotBeUsedAsAssignmentDestination {
        expression: &'source_code Expression<'source_code>
    },

    #[error("Werkwijze `{name}` bestaat niet.")]
    InvalidFunctionReference { name: &'source_code str },

    #[error("Kon waarde `{identifier}` niet vinden binnen deze scoop.")]
    InvalidIdentifierReference { identifier: &'source_code str },

    #[error("Te weinig argumenten gegeven aan werkwijze `{function_name}` ({arg_count} gegeven maar {param_count} verwacht)")]
    TooFewArguments {
        function_name: &'source_code str,
        param_count: usize,
        arg_count: usize,
    },

    #[error("Te veel argumenten gegeven aan werkwijze `{function_name}` ({arg_count} gegeven maar {param_count} verwacht)")]
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

    #[error("Ongeldig argument gegeven voor werkwijze: argument van type `{argument_type}` is niet gelijksoortig met parameter van type `{parameter_type}`.")]
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

    #[error("Type `{typ}` is een werkwijze, en kan geen methodes bevatten.")]
    FunctionCannotHaveMethod {
        typ: SemanticType<'source_code>,
        name: &'source_code str,
    },

    #[error("Attribuut `{name}` is onbekend")]
    UnknownAttribute { name: &'source_code str, },

    #[error("Veldnaam `{name}` wordt meerdere keren gebruikt")]
    DuplicateFieldName { name: &'source_code str },

    #[error("Werkwijzenaam `{name}` in structuur `{structure}` wordt meerdere keren gebruikt")]
    DuplicateMethodName { name: &'source_code str, structure: &'source_code str },

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

    pub fn push_function_scope(&mut self, start: FileLocation, this: Option<SemanticType<'source_code>>) -> &mut SemanticScope<'source_code> {
        self.scope.push(SemanticScope {
            range: FileRange::new(start, start),
            locals: HashMap::new(),
            structures: HashMap::new(),
            this,
        });
        self.scope.last_mut().expect("we just pushed a scope")
    }

    pub fn push_block_scope(&mut self, start: FileLocation) -> &mut SemanticScope<'source_code> {
        let this = self.scope.last().and_then(|x| x.this.clone());
        self.scope.push(SemanticScope {
            range: FileRange::new(start, start),
            locals: HashMap::new(),
            structures: HashMap::new(),
            this,
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
        debug_assert!(self.scope.len() > 1);
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
    pub this: Option<SemanticType<'source_code>>,
}

impl<'source_code> SemanticScope<'source_code> {
    fn new_top_level() -> Self {
        let mut this = Self {
            range: FileRange::new(FileLocation::default(), FileLocation::new(usize::MAX, usize::MAX, usize::MAX)),
            structures: HashMap::new(),
            locals: HashMap::default(),
            this: None,
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
    pub name: Ranged<&'source_code str>,
    pub parameters: &'source_code [Parameter<'source_code>],
    // type ...
}

impl<'source_code> Display for SemanticFunction<'source_code> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("werkwijze ")?;
        f.write_str(&self.name)?;
        f.write_str("()")
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

    pub fn documentation(&self) -> Option<Cow<'source_code, str>> {
        match &self.typ {
            SemanticType::Builtin(builtin) => Some(builtin.documentation().into()),
            SemanticType::Custom(..) => None,
            SemanticType::Function(..) => None,
            SemanticType::FunctionReference(func) => func.documentation().map(Cow::from),
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
pub struct SemanticField<'source_code> {
    pub attributes: &'source_code [Attribute<'source_code>],
    pub name: Ranged<&'source_code str>,
    pub ty: SemanticType<'source_code>,
}

#[derive(Debug, Clone)]
pub struct SemanticMethod<'source_code> {
    pub function: SemanticFunction<'source_code>,
}

impl<'source_code> SemanticMethod<'source_code> {
    #[must_use]
    fn name(&self) -> &'source_code str {
        &self.function.name
    }

    #[must_use]
    fn return_type(&self) -> SemanticType<'source_code> {
        SemanticType::null()
    }

    #[must_use]
    fn return_type_usage(&self) -> SemanticUsage<'source_code> {
        SemanticUsage::Indifferent
    }
}

#[derive(Debug, Clone)]
pub struct SemanticStructure<'source_code> {
    pub attributes: &'source_code [Attribute<'source_code>],
    pub name: Ranged<&'source_code str>,
    pub left_curly_range: FileRange,
    pub fields: Vec<SemanticField<'source_code>>,
    pub methods: Vec<SemanticMethod<'source_code>>,
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

    /// A hint to a name that could be used as the name for a field or value.
    #[must_use]
    pub fn value_or_field_name_hint(&self) -> &str {
        match self {
            Self::Builtin(BuiltinType::Slinger) => "tekst",
            Self::Builtin(BuiltinType::G32) => "getal",
            Self::Builtin(builtin) => builtin.name(),
            Self::Custom(custom) => &custom.name,

            Self::Function(..) => "",
            Self::FunctionReference(..) => "",
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
    pub fn name(&self) -> &str {
        match self {
            Self::Builtin(builtin) => builtin.name(),
            Self::Custom(custom) => custom.name.value(),
            Self::Function(func) => func.name.value(),
            Self::FunctionReference(func) => func.name(),
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

#[derive(Debug)]
pub struct SemanticValue<'source_code> {
    ty: SemanticType<'source_code>,
    usage: SemanticUsage<'source_code>,
}

impl<'source_code> SemanticValue<'source_code> {
    #[must_use]
    fn null() -> Self {
        Self {
            ty: SemanticType::null(),
            usage: SemanticUsage::Indifferent,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SemanticUsage<'source_code> {
    Indifferent,
    Pure(PureValue<'source_code>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PureValue<'source_code> {
    ConstantValue,
    FieldReference {
        declaration: FileRange,
        name: &'source_code str,
    },
    Operator {
        operator_range: FileRange,
    },
    ReturnValue,
}
