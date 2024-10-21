// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::{HashMap, HashSet}, fmt::{Display, Write}, sync::Arc};

use log::warn;
use strum::{AsRefStr, EnumIter, IntoEnumIterator};
use thiserror::Error;

use crate::*;

#[derive(Debug)]
pub struct SemanticAnalyzer {
    pub context: SemanticContext,
    files: HashMap<FileId, SourceCode>,
    diagnostics: SemanticDiagnosticsList,
    should_produce_diagnostics: bool,
}

impl SemanticAnalyzer {
    #[must_use]
    pub fn new(files: HashMap<FileId, SourceCode>, should_produce_diagnostics: bool) -> Self {
        Self {
            context: SemanticContext::new(),
            diagnostics: SemanticDiagnosticsList::new(should_produce_diagnostics),
            files,
            should_produce_diagnostics,
        }
    }

    #[must_use]
    pub fn new_single(source_code: &SourceCode, should_produce_diagnostics: bool) -> Self {
        let mut files = HashMap::new();
        files.insert(source_code.file_id(), source_code.clone());
        Self::new(files, should_produce_diagnostics)
    }

    pub fn analyze_tree(&mut self, tree: &ParseTree, phase: SemanticAnalysisPhase) {
        self.context.announce_file(tree);

        match phase {
            SemanticAnalysisPhase::Phase1 => {
                for statement in tree.structures() {
                    if let StatementKind::Structure(structure) = &statement.kind {
                        self.analyze_structure(statement, structure);
                    }
                }

                for statement in tree.interfaces() {
                    if let StatementKind::Interface(interface) = &statement.kind {
                        self.analyze_interface(statement, interface);
                    }
                }
            }

            SemanticAnalysisPhase::Phase2 => {
                self.analyze_statements(tree.extensions());
            }

            SemanticAnalysisPhase::Phase3 => {
                for statement in tree.functions() {
                    if let StatementKind::Function(function) = &statement.kind {
                        self.analyze_function_declaration(function, statement.range);
                    }
                }
            }

            SemanticAnalysisPhase::Phase4 => {
                for statement in tree.all() {
                    self.analyze_statement(statement);
                }
            }
        }
    }

    pub fn finish_analysis(&mut self) {
        if self.should_produce_diagnostics {
            self.analyze_usages();
        }
    }

    fn analyze_statements(&mut self, statements: &[Statement]) {
        for statement in statements {
            if let StatementKind::Structure(structure) = &statement.kind {
                self.analyze_structure(statement, structure);
            }
        }

        for statement in statements {
            if let StatementKind::Interface(interface) = &statement.kind {
                self.analyze_interface(statement, interface);
            }
        }

        for statement in statements {
            if let StatementKind::Function(function) = &statement.kind {
                self.analyze_function_declaration(function, statement.range);
            }
        }

        for statement in statements {
            self.analyze_statement(statement);
        }
    }

    /// Analyze a function declaration (signature) without analyzing the statements inside
    fn analyze_function_declaration(&mut self, function: &FunctionStatement, range: FileRange) {
        if let Some(other) = self.context.current().get_function_mut(&function.name) {
            self.diagnostics.create(||
                SemanticDiagnostic::new(
                    function.name.range(),
                    SemanticDiagnosticKind::DuplicateFunction {
                        name: BabString::clone(&function.name),
                    })
                    .with_related(SemanticRelatedInformation::new(
                        other.name.range(),
                        SemanticRelatedMessage::FunctionDefinedHere {
                            name: BabString::clone(&function.name)
                        }
                    ))
                    .with_action(BabbelaarCodeAction::new_command(function.name.range(), BabbelaarCommand::RenameFunction))
            );
            return;
        }

        let return_type = Box::new(
            match &function.return_type {
                Some(ty) => self.resolve_type(ty),
                None => SemanticType::Builtin(BuiltinType::Null),
            }
        );

        let parameters = function.parameters.iter()
            .map(|param| {
                let ty = self.resolve_type(&param.ty);
                SemanticParameter {
                    name: param.name.clone(),
                    ty: Ranged::new(param.ty.range(), ty),
                }
            })
            .collect();

        self.context.push_function(
            SemanticFunction {
                name: function.name.clone(),
                parameters,
                parameters_right_paren_range: function.parameters_right_paren_range,
                extern_function: None,
                return_type,
            },
            range,
        );
    }

    fn analyze_expression(&mut self, expression: &Ranged<Expression>) -> SemanticValue {
        let value = match expression.value() {
            Expression::BiExpression(bi) => self.analyze_bi_expression(bi),
            Expression::Postfix(postfix) => self.analyze_postfix_expression(postfix),
            Expression::Primary(primary) => self.analyze_primary_expression(primary, expression.range()),
            Expression::Unary(unary) => self.analyze_unary_expression(unary),
        };

        if let Some(value_type_tracker) = &mut self.context.value_type_tracker {
            value_type_tracker.insert(expression.range(), value.ty.clone());
        }

        value
    }

    fn analyze_extension_statement(&mut self, extension: &ExtensionStatement, range: FileRange) {
        self.context.push_extension_scope(extension, range);

        let ty = self.resolve_type_specifier(&extension.type_specifier);

        if !ty.can_be_extended() {
            self.diagnostics.create(|| SemanticDiagnostic::new(extension.type_specifier.range(), SemanticDiagnosticKind::TypeCannotBeExtended { name: extension.type_specifier.fully_qualified_name() }));
            return;
        }

        let interface = extension.interface_specifier.as_ref()
            .and_then(|specifier| self.resolve_interface(specifier));

        let mut ext = SemanticExtension {
            ty,
            interface,
            generic_types: extension.generic_types.iter().map(|x| x.value().clone()).collect(),
            methods: HashMap::new(),
            range,
            right_curly_bracket: extension.right_curly_bracket,
        };

        for method in &extension.methods {
            let name = method.function.name.value();

            let function = self.analyze_function(&method.function, Some(ext.ty.clone()));

            if self.is_invalid_method(&ext, &name, method, function) {
                continue;
            }

            let method = self.create_semantic_method(method);
            ext.methods.insert(name.clone(), method);
        }

        if let Some(interface) = &ext.interface {
            self.analyze_missing_interface_methods(&ext, extension, interface);
        }

        self.context.pop_scope();

        let scope = self.context.current();
        scope.extensions.push(ext);
    }

    /// Returns whether or not this method is invalid.
    fn is_invalid_method(&mut self, ext: &SemanticExtension, name: &BabString, method: &Method, function: SemanticFunctionAnalysis) -> bool {
        if let Some(existing) = ext.methods.get(name) {
            let name = name.clone();
            self.diagnostics.create(||
                SemanticDiagnostic::new(method.function.name.range(), SemanticDiagnosticKind::DuplicateMethodNameInExtension { name: name.clone(), structure: ext.ty.name() })
                    .with_related(SemanticRelatedInformation::new(existing.function.name.range(), SemanticRelatedMessage::DuplicateMethodFirstDefinedHere { name }))
                    .with_action(BabbelaarCodeAction::new_command(method.function.name.range(), BabbelaarCommand::RenameFunction))
            );
            return true;
        }

        if let Some(interface) = &ext.interface {
            self.is_invalid_method_in_interface_extension(name, method, interface, function)
        } else {
            self.is_invalid_method_in_normal_extension(ext, name, method)
        }
    }

    /// Returns whether or not this method is invalid.
    fn is_invalid_method_in_interface_extension(&mut self, name: &BabString, method: &Method, interface: &SemanticInterface, function: SemanticFunctionAnalysis) -> bool {
        let Some(expected_method) = interface.methods.iter().find(|x| x.name() == name) else {
            self.diagnostics.create(||
                SemanticDiagnostic::new(
                    method.function.name.range(),
                    SemanticDiagnosticKind::InvalidInterfaceExtensionMethod {
                        name: name.clone(),
                        interface: interface.name.value().clone(),
                    }
                )
                .with_related(SemanticRelatedInformation::new(
                    interface.name.range(),
                    SemanticRelatedMessage::InterfaceDefinedHere { name: interface.name.value().clone() },
                ))
            );

            return true;
        };

        for (index, (interface_param, actual_param)) in expected_method.function.parameters.iter().zip(&method.function.parameters).enumerate() {
            let expected_type = self.refine_type(&interface_param.ty);
            let actual_type = &function.parameters[index];
            if expected_type != *actual_type {
                self.diagnostics.create(||
                    SemanticDiagnostic::new(
                        actual_param.ty.range(),
                        SemanticDiagnosticKind::InterfaceDeclarationHasDifferentParameters {
                            expected: expected_type.to_string(),
                            actual: actual_type.to_string(),
                        }
                    )
                );
            }
        }

        let expected_params = expected_method.function.parameters.len();
        let actual_params = method.function.parameters.len();

        if expected_params < actual_params {
            self.diagnostics.create(||
                SemanticDiagnostic::new(
                    FileRange::new(
                        method.function.parameters[actual_params].name.range().start(),
                        method.function.parameters.last().unwrap().name.range().start(),
                    ),
                    SemanticDiagnosticKind::TooManyParametersForInterfaceMethod {
                        expected: expected_params,
                        actual: actual_params,
                    }
                )
            );
        } else if expected_params > actual_params {
            self.diagnostics.create(||
                SemanticDiagnostic::new(
                    FileRange::new(
                        method.function.parameters[actual_params].name.range().start(),
                        method.function.parameters.last().unwrap().name.range().start(),
                    ),
                    SemanticDiagnosticKind::TooFewParametersForInterfaceMethod {
                        expected: expected_params,
                        actual: actual_params,
                    }
                )
            );
        }

        let return_type = function.return_type.unwrap_or(SemanticType::null());
        let expected_ty = self.refine_type(&expected_method.function.return_type);

        if return_type != expected_ty {
            let range = method.function.return_type.as_ref().map(|x| x.range()).unwrap_or(method.function.parameters_right_paren_range);

            let related_our_type = SemanticRelatedInformation::new(return_type.declaration_range(), SemanticRelatedMessage::TypeDefinedHere { typ: return_type.to_string().into() });
            let related_their_type = SemanticRelatedInformation::new(expected_ty.declaration_range(), SemanticRelatedMessage::TypeDefinedHere { typ: expected_ty.to_string().into() });

            self.diagnostics.create(||
                SemanticDiagnostic::new(
                    range,
                    SemanticDiagnosticKind::InterfaceDeclarationHasDifferentReturnType {
                        expected: expected_method.return_type(),
                        actual: return_type,
                    }
                )
                .with_related(related_our_type)
                .with_related(related_their_type)
            );
        }

        false
    }

    /// Returns whether or not this method is invalid.
    fn is_invalid_method_in_normal_extension(&mut self, ext: &SemanticExtension, name: &BabString, method: &Method) -> bool {
        if let SemanticType::Custom { base, .. } = &ext.ty {
            if let Some(existing) = base.methods.iter().find(|x| x.name() == name) {
                let name = name.clone();
                self.diagnostics.create(||
                    SemanticDiagnostic::new(method.function.name.range(), SemanticDiagnosticKind::DuplicateMethodNameInExtension { name: name.clone(), structure: ext.ty.name() })
                        .with_related(SemanticRelatedInformation::new(existing.function.name.range(), SemanticRelatedMessage::DuplicateMethodFirstDefinedHere { name }))
                        .with_action(BabbelaarCodeAction::new_command(method.function.name.range(), BabbelaarCommand::RenameFunction))
                );
                return true;
            }
        }

        false
    }

    fn analyze_missing_interface_methods(&mut self, ext: &SemanticExtension, ast: &ExtensionStatement, interface: &SemanticInterface) {
        let mut methods = Vec::new();

        for method in &interface.methods {
            if !ext.methods.contains_key(method.name()) {
                methods.push(method.name().clone());
            }
        }

        if methods.is_empty() {
            return;
        }

        let count = methods.len();
        let mut names = String::new();

        for (idx, method) in methods.iter().enumerate() {
            if idx != 0 {
                if idx == count - 1 {
                    names += " en ";
                } else {
                    names += ", ";
                }
            }

            names += "`";
            names += method.as_str();
            names += "`";
        }

        self.diagnostics.create(||
            SemanticDiagnostic::new(ast.right_curly_bracket, SemanticDiagnosticKind::MissingMethodsInInterfaceExtension { names, count })
        );
    }

    fn analyze_function(&mut self, function: &FunctionStatement, this: Option<SemanticType>) -> SemanticFunctionAnalysis {
        let is_definition = function.body.is_some();

        self.context.push_function_scope(function, this);

        let mut analysis = SemanticFunctionAnalysis::default();

        let mut params = HashSet::new();

        for param in &function.parameters {
            let typ = self.resolve_type(&param.ty);
            analysis.parameters.push(typ.clone());

            if !params.insert(param.name.value().clone()) {
                self.diagnostics.create(||
                    SemanticDiagnostic::new(
                        param.name.range(),
                        SemanticDiagnosticKind::DuplicateParameterName {
                            name: param.name.value().clone(),
                        }
                    )
                    .with_action(BabbelaarCodeAction::new_command(param.name.range(), BabbelaarCommand::RenameParameter))
                );
                continue;
            }

            if let Some(tracker) = &mut self.context.definition_tracker {
                tracker.insert(param.ty.range(), SemanticReference {
                    local_name: param.ty.specifier.fully_qualified_name(),
                    local_kind: SemanticLocalKind::StructureReference,
                    declaration_range: typ.declaration_range(),
                    typ: typ.clone(),
                });
            }

            if is_definition {
                self.context.push_local(&param.name, SemanticLocal::new(
                    SemanticLocalKind::Parameter,
                    typ,
                    param.name.range(),
                ));
            }

            self.analyze_attributes_for_parameter(function, param);
        }

        if let Some(ty) = &function.return_type {
            let typ = self.resolve_type(ty);

            if let Some(tracker) = &mut self.context.definition_tracker {
                tracker.insert(ty.range(), SemanticReference {
                    local_name: function.name.value().clone(),
                    local_kind: SemanticLocalKind::StructureReference,
                    declaration_range: typ.declaration_range(),
                    typ: typ.clone(),
                });
            }

            analysis.return_type = Some(typ.clone());
            self.context.current().return_type = Some(Ranged::new(ty.range(), typ));
        }

        self.analyze_statements(function.body.as_inner_slice());

        self.context.pop_scope();

        analysis
    }

    pub fn analyze_statement(&mut self, statement: &Statement) {
        self.analyze_attributes_for_statement(statement);

        match &statement.kind {
            StatementKind::Expression(expr) => {
                let value = self.analyze_expression(expr);

                if let SemanticUsage::Pure(pure) = value.usage {
                    let diag = SemanticDiagnostic::new(expr.range(), SemanticDiagnosticKind::UnusedPureValue { ty: value.ty.to_string().into() })
                        .warn()
                        .with_action(BabbelaarCodeAction::new(
                            BabbelaarCodeActionType::AssignToNewVariable,
                            vec![
                                FileEdit::new(
                                    statement.range.start().as_zero_range(),
                                    match self.find_canonical_name_for_variable(expr) {
                                        Some(name) => format!("stel {name}"),
                                        None => format!("stel {} = ", value.ty.value_or_field_name_hint()),
                                    }
                                )
                            ]
                        ))
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

                    self.diagnostics.create(|| diag);
                }
            }
            StatementKind::Extension(extension) => {
                self.analyze_extension_statement(extension, statement.range);
            }
            StatementKind::Assignment(assign) => {
                let source_type = self.analyze_expression(&assign.source).ty;

                self.context.statements_state.push(StatementAnalysisState {
                    assignment_type: Some(source_type.clone()),
                    ..Default::default()
                });

                if assign.destination.value().as_identifier() != Some(&Constants::DISCARDING_IDENT) {
                    let destination_type = self.analyze_expression(&assign.destination).ty;
                    self.analyze_assignment_destination(assign.range(), &assign.destination);
                    self.analyze_assignment_source_dest(assign, destination_type, source_type);
                }

                self.context.statements_state.pop();
            }
            StatementKind::For(statement) => self.analyze_for_statement(statement),
            StatementKind::Function(function) => {
                self.analyze_function(function, None);
            }
            StatementKind::If(statement) => self.analyze_if_statement(statement),
            StatementKind::Interface(..) => (),
            StatementKind::Return(function) => self.analyze_return_statement(function),
            StatementKind::Structure(..) => (),
            StatementKind::Variable(variable) => self.analyze_variable_statement(variable, statement),
        }
    }

    fn analyze_assignment_destination(&mut self, range: FileRange, expression: &Expression) {
        match expression {
            Expression::Primary(PrimaryExpression::Reference(..)) => return,

            Expression::Postfix(postfix) => {
                match postfix.kind.value() {
                    PostfixExpressionKind::Member(..) => return,
                    PostfixExpressionKind::Subscript(..) => return,
                    _ => (),
                }
            }

            _ => (),
        }

        self.diagnostics.create(|| SemanticDiagnostic::new(
            range,
            SemanticDiagnosticKind::ExpressionCannotBeUsedAsAssignmentDestination {
                expression: expression.clone(),
            }
        ));
    }

    fn analyze_for_statement(&mut self, statement: &ForStatement) {
        let ty = match statement.iterable.value() {
            ForIterableKind::Expression(expression) => {
                let ty = self.analyze_expression(expression).ty;
                self.analyze_for_statement_iterable_expression(expression, ty)
            }

            ForIterableKind::Range(range) => {
                self.analyze_range(range);
                SemanticType::Builtin(BuiltinType::G32)
            }
        };

        if let Some(tracker) = &mut self.context.declaration_tracker {
            tracker.push(SemanticReference {
                local_name: statement.iterator_name.value().clone(),
                local_kind: SemanticLocalKind::Iterator,
                declaration_range: statement.iterator_name.range(),
                typ: ty.clone(),
            });
        }

        let scope = self.context.push_block_scope(statement.file_range);
        scope.locals.insert(statement.iterator_name.value().clone(), SemanticLocal::new(
            SemanticLocalKind::Iterator,
            ty,
            statement.iterator_name.range(),
        ));

        self.analyze_statements(&statement.body);

        self.context.pop_scope();
    }

    fn analyze_for_statement_iterable_expression(&mut self, expression: &Ranged<Expression>, ty: SemanticType) -> SemanticType {
        match ty {
            SemanticType::Array(item_type) => {
                return *item_type;
            }

            SemanticType::Custom { ref base, ref parameters } => {
                if let Some(interface) = self.resolve_interface_by_name(&BabString::new_static("Doorloper")) {
                    if let Some(extension) = self.get_interface_implementation_for(&ty, &interface) {
                        let ty_name = interface.generic_types[0].value().clone();
                        let ty_idx = base.index_of_generic_type(&ty_name).expect("Het vinden van de generieke parameter van `Doorloper`");
                        _ = extension; // Dit zou gebruikt moeten worden?
                        return parameters[ty_idx].clone();
                    }
                }

                self.emit_diagnostic(|this| {
                    SemanticDiagnostic::new(expression.range(), SemanticDiagnosticKind::ExpressionNotIterable)
                        .with_related(SemanticRelatedInformation::new(expression.range(), SemanticRelatedMessage::ExpressionIsOfType { ty: ty.clone() }))
                        .with_action(this.create_actions_extend_structure_with_interface_by_name(&base, expression.range().file_id(), "Doorloper"))
                });

                return ty;
            }

            _ => {
                self.diagnostics.create(||
                    SemanticDiagnostic::new(expression.range(), SemanticDiagnosticKind::ExpressionNotIterableStructure)
                        .with_related(SemanticRelatedInformation::new(expression.range(), SemanticRelatedMessage::ExpressionIsOfType { ty: ty.clone() }))
                );

                ty
            }
        }
    }

    fn analyze_range(&mut self, range: &RangeExpression) {
        self.analyze_range_param("Startwaarde", &range.start);
        self.analyze_range_param("Eindwaarde", &range.end);
    }

    fn analyze_range_param(&mut self, name: &'static str, expression: &Ranged<Expression>) {
        let ty = self.analyze_expression(expression).ty;

        if ty == SemanticType::null() {
            return;
        }

        if ty != SemanticType::Builtin(BuiltinType::G32) {
            let conversion_actions = self.try_create_conversion_actions(&SemanticType::Builtin(BuiltinType::G32), &ty, expression);
            self.diagnostics.create(||
                SemanticDiagnostic::new(expression.range(), SemanticDiagnosticKind::RangeExpectsInteger { name, ty })
                    .with_actions(conversion_actions)
            );
        }
    }

    fn analyze_if_statement(&mut self, statement: &IfStatement) {
        self.context.push_block_scope(statement.range);

        self.analyze_expression(&statement.condition);

        self.analyze_statements(&statement.body);

        self.context.pop_scope();
    }

    fn analyze_return_statement(&mut self, statement: &ReturnStatement) {
        match (&statement.expression, self.context.current().return_type.clone()) {
            (Some(actual), Some(expected)) => {
                let actual_type = self.analyze_expression(actual);
                let conversion_actions = self.try_create_conversion_actions(&expected, &actual_type.ty, actual);

                if actual_type.ty == SemanticType::Builtin(BuiltinType::Null) {
                    return;
                }

                if actual_type.ty != *expected.value() {
                    self.diagnostics.create(||
                        SemanticDiagnostic::new(
                            actual.range(),
                            SemanticDiagnosticKind::ReturnStatementIncompatibleTypes {
                                actual: actual_type.ty.clone(),
                                expected: expected.value().clone(),
                            },
                        )
                        .with_related(SemanticRelatedInformation::new(
                            expected.range(),
                            SemanticRelatedMessage::ReturnTypeDefinedHere {
                                typ: expected.name(),
                            }
                        ))
                        .with_actions(conversion_actions)
                        .with_action(BabbelaarCodeAction::new(
                            BabbelaarCodeActionType::ChangeReturnType {
                                typ: actual_type.ty.name(),
                            },
                            vec![
                                FileEdit::new(expected.range(), actual_type.ty.name().to_string())
                            ]
                        ))
                    );
                }
            }

            (Some(actual), None) => {
                let actual_type = self.analyze_expression(actual);

                if actual_type.ty == SemanticType::Builtin(BuiltinType::Null) {
                    return;
                }

                let right_parameter_range = self.context.scope.iter().rev()
                    .filter_map(|x| match &x.kind {
                        SemanticScopeKind::Default => None,
                        SemanticScopeKind::TopLevel => None,
                        SemanticScopeKind::Structure => None,
                        SemanticScopeKind::Werkwijze => None,
                        SemanticScopeKind::Function { right_parameter_range, .. } => Some(right_parameter_range.clone()),
                    })
                    .next();

                let diag = SemanticDiagnostic::new(
                    actual.range(),
                    SemanticDiagnosticKind::ReturnStatementExpectedNoValue
                );

                let diag = diag.with_action(right_parameter_range.map(|right_parameter_range| {
                    let edit = FileEdit::new(
                        right_parameter_range.end().as_zero_range(),
                        format!(" -> {}", actual_type.ty),
                    );

                    BabbelaarCodeAction::new(
                        BabbelaarCodeActionType::AddReturnType {
                            typ: actual_type.ty.name()
                        },
                        vec![edit]
                    )
                }));

                let diag = diag.with_action(
                    BabbelaarCodeAction::new(
                        BabbelaarCodeActionType::RemoveExpression,
                        vec![
                            FileEdit::new(
                                FileRange::new(statement.keyword_range.end(), actual.range().end()),
                                String::new()
                            )
                        ]
                    )
                );

                self.diagnostics.create(|| diag);
            }

            (None, Some(expected)) => {
                let right_parameter_range = self.context.scope.iter().rev()
                    .filter_map(|x| match &x.kind {
                        SemanticScopeKind::Default => None,
                        SemanticScopeKind::TopLevel => None,
                        SemanticScopeKind::Structure => None,
                        SemanticScopeKind::Werkwijze => None,
                        SemanticScopeKind::Function { right_parameter_range, .. } => Some(right_parameter_range.clone()),
                    })
                    .next();

                self.diagnostics.create(||
                    SemanticDiagnostic::new(
                        statement.keyword_range.end().as_zero_range(),
                        SemanticDiagnosticKind::ReturnStatementExpectedValue {
                            typ: expected.name()
                        },
                    )
                    .with_related(SemanticRelatedInformation::new(
                        expected.range(),
                        SemanticRelatedMessage::ReturnTypeDefinedHere {
                            typ: expected.name(),
                        }
                    ))
                    .with_action(right_parameter_range.map(|right_parameter_range| {
                        BabbelaarCodeAction::new(
                            BabbelaarCodeActionType::RemoveReturnType,
                            vec![
                                FileEdit::new(
                                    FileRange::new(right_parameter_range.end(), expected.range().end()),
                                    String::new(),
                                )
                            ]
                        )
                    }))
                );
            }

            (None, None) => (),
        }
    }

    fn analyze_interface(&mut self, statement: &Statement, interface: &InterfaceStatement) {
        self.context.push_interface_scope(interface);

        let methods = interface.methods.iter().map(|x| self.create_semantic_method(x)).collect();

        let semantic_interface = Arc::new(SemanticInterface {
            attributes: statement.attributes.clone(),
            name: interface.name.clone(),
            generic_types: interface.generic_types.clone(),
            left_curly_range: interface.left_curly_range,
            right_curly_range: interface.right_curly_range,
            methods,
        });

        self.context.push_interface(Arc::clone(&semantic_interface));

        let this_type = Some(SemanticType::Interface {
            base: semantic_interface,
            parameters: Vec::new(),
        });

        let mut names = HashSet::new();
        for method in &interface.methods {
            if !names.insert(method.function.name.value()) {
                self.diagnostics.create(||
                    SemanticDiagnostic::new(
                        method.function.name.range(),
                        SemanticDiagnosticKind::DuplicateMethodNameInInterface { name: method.function.name.value().clone(), interface: interface.name.value().clone() },
                    )
                    .with_action(BabbelaarCodeAction::new_command(method.function.name.range(), BabbelaarCommand::RenameFunction))
                );
            }

            self.analyze_function(&method.function, this_type.clone());
        }

        self.context.pop_scope();
    }

    fn analyze_structure(&mut self, statement: &Statement, structure: &Structure) {
        self.context.push_structure_scope(structure);

        let fields: Vec<SemanticField> = structure.fields.iter().map(|x| SemanticField {
            attributes: x.attributes.clone(),
            name: x.name.clone(),
            ty: self.resolve_type(&x.ty),
            has_default_value: x.default_value.is_some(),
        }).collect();

        let methods = structure.methods.iter().map(|x| self.create_semantic_method(x)).collect();

        let semantic_structure = Arc::new(SemanticStructure {
            attributes: statement.attributes.clone(),
            name: structure.name.clone(),
            generic_types: structure.generic_types.clone(),
            left_curly_range: structure.left_curly_range,
            right_curly_range: structure.right_curly_range,
            fields,
            methods,
        });

        self.context.push_structure(Arc::clone(&semantic_structure));

        for field in &structure.fields {
            if let Some(default_value) = &field.default_value {
                self.analyze_expression(default_value);
            }
        }

        let mut names = HashSet::new();
        for field in &semantic_structure.fields {
            if !names.insert(field.name.value()) {
                self.diagnostics.create(||
                    SemanticDiagnostic::new(
                        field.name.range(),
                        SemanticDiagnosticKind::DuplicateFieldName { name: field.name.value().clone() },
                    )
                    .with_action(BabbelaarCodeAction::new_command(field.name.range(), BabbelaarCommand::RenameField))
                );
            }

            self.analyze_attributes_for_field(field);
        }

        let this_type = Some(SemanticType::Custom {
            base: Arc::clone(&semantic_structure),
            parameters: Vec::new(),
        });

        let mut names = HashSet::new();
        for method in &structure.methods {
            if !names.insert(method.function.name.value()) {
                self.diagnostics.create(||
                    SemanticDiagnostic::new(
                        method.function.name.range(),
                        SemanticDiagnosticKind::DuplicateMethodNameInStructure { name: method.function.name.value().clone(), structure: structure.name.value().clone() },
                    )
                    .with_action(BabbelaarCodeAction::new_command(method.function.name.range(), BabbelaarCommand::RenameFunction))
                );
            }

            self.analyze_function(&method.function, this_type.clone());
        }

        self.context.pop_scope();
    }

    #[must_use]
    fn create_semantic_method(&mut self, method: &Method) -> SemanticMethod {
        SemanticMethod {
            range: method.range,
            function: self.create_semantic_function(&method.function),
        }
    }

    #[must_use]
    fn create_semantic_function(&mut self, function: &FunctionStatement) -> SemanticFunction {
        let parameters = function.parameters.iter()
            .map(|param| self.create_semantic_parameter(param))
            .collect();

        let return_type = Box::new(
            match &function.return_type {
                Some(ty) => self.resolve_type(ty),
                None => SemanticType::Builtin(BuiltinType::Null),
            }
        );

        SemanticFunction {
            name: function.name.clone(),
            parameters,
            parameters_right_paren_range: function.parameters_right_paren_range,
            extern_function: None,
            return_type,
        }
    }

    fn create_semantic_parameter(&mut self, parameter: &Parameter) -> SemanticParameter {
        let ty = self.resolve_type(&parameter.ty);
        SemanticParameter {
            name: parameter.name.clone(),
            ty: Ranged::new(parameter.ty.range(), ty),
        }
    }

    fn analyze_variable_statement(&mut self, statement: &VariableStatement, stmt: &Statement) {
        let typ = self.analyze_expression(&statement.expression).ty;

        if statement.name.value() == &Constants::DISCARDING_IDENT {
            return;
        }

        let local = SemanticLocal::new(
            SemanticLocalKind::Variable,
            typ,
            statement.name.range(),
        );

        let local = local.with_declaration_range(stmt.range);

        self.context.push_local(&statement.name, local);
    }

    fn analyze_bi_expression(&mut self, expression: &BiExpression) -> SemanticValue {
        let lhs_type = self.analyze_expression(&expression.lhs).ty;
        let rhs_type = self.analyze_expression(&expression.rhs).ty;

        if !lhs_type.is_null() && !rhs_type.is_null() && !lhs_type.is_compatible_with(&rhs_type) {
            self.diagnostics.create(|| SemanticDiagnostic::new(
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

    fn analyze_function_call_expression(&mut self, lhs: SemanticType, expression: &FunctionCallExpression, postfix: &PostfixExpression) -> SemanticValue {
        // TODO: why do we this again? we only need the function and function ref...
        let function_name = match lhs {
            SemanticType::Array(..) => postfix.lhs.value().to_string().into(),
            SemanticType::Builtin(BuiltinType::Null) => postfix.lhs.value().to_string().into(),
            SemanticType::Builtin(builtin) => builtin.name(),
            SemanticType::Custom { base, .. } => base.name.value().clone(),
            SemanticType::Function(func) => func.name.value().clone(),
            SemanticType::FunctionReference(func) => func.name(),
            SemanticType::IndexReference(ty) => ty.name().clone(),
            SemanticType::Interface { base, .. } => base.name.value().clone(),
            SemanticType::Generic(ty) => ty.name.clone(),
            SemanticType::Pointer(..) => postfix.lhs.value().to_string().into(),
        };

        let Some(function) = self.find_and_use_function(&function_name) else {
            let diag = SemanticDiagnostic::new(
                postfix.lhs.range(),
                SemanticDiagnosticKind::InvalidFunctionReference { name: function_name.clone() }
            );

            let diag = diag.with_action(self.create_action_create_function(function_name, expression));

            self.diagnostics.create(|| diag);
            return SemanticValue::null();
        };

        let ret_typ = function.return_value();

        self.analyze_function_parameters(function_name, function, expression, None);

        SemanticValue {
            usage: if ret_typ == SemanticType::Builtin(BuiltinType::Null) {
                SemanticUsage::Indifferent
            } else {
                SemanticUsage::Pure(PureValue::ReturnValue)
            },

            ty: ret_typ,
        }
    }

    fn analyze_function_parameters(
        &mut self,
        function_name: BabString,
        function: SemanticReference,
        expression: &FunctionCallExpression,
        this_structure: Option<&SemanticType>
    ) {
        let function_hint = SemanticRelatedInformation::new(
            function.declaration_range,
            SemanticRelatedMessage::FunctionDefinedHere { name: function_name.clone() }
        );

        let param_count = function.typ.parameter_count().expect("This reference to be a function, verified by self.find_function()");
        let arg_count = expression.arguments.len();

        if param_count > arg_count {
            self.diagnostics.create(|| SemanticDiagnostic::new(
                expression.token_right_paren,
                SemanticDiagnosticKind::TooFewArguments { function_name: function_name.clone(), param_count, arg_count },
            ).with_related(function_hint.clone()));
        }

        if param_count < arg_count {
            let mut add_parameter_action = None;
            let mut remove_parameter_action = None;

            if let SemanticType::FunctionReference(FunctionReference::Custom(func)) = &function.typ {
                let residual_args = arg_count - param_count;
                let mut params = String::new();

                for (idx, arg) in expression.arguments.iter().enumerate().skip(param_count) {
                    if idx != 0 {
                        params += ", ";
                    }

                    let argument_type = self.analyze_expression(arg).ty;
                    params += &format!("param{idx}: {argument_type}");
                }

                let end = expression.arguments.last().unwrap().range().end();
                let range = if param_count == 0 {
                    FileRange::new(expression.arguments[0].range().start(), end)
                } else {
                    FileRange::new(expression.arguments[param_count - 1].range().end(), end)
                };

                let edit = FileEdit::new(range, "");
                remove_parameter_action = Some(BabbelaarCodeAction::new(BabbelaarCodeActionType::RemoveArgument { residual_args }, vec![ edit ]));

                let insert_range = func.parameters.last()
                    .map(|x| x.ty.range().end())
                    .unwrap_or_else(|| func.parameters_right_paren_range.start());

                let edit = FileEdit::new(insert_range.as_zero_range(), params);
                add_parameter_action = Some(BabbelaarCodeAction::new(BabbelaarCodeActionType::AddParameter { residual_args }, vec![ edit ]));
            }


            self.diagnostics.create(||
                SemanticDiagnostic::new(
                    expression.arguments[param_count].range(),
                    SemanticDiagnosticKind::TooManyArguments { function_name: BabString::new(format!("{function:#?}")), param_count, arg_count },
                )
                .with_related(function_hint.clone())
                .with_action(remove_parameter_action)
                .with_action(add_parameter_action)
            );
            return;
        }

        for (arg_idx, arg) in expression.arguments.iter().enumerate() {
            let argument_type = self.analyze_expression(arg).ty;

            let Some(parameter_type) = self.resolve_parameter_type(&function, arg_idx) else {
                warn!("Cannot check type of parameter with index {arg_idx}");
                break;
            };

            let parameter_type = match this_structure {
                Some(this) => parameter_type.resolve_against(this),
                None => parameter_type,
            };

            if argument_type != parameter_type {
                let param_hint = self.resolve_parameter_name(&function, arg_idx)
                    .map(|x| SemanticRelatedInformation::new(
                        x.range(),
                        SemanticRelatedMessage::ParameterDeclaredHere { name: x.value().clone()}
                    ));

                self.diagnostics.create(|| SemanticDiagnostic::new(
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
            PrimaryExpression::CharacterLiteral(..) => {
                SemanticType::Builtin(BuiltinType::Teken)
            }

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
                    self.diagnostics.create(|| diag);
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
                if reference.value() == &Constants::DISCARDING_IDENT {
                    self.diagnostics.create(||
                        SemanticDiagnostic::new(
                            reference.range(),
                            SemanticDiagnosticKind::DiscardingIdentifierUsedAsReference,
                        )
                    );
                    return SemanticValue::null();
                }

                let Some(local) = self.find_local_by_name(|name| name == reference.value()) else {
                    self.diagnostics.create(|| SemanticDiagnostic::new(
                        reference.range(),
                        SemanticDiagnosticKind::InvalidIdentifierReference { identifier: reference.value().clone() }
                    ));
                    return SemanticValue::null();
                };

                let local_reference = SemanticReference {
                    local_name: reference.value().clone(),
                    local_kind: local.kind,
                    declaration_range: local.name_declaration_range,
                    typ: local.typ.clone(),
                };

                let typ = local.typ.clone();
                if let Some(tracker) = &mut self.context.definition_tracker {
                    tracker.insert(reference.range(), local_reference);
                }

                typ
            }

            PrimaryExpression::Parenthesized(expr) => return self.analyze_expression(expr),

            PrimaryExpression::SizedArrayInitializer { typ, size } => {
                let size_value = self.analyze_expression(&size);
                if size_value.ty != SemanticType::Builtin(BuiltinType::G32) {
                    self.diagnostics.create(|| SemanticDiagnostic::new(
                        size.range(),
                        SemanticDiagnosticKind::SizedArrayInitializerInvalidSize,
                    ));
                }

                let ty = self.resolve_type(typ);

                SemanticType::Array(Box::new(ty))
            }
        };

        SemanticValue {
            ty,
            usage: SemanticUsage::Pure(PureValue::ConstantValue),
        }
    }

    fn analyze_structure_instantiation(&mut self, instantiation: &StructureInstantiationExpression) -> SemanticValue {
        let ty = self.resolve_type_by_name(&instantiation.name, &instantiation.type_parameters, Some(instantiation));
        let SemanticType::Custom { base, .. } = &ty else {
            return SemanticValue::null();
        };

        let struct_hint = SemanticRelatedInformation::new(
            base.name.range(),
            SemanticRelatedMessage::StructureDefinedHere { name: base.name.value().clone() }
        );

        let all_valid_fields: HashMap<&BabString, &SemanticField> = base.fields.iter().map(|x| (x.name.value(), x)).collect();
        let mut fields_left = all_valid_fields.clone();

        if let Some(tracker) = &mut self.context.definition_tracker {
            tracker.insert(instantiation.name.range(), SemanticReference {
                local_name: instantiation.name.value().clone(),
                local_kind: SemanticLocalKind::StructureReference,
                declaration_range: base.name.range(),
                typ: ty.clone(),
            });
        }

        for field_instantiation in &instantiation.fields {
            let name = &field_instantiation.name;
            match fields_left.remove(name.value()) {
                Some(field) => {
                    let declaration_type = field.ty.clone().resolve_against(&ty);
                    let definition_type = self.analyze_expression(&field_instantiation.value).ty;
                    if !declaration_type.is_compatible_with(&definition_type) {
                        let actions = self.try_create_conversion_actions(&declaration_type, &definition_type, &field_instantiation.value);

                        self.diagnostics.create(|| SemanticDiagnostic::new(
                            field_instantiation.value.range(),
                            SemanticDiagnosticKind::IncompatibleFieldTypes {
                                struct_name: base.name.value().clone(),
                                field_name: name.value().clone(),
                                declaration_type: declaration_type.to_string(),
                                definition_type: definition_type.to_string(),
                            },
                        ).with_related(struct_hint.clone()).with_actions(actions));
                    }

                    if let Some(tracker) = &mut self.context.definition_tracker {
                        tracker.insert(field_instantiation.name.range(), SemanticReference {
                            local_name: field.name.value().clone(),
                            local_kind: SemanticLocalKind::FieldReference,
                            declaration_range: field.name.range(),
                            typ: declaration_type.clone(),
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

                        self.diagnostics.create(|| diag.with_related(field_def_hint).with_related(struct_hint.clone()));
                    } else {
                        let definition_type = self.analyze_expression(&field_instantiation.value).ty;
                        let create_field_actions = self.create_actions_create_field(&ty, &field_instantiation.name, definition_type);

                        self.diagnostics.create(||
                            SemanticDiagnostic::new(
                                name.range(),
                                SemanticDiagnosticKind::InvalidFieldInstantiation {
                                    struct_name: base.name.value().clone(),
                                    field_name: name.value().clone()
                                },
                            )
                            .with_actions(create_field_actions)
                            .with_related(struct_hint.clone()),
                        );
                    }
                }
            }
        }

        let count_fields_left = fields_left.iter()
            .filter(|(_, field)| !field.has_default_value)
            .count();

        if count_fields_left != 0 {
            let names = fields_left.iter()
                .filter(|(_, x)| !x.has_default_value)
                .map(|(x, _)| x.as_str())
                .join("`, `");

            self.diagnostics.create(|| SemanticDiagnostic::new(instantiation.range, SemanticDiagnosticKind::MissingFieldInitializers {
                names,
                field_word: if count_fields_left == 1 {
                    "Het veld"
                } else {
                    "De velden"
                },
                verb: if count_fields_left == 1 {
                    "ontbreekt"
                } else {
                    "ontbreken"
                },
            }));
        }

        SemanticValue {
            ty,
            usage: SemanticUsage::Pure(PureValue::ConstantValue),
        }
    }

    fn analyze_unary_expression(&mut self, unary: &UnaryExpression) -> SemanticValue {
        let ty = self.analyze_expression(&unary.rhs).ty;

        let operator_range = unary.kind.range();

        match unary.kind.value() {
            UnaryExpressionKind::Negate => {
                if ty != SemanticType::Builtin(BuiltinType::G32) {
                    self.diagnostics.create(|| {
                        SemanticDiagnostic::new(operator_range, SemanticDiagnosticKind::CannotNegateNonInteger)
                    });
                }
            }
        }

        SemanticValue {
            ty,
            usage: SemanticUsage::Pure(PureValue::Operator { operator_range }),
        }
    }

    fn find_local_by_name<'this, P>(&'this mut self, predicate: P) -> Option<&'this SemanticLocal>
            where P: Fn(&str) -> bool {
        for scope in self.context.scope.iter_mut().rev() {
            for (local_name, local) in &mut scope.locals {
                if predicate(local_name) {
                    local.add_usage();
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

    pub fn find_function_by_name<P>(&self, predicate: P) -> Option<SemanticReference>
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

    fn find_and_use_function(&mut self, name: &str) -> Option<SemanticReference> {
        for scope in self.context.scope.iter_mut().rev() {
            for (func_name, func) in &mut scope.locals {
                if !func.kind.is_function() {
                    continue;
                }

                if name == func_name {
                    func.add_usage();

                    return Some(SemanticReference {
                        local_name: func_name.clone(),
                        local_kind: SemanticLocalKind::FunctionReference,
                        declaration_range: func.typ.declaration_range(),
                        typ: func.typ.clone(),
                    });
                }
            }
        }

        None
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
        self.diagnostics.to_vec()
    }

    #[must_use]
    pub fn diagnostics(&self) -> &[SemanticDiagnostic] {
        self.diagnostics.as_slice()
    }

    fn resolve_interface(&mut self, specifier: &Ranged<InterfaceSpecifier>) -> Option<Arc<SemanticInterface>> {
        if let Some(interface) = self.resolve_interface_by_name(&specifier.name) {
            return Some(interface);
        }

        let diag = SemanticDiagnostic::new(specifier.name.range(), SemanticDiagnosticKind::UnknownInterface { name: specifier.name.value().clone() });

        self.diagnostics.create(|| diag);

        None
    }

    fn resolve_interface_by_name(&self, name: &BabString) -> Option<Arc<SemanticInterface>> {
        for scope in self.context.scope.iter().rev() {
            if let Some(interface) = scope.interfaces.get(&name) {
                return Some(Arc::clone(&interface));
            }
        }

        None
    }

    #[must_use]
    pub fn resolve_type(&mut self, ty: &Ranged<Type>) -> SemanticType {
        let mut semantic_type = self.resolve_type_specifier(&ty.specifier);

        for qual in &ty.qualifiers {
            semantic_type = match qual.value() {
                TypeQualifier::Array => SemanticType::Array(Box::new(semantic_type)),
                TypeQualifier::Pointer => SemanticType::Pointer(Box::new(semantic_type)),
            };
        }

        semantic_type
    }

    #[must_use]
    fn resolve_type_specifier(&mut self, specifier: &TypeSpecifier) -> SemanticType {
        match specifier {
            TypeSpecifier::BuiltIn(ty) => SemanticType::Builtin(*ty.value()),
            TypeSpecifier::Custom { name, type_parameters } => {
                self.resolve_type_by_name(name, &type_parameters, None)
            }
        }
    }

    /// This re-resolves a type, such that given a generic identifier is
    /// originally resolved to an generic type, but later the type of the
    /// generic is known such that the resolved type can now be further
    /// resolved to that concrete type.
    fn refine_type(&mut self, ty: &SemanticType) -> SemanticType {
        match ty {
            SemanticType::Generic(ty) => {
                for scope in self.context.scope.iter().rev() {
                    if let Some(generic) = scope.generic_types.get(&ty.name) {
                        return SemanticType::Generic(generic.clone());
                    }
                }

                SemanticType::Generic(ty.clone())
            }

            SemanticType::Custom { base, parameters } => {
                let parameters = parameters.iter().map(|x| self.refine_type(x)).collect();
                SemanticType::Custom { base: Arc::clone(&base), parameters }
            }

            SemanticType::Interface { base, parameters } => {
                let parameters = parameters.iter().map(|x| self.refine_type(x)).collect();
                SemanticType::Interface { base: Arc::clone(&base), parameters }
            }

            SemanticType::Array(ty) => {
                SemanticType::Array(Box::new(self.refine_type(&ty)))
            }

            SemanticType::IndexReference(ty) => {
                SemanticType::IndexReference(Box::new(self.refine_type(&ty)))
            }
            SemanticType::Pointer(ty) => {
                SemanticType::Pointer(Box::new(self.refine_type(&ty)))
            }

            SemanticType::Builtin(..) => ty.clone(),
            SemanticType::Function(..) => ty.clone(),
            SemanticType::FunctionReference(..) => ty.clone(),
        }
    }

    #[must_use]
    fn resolve_type_by_name(&mut self, name: &Ranged<BabString>, params: &Ranged<Vec<Ranged<Type>>>, instantiation: Option<&StructureInstantiationExpression>) -> SemanticType {
        for scope in self.context.scope.iter().rev() {
            if let Some(generic) = scope.generic_types.get(&name) {
                return SemanticType::Generic(generic.clone());
            }

            if let Some(structure) = scope.structures.get(name.value()) {
                let structure = Arc::clone(structure);

                let mut parameters = Vec::new();
                for parameter in params.value() {
                    parameters.push(self.resolve_type(parameter));
                }

                if params.len() != 0 && structure.generic_types.len() == 0 {
                    self.diagnostics.create(||
                        SemanticDiagnostic::new(params.range(), SemanticDiagnosticKind::TypeParametersUnexpected { ty: structure.name.value().clone() })
                            .with_action(BabbelaarCodeAction::new(BabbelaarCodeActionType::RemoveGenericParameters, [
                                FileEdit::new(params.range(), String::new())
                            ].to_vec()))
                    );
                } else if params.len() < structure.generic_types.len() {
                    self.diagnostics.create(||
                        SemanticDiagnostic::new(params.range(), SemanticDiagnosticKind::TooFewGenericTypes { ty: structure.name.value().clone() })
                    );
                } else if params.len() > structure.generic_types.len() {
                    let range = FileRange::new(params[structure.generic_types.len() - 1].range().end(), params.last().unwrap().range().end());
                    self.diagnostics.create(||
                        SemanticDiagnostic::new(range, SemanticDiagnosticKind::TooManyGenericTypes { ty: structure.name.value().clone() })
                            .with_action(BabbelaarCodeAction::new(BabbelaarCodeActionType::RemoveExtraneousGenericTypes, [
                                FileEdit::new(range, String::new())
                            ].to_vec()))
                    );
                }

                parameters.resize(structure.generic_types.len(), SemanticType::null());

                return SemanticType::Custom {
                    base: structure,
                    parameters,
                };
            }
        }

        self.emit_diagnostic(|this| {
            let body = this.build_structure_body_based_on_instantiation(instantiation);
            SemanticDiagnostic::new(
                name.range(),
                SemanticDiagnosticKind::UnknownType { name: name.value().clone() },
            )
            .with_actions(this.create_actions_create_structure(name, body))
        });

        SemanticType::Builtin(BuiltinType::Null)
    }

    pub fn scopes_surrounding<F>(&self, location: FileLocation, mut f: F)
            where F: FnMut(&SemanticScope) {
        for scope in &self.context.previous_scopes {
            if scope.is_location_inside(location) {
                f(scope);
            }
        }

        for scope in &self.context.scope {
            if scope.is_location_inside(location) {
                f(scope);
            }
        }
    }

    fn resolve_parameter_name<'this>(&'this mut self, function: &SemanticReference, arg_idx: usize) -> Option<Ranged<BabString>> {
        match &function.typ {
            SemanticType::Array(..) => todo!(),
            SemanticType::Builtin(..) => todo!(),
            SemanticType::Custom { .. } => todo!(),
            SemanticType::Function(func) => {
                Some(func.parameters[arg_idx].name.clone())
            }
            SemanticType::FunctionReference(FunctionReference::Builtin(..)) => {
                None
            }
            SemanticType::FunctionReference(FunctionReference::Custom(func)) => {
                Some(func.name.clone())
            }
            SemanticType::IndexReference(..) => todo!(),
            SemanticType::Interface { .. } => todo!(),
            SemanticType::Generic(..) => todo!(),
            SemanticType::Pointer(..) => todo!(),
        }
    }

    fn resolve_parameter_type<'this>(&'this mut self, function: &SemanticReference, arg_idx: usize) -> Option<SemanticType> {
        Some(match &function.typ {
            SemanticType::Array(..) => todo!(),
            SemanticType::Builtin(..) => todo!(),
            SemanticType::Custom { .. } => todo!(),
            SemanticType::Function(func) => {
                func.parameters.get(arg_idx)?.ty.value().clone()
            }
            SemanticType::FunctionReference(FunctionReference::Builtin(func)) => {
                SemanticType::Builtin(func.parameters.get(arg_idx)?.typ)
            }
            SemanticType::FunctionReference(FunctionReference::Custom(func)) => {
                func.parameters.get(arg_idx)?.ty.value().clone()
            }
            SemanticType::IndexReference(..) => todo!(),
            SemanticType::Interface { .. } => todo!(),
            SemanticType::Generic(..) => todo!(),
            SemanticType::Pointer(..) => todo!(),
        })
    }

    fn analyze_postfix_expression(&mut self, postfix: &PostfixExpression) -> SemanticValue {
        let lhs = self.analyze_expression(&postfix.lhs).ty;
        match postfix.kind.value() {
            PostfixExpressionKind::Call(call) => self.analyze_function_call_expression(lhs, call, postfix),
            PostfixExpressionKind::Member(member) => self.analyze_member_expression(lhs, member),
            PostfixExpressionKind::MethodCall(method) => self.analyze_method_expression(lhs, method),
            PostfixExpressionKind::Subscript(expr) => self.analyze_subscript_expression(lhs, &expr, postfix.kind.range()),
        }
    }

    fn analyze_member_expression(&mut self, typ: SemanticType, member: &Ranged<BabString>) -> SemanticValue {
        let SemanticType::Custom { base, .. } = &typ else {
            self.diagnostics.create(|| SemanticDiagnostic::new(
                member.range(),
                SemanticDiagnosticKind::InvalidMember { typ, name: member.value().clone() }
            ));

            return SemanticValue::null()
        };

        for field in &base.fields {
            if field.name.value() == member.value() {
                let ty = field.ty.clone().resolve_against(&typ);

                if let Some(tracker) = &mut self.context.definition_tracker {
                    tracker.insert(member.range(), SemanticReference {
                        local_name: member.value().clone(),
                        local_kind: SemanticLocalKind::FieldReference,
                        declaration_range: field.name.range(),
                        typ: ty.clone(),
                    });
                }

                return SemanticValue {
                    ty,
                    usage: SemanticUsage::Pure(PureValue::FieldReference {
                        declaration: field.name.range(),
                        name: field.name.value().clone(),
                    }),
                };
            }
        }

        let struct_hint = SemanticRelatedInformation::new(
            base.name.range(),
            SemanticRelatedMessage::StructureDefinedHere { name: base.name.value().clone() }
        );

        let actions = self.context.statements_state.last()
            .and_then(|x| x.assignment_type.as_ref())
            .map(|ty| {
                self.create_actions_create_field(&typ, &member, ty.clone())
            })
            .unwrap_or_default();

        let diag = SemanticDiagnostic::new(
            member.range(),
            SemanticDiagnosticKind::InvalidMember { typ, name: member.value().clone() }
        );

        self.diagnostics.create(|| diag.with_related(struct_hint).with_actions(actions));

        SemanticValue::null()
    }

    fn analyze_method_expression(&mut self, typ: SemanticType, expression: &MethodCallExpression) -> SemanticValue {
        match typ {
            SemanticType::Array(..) => {
                for method in Builtin::array().methods() {
                    if *expression.method_name == method.name {
                        return SemanticValue {
                            ty: method.return_type.resolve(typ),
                            usage: if method.must_use { SemanticUsage::Pure(PureValue::ReturnValue) } else { SemanticUsage::Indifferent },
                        };
                    }
                }

                self.diagnostics.create(|| SemanticDiagnostic::new(
                    expression.method_name.range(),
                    SemanticDiagnosticKind::InvalidMethod { typ, name: expression.method_name.value().clone()}
                ));

                SemanticValue::null()
            }

            SemanticType::Builtin(builtin) => {
                for method in builtin.methods() {
                    if *expression.method_name == method.name {
                        return SemanticValue {
                            ty: SemanticType::Builtin(method.return_type),
                            usage: if method.must_use { SemanticUsage::Pure(PureValue::ReturnValue) } else { SemanticUsage::Indifferent },
                        };
                    }
                }

                if let Some(value) = self.analyze_method_expression_with_extensions(&typ, expression) {
                    return value;
                }

                let args: Vec<SemanticType> = expression.call.arguments.iter().map(|x| self.analyze_expression(x).ty).collect();
                let create_method_extension_actions = self.create_actions_create_method_extension(&typ, expression, &args);

                self.diagnostics.create(|| SemanticDiagnostic::new(
                    expression.method_name.range(),
                    SemanticDiagnosticKind::InvalidMethod { typ, name: expression.method_name.value().clone()}
                ).with_actions(create_method_extension_actions));

                SemanticValue::null()
            }

            SemanticType::Custom { ref base, .. } => {
                for method in &base.methods {
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

                        self.analyze_function_parameters(method.name().clone(), local_reference, &expression.call, Some(&typ));

                        let ty = method.return_type().resolve_against(&typ);
                        if ty.is_null() {
                            return SemanticValue::null();
                        }

                        return SemanticValue {
                            ty,
                            usage: method.return_type_usage(),
                        };
                    }
                }

                if let Some(value) = self.analyze_method_expression_with_extensions(&typ, expression) {
                    return value;
                }

                let struct_hint = SemanticRelatedInformation::new(
                    base.name.range(),
                    SemanticRelatedMessage::StructureDefinedHere { name: base.name.value().clone() }
                );

                let args: Vec<SemanticType> = expression.call.arguments.iter().map(|x| self.analyze_expression(x).ty).collect();
                let create_method_action = self.create_action_create_method(base.as_ref(), expression, &args);
                let create_method_extension_actions = self.create_actions_create_method_extension(&typ, expression, &args);
                let diag = SemanticDiagnostic::new(
                    expression.method_name.range(),
                    SemanticDiagnosticKind::InvalidMethod { typ, name: expression.method_name.value().clone() }
                ).with_action(create_method_action).with_actions(create_method_extension_actions);

                self.diagnostics.create(|| diag.with_related(struct_hint));

                SemanticValue::null()
            }

            SemanticType::Function(..) | SemanticType::FunctionReference(..) => {
                self.diagnostics.create(|| SemanticDiagnostic::new(
                    expression.method_name.range(),
                    SemanticDiagnosticKind::FunctionCannotHaveMethod { typ, name: expression.method_name.value().clone() }
                ));

                // default SemanticUsage of functions should be default-must_use
                SemanticValue::null()
            }

            SemanticType::IndexReference(ty) => {
                self.analyze_method_expression(ty.as_ref().clone(), expression)
            }

            SemanticType::Interface { ref base, .. } => {
                for method in &base.methods {
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

                        self.analyze_function_parameters(method.name().clone(), local_reference, &expression.call, Some(&typ));

                        let ty = method.return_type().resolve_against(&typ);
                        if ty.is_null() {
                            return SemanticValue::null();
                        }

                        return SemanticValue {
                            ty,
                            usage: method.return_type_usage(),
                        };
                    }
                }

                if let Some(value) = self.analyze_method_expression_with_extensions(&typ, expression) {
                    return value;
                }

                let struct_hint = SemanticRelatedInformation::new(
                    base.name.range(),
                    SemanticRelatedMessage::StructureDefinedHere { name: base.name.value().clone() }
                );

                let args: Vec<SemanticType> = expression.call.arguments.iter().map(|x| self.analyze_expression(x).ty).collect();
                let create_method_action = self.create_action_create_method(base.as_ref(), expression, &args);
                let create_method_extension_actions = self.create_actions_create_method_extension(&typ, expression, &args);
                let diag = SemanticDiagnostic::new(
                    expression.method_name.range(),
                    SemanticDiagnosticKind::InvalidMethod { typ, name: expression.method_name.value().clone() }
                ).with_action(create_method_action).with_actions(create_method_extension_actions);

                self.diagnostics.create(|| diag.with_related(struct_hint));

                SemanticValue::null()
            }

            SemanticType::Generic(..) => todo!(),

            SemanticType::Pointer(ref element_type) => {
                for method in Builtin::pointer().methods() {
                    if *expression.method_name == method.name {
                        return SemanticValue {
                            ty: method.return_type.resolve(element_type.as_ref().clone()),
                            usage: if method.must_use { SemanticUsage::Pure(PureValue::ReturnValue) } else { SemanticUsage::Indifferent },
                        };
                    }
                }

                self.diagnostics.create(|| SemanticDiagnostic::new(
                    expression.method_name.range(),
                    SemanticDiagnosticKind::InvalidMethod { typ, name: expression.method_name.value().clone()}
                ));

                SemanticValue::null()
            }
        }
    }

    fn analyze_method_expression_with_extensions(&mut self, typ: &SemanticType, expression: &MethodCallExpression) -> Option<SemanticValue> {
        for extension in self.context.scope.iter().rev().flat_map(|x| &x.extensions) {
            if !extension.is_for_type(typ) {
                continue;
            }

            let Some(method) = extension.methods.get(&expression.method_name) else {
                continue;
            };

            let local_reference = SemanticReference {
                local_name: method.name().clone(),
                local_kind: SemanticLocalKind::Method,
                declaration_range: method.function.name.range(),
                typ: SemanticType::FunctionReference(FunctionReference::Custom(method.function.clone())), // is this okay?
            };

            if let Some(tracker) = &mut self.context.definition_tracker {
                tracker.insert(expression.method_name.range(), local_reference.clone());
            }

            let return_type = method.return_type().resolve_against(typ);
            let usage = method.return_type_usage();

            self.analyze_function_parameters(method.name().clone(), local_reference, &expression.call, Some(&typ));

            if return_type.is_null() {
                return Some(SemanticValue::null());
            }

            return Some(SemanticValue {
                ty: return_type,
                usage,
            });
        }

        None
    }

    fn analyze_attributes_for_statement(&mut self, statement: &Statement) {
        for attribute in &statement.attributes {
            if attribute.name.value() == Attribute::NAME_EXTERN {
                self.analyze_attribute_extern(statement, attribute);
                continue;
            }

            self.diagnostics.create(|| SemanticDiagnostic::new(
                attribute.name.range(),
                SemanticDiagnosticKind::UnknownAttribute { name: attribute.name.value().clone(), range: attribute.range() },
            ));
        }
    }

    fn analyze_attributes_for_parameter(&mut self, function: &FunctionStatement, parameter: &Parameter) {
        for attribute in &parameter.attributes {
            match attribute.name.as_str() {
                _ => {
                    _ = function;

                    self.diagnostics.create(|| SemanticDiagnostic::new(
                        attribute.name.range(),
                        SemanticDiagnosticKind::UnknownAttribute { name: attribute.name.value().clone(), range: attribute.range() },
                    ));
                }
            }
        }
    }

    fn analyze_attribute_extern(&mut self, statement: &Statement, attr: &Attribute) {
        let StatementKind::Function(function) = &statement.kind else {
            let diag = SemanticDiagnostic::new(
                attr.name.range().as_full_line(),
                SemanticDiagnosticKind::AttributeExternOnlyOnFunctions,
            );
            let diag = diag.with_action(BabbelaarCodeAction::new(
                BabbelaarCodeActionType::RemoveAttribute { name: attr.name.value().clone() },
                vec![FileEdit::new(attr.name.range().as_full_line(), "")],
            ));
            self.diagnostics.create(|| diag);
            return;
        };

        if function.body.is_some() {
            let diag = SemanticDiagnostic::new(
                attr.name.range().as_full_line(),
                SemanticDiagnosticKind::AttributeExternOnlyOnFunctionsWithoutBody,
            );
            self.diagnostics.create(|| diag);
        }

        let Some(extern_func) = self.attribute_extern_evaluate(attr) else {
            return;
        };

        let Some(func) = self.context.current().get_function_mut(&function.name) else {
            log::warn!("Expected function '{}' to be defined earlier", function.name.value());
            return;
        };

        if func.extern_function.is_some() {
            let diag = SemanticDiagnostic::new(
                attr.name.range().as_full_line(),
                SemanticDiagnosticKind::AttributeExternOnlyOnce,
            );
            let diag = diag.with_action(BabbelaarCodeAction::new(
                BabbelaarCodeActionType::RemoveAttribute { name: attr.name.value().clone() },
                vec![FileEdit::new(attr.name.range().as_full_line(), "")],
            ));
            self.diagnostics.create(|| diag);
            return;
        }

        func.extern_function = Some(extern_func);
    }

    fn attribute_extern_evaluate(&mut self, attr: &Attribute) -> Option<SemanticExternFunction> {
        let mut name = None;

        for arg in attr.arguments.value() {
            if arg.name.value() == "naam" {
                if name.is_none() {
                    name = Some(&arg.value);
                } else {
                    self.diagnostics.create(|| SemanticDiagnostic::new(
                        arg.name.range(),
                        SemanticDiagnosticKind::AttributeExternDuplicateName,
                    ));
                }
            } else {
                self.diagnostics.create(|| SemanticDiagnostic::new(
                    arg.name.range(),
                    SemanticDiagnosticKind::AttributeExternInvalidArgument,
                ));
            }
        }

        let Some(name) = name else {
            let diag = SemanticDiagnostic::new(
                attr.name.range(),
                SemanticDiagnosticKind::AttributeExternRequiresName,
            );
            // TODO add action
            self.diagnostics.create(|| diag);
            return None;
        };

        let PrimaryExpression::StringLiteral(name_literal) = name.value() else {
            let diag = SemanticDiagnostic::new(
                attr.name.range(),
                SemanticDiagnosticKind::AttributeExternNameMustBeString,
            );
            self.diagnostics.create(|| diag);
            return None;
        };

        if name_literal.is_empty() {
            self.diagnostics.create(|| SemanticDiagnostic::new(
                attr.name.range(),
                SemanticDiagnosticKind::AttributeExternNameMustBeNonEmpty,
            ));
            return None;
        }

        Some(SemanticExternFunction {
            name: name_literal.clone(),
        })
    }

    fn analyze_attributes_for_field(&mut self, field: &SemanticField) {
        for attribute in &field.attributes {
            self.diagnostics.create(|| SemanticDiagnostic::new(
                attribute.name.range(),
                SemanticDiagnosticKind::UnknownAttribute { name: attribute.name.value().clone(), range: attribute.range() },
            ));
        }
    }

    fn try_create_conversion_actions(
        &self,
        expected_type: &SemanticType,
        actual_type: &SemanticType,
        expression: &Ranged<Expression>,
    ) -> Vec<BabbelaarCodeAction> {
        let mut actions = Vec::new();

        if *expected_type == BuiltinType::G32 && *actual_type == BuiltinType::Slinger {
            if let Expression::Primary(PrimaryExpression::StringLiteral(literal)) = expression.value().clone(){
                if let Ok(value) = literal.trim().parse::<isize>() {
                    actions.push(BabbelaarCodeAction::new(
                        BabbelaarCodeActionType::ChangeStringToNumber { number: value, },
                        vec![
                            FileEdit::new(
                                expression.range(),
                                value.to_string(),
                            )
                        ]
                    ));
                }
            }

            actions.push(BabbelaarCodeAction::new(
                BabbelaarCodeActionType::UseMethod { method_name: BabString::new_static("lengte") },
                vec![
                    FileEdit::new(
                        expression.range().end().as_zero_range(),
                        ".lengte()",
                    )
                ]
            ));
        }

        actions
    }

    #[must_use]
    fn create_actions_create_field(&self, structure: &SemanticType, name: &str, ty: SemanticType) -> Vec<BabbelaarCodeAction> {
        let SemanticType::Custom { base: structure, parameters: generic_parameters, .. } = structure else {
            return Vec::new();
        };

        let (add_location, indent) = self.calculate_new_field_or_method_location(structure.as_ref());

        let mut items = Vec::new();

        for (generic_index,generic_parameter_type) in generic_parameters.iter().enumerate() {
            if generic_parameter_type == &ty {
                let ty = structure.generic_types[generic_index].value().clone();
                let add_text = format!("\n{indent}veld {name}: {ty},");

                items.push(BabbelaarCodeAction::new(
                    BabbelaarCodeActionType::CreateFieldGeneric { name: name.to_string(), ty },
                    vec![FileEdit::new(add_location.as_zero_range(), add_text)]
                ));
            }
        }

        let add_text = format!("\n{indent}veld {name}: {ty},");
        items.push(BabbelaarCodeAction::new(
            BabbelaarCodeActionType::CreateField { name: name.to_string(), ty: ty.to_string().into() },
            vec![FileEdit::new(add_location.as_zero_range(), add_text)]
        ));

        items
    }

    #[must_use]
    fn create_action_create_method(&self, object: &dyn StructureOrInterface, method: &MethodCallExpression, args: &[SemanticType]) -> BabbelaarCodeAction {
        let name = method.method_name.value().clone();
        let (add_location, indent) = self.calculate_new_field_or_method_location(object);

        let mut add_text = format!("\n\n{indent}werkwijze {name}(");

        for (idx, typ) in args.iter().enumerate() {
            if idx != 0 {
                add_text += ", ";
            }

            add_text += &format!("param{idx}: {typ}");
        }

        add_text += ") {\n\n";
        add_text += &indent;
        add_text += "}";

        BabbelaarCodeAction::new(
            BabbelaarCodeActionType::CreateMethod {
                name,
                structure: object.name().value().clone(),
            },
            vec![FileEdit::new(add_location.as_zero_range(), add_text)]
        )
    }

    #[must_use]
    fn create_actions_create_method_extension(&self, typ: &SemanticType, method: &MethodCallExpression, args: &[SemanticType]) -> Vec<BabbelaarCodeAction> {
        let add_to_existing = self.create_actions_create_method_extension_existing_structure(typ, method, args);

        let is_explicitly_new = add_to_existing.is_some();
        let new = self.create_actions_create_method_extension_new_structure(typ, method, args, is_explicitly_new);

        [add_to_existing, Some(new)].into_iter().flatten().collect()
    }

    #[must_use]
    fn create_actions_create_method_extension_new_structure(&self, typ: &SemanticType, method: &MethodCallExpression, args: &[SemanticType], is_explicitly_new: bool) -> BabbelaarCodeAction {
        let name = method.method_name.value().clone();
        let location = self.calculate_new_extension_location(method.method_name.range().file_id());
        let indent = self.indentation_at(location).unwrap_or_default();

        let mut add_text = format!("\n\n{indent}uitbreiding {typ} {{\n{indent}    werkwijze {name}(");

        for (idx, typ) in args.iter().enumerate() {
            if idx != 0 {
                add_text += ", ";
            }

            add_text += &format!("param{idx}: {typ}");
        }

        add_text += ") {\n\n";
        add_text += &indent;
        add_text += "    }\n";
        add_text += &indent;
        add_text += "}";

        BabbelaarCodeAction::new(
            BabbelaarCodeActionType::CreateMethodExtension {
                name,
                structure: typ.to_string().into(),
                is_explicitly_new,
            },
            vec![FileEdit::new(location.as_zero_range(), add_text)]
        )
    }

    #[must_use]
    fn create_actions_extend_structure_with_interface_by_name(&self, structure: &SemanticStructure, file_id: FileId, name: &'static str) -> Option<BabbelaarCodeAction> {
        let interface = self.resolve_interface_by_name(&BabString::new_static(name))?;
        Some(self.create_actions_extend_structure_with_interface(structure, file_id, &interface))
    }

    #[must_use]
    fn create_actions_extend_structure_with_interface(&self, structure: &SemanticStructure, file_id: FileId, interface: &SemanticInterface) -> BabbelaarCodeAction {
        let location = self.calculate_new_extension_location(file_id);
        let indent = self.indentation_at(location).unwrap_or_default();

        let mut generics = structure.generic_types.iter().map(|x| x.as_str())
            .chain(interface.generic_types.iter().map(|x| x.as_str()))
            .collect::<HashSet<&str>>()
            .into_iter()
            .join(", ");

        if !generics.is_empty() {
            generics = format!("<{generics}>");
        }

        let mut add_text = format!(
            "{new_line}{indent}uitbreiding{generics} {interface_name}{generics} op {structure_name}{generics} {{\n",
            new_line = if location.line() == 0 { "" } else { "\n" },
            interface_name = interface.name.value(),
            structure_name = structure.name.value(),
        );

        for (idx, method) in interface.methods.iter().enumerate() {
            if idx != 0 {
                add_text += "\n";
            }

            add_text += &indent;
            add_text += "    werkwijze ";
            add_text += method.name();
            add_text += "(";

            for (idx, param) in method.function.parameters.iter().enumerate() {
                if idx != 0 {
                    add_text += ", ";
                }

                add_text += &format!("{}: {}", param.name.value(), param.ty.value());
            }

            add_text += ")";

            let ret = method.return_type();
            if !ret.is_null() {
                write!(&mut add_text, " -> {ret}").unwrap();
            }

            add_text += " {\n\n";
            add_text += &indent;
            add_text += "    }\n";
        }

        add_text += &indent;
        add_text += "}\n";

        BabbelaarCodeAction::new(
            BabbelaarCodeActionType::ExtendStructureWithInterface {
                structure: structure.name.value().clone(),
                interface: interface.name.value().clone(),
            },
            vec![FileEdit::new(location.as_zero_range(), add_text)]
        )
    }

    #[must_use]
    fn create_actions_create_method_extension_existing_structure(&self, typ: &SemanticType, method: &MethodCallExpression, args: &[SemanticType]) -> Option<BabbelaarCodeAction> {
        let extension = self.find_extension_block_for(typ)?;

        let location = extension.methods.values().map(|x| x.range.end()).max().unwrap_or(extension.right_curly_bracket.start());

        let indent;
        let new_lines_at_start;
        let new_lines_at_end;
        if extension.methods.is_empty() {
            new_lines_at_start = "";
            new_lines_at_end = "\n\n";
            indent = self.indentation_at(location).unwrap_or_default().to_string();
        } else {
            new_lines_at_start = "\n\n";
            new_lines_at_end = "";
            indent = String::new();
        }

        let name = method.method_name.value().clone();

        let mut add_text = format!("{new_lines_at_start}{indent}    werkwijze {name}(");

        for (idx, typ) in args.iter().enumerate() {
            if idx != 0 {
                add_text += ", ";
            }

            add_text += &format!("param{idx}: {typ}");
        }

        add_text += ") {\n\n";
        add_text += &indent;
        add_text += "    }";
        add_text += new_lines_at_end;

        Some(BabbelaarCodeAction::new(
            BabbelaarCodeActionType::CreateMethodExtension {
                name,
                structure: typ.to_string().into(),
                is_explicitly_new: false,
            },
            vec![FileEdit::new(location.as_zero_range(), add_text)]
        ))
    }

    fn find_extension_block_for(&self, typ: &SemanticType) -> Option<&SemanticExtension> {
        for scope in self.context.scope.iter().rev() {
            for extension in scope.extensions.iter().rev() {
                if extension.ty == *typ {
                    return Some(extension);
                }
            }
        }

        None
    }

    pub fn indentation_at(&self, start: FileLocation) -> Option<&str> {
        self.files.get(&start.file_id())?.indentation_at(start)
    }

    /// The parameter `file_id` is to ensure we can always provide some location,
    /// even if it's not the most optimal one.
    #[must_use]
    fn calculate_new_extension_location(&self, file_id: FileId) -> FileLocation {
        let mut last_structure = None;
        let mut last_extension = None;

        for scope in self.context.scope.iter().rev() {
            if let Some(ext) = scope.extensions.last() {
                if ext.range.file_id() == file_id {
                    last_extension = Some(ext.range);
                }
            }

            if let Some(structure) = scope.structures.values().map(|x| x.right_curly_range).max() {
                if structure.file_id() == file_id {
                    last_structure = Some(structure);
                }
            }
        }

        if let Some(range) = last_extension {
            return range.end();
        }

        if let Some(range) = last_structure {
            return range.end();
        }

        FileLocation::new(file_id, 0, 0, 0)
    }

    fn calculate_new_field_or_method_location(&self, object: &dyn StructureOrInterface) -> (FileLocation, String) {
        if let Some(last) = object.fields().last() {
            let indent = self.indentation_at(last.name.range().start()).unwrap_or_default();
            let location = FileLocation::new(object.name().range().file_id(), 0, last.name.range().start().line(), usize::MAX);

            (location, format!("{indent}"))
        } else {
            let indent = self.indentation_at(object.name().range().start()).unwrap_or_default();

            (object.left_curly_range().end(), format!("{indent}    "))
        }
    }

    fn create_action_create_function(&mut self, function_name: BabString, expression: &FunctionCallExpression) -> Option<BabbelaarCodeAction> {
        let location = self.find_function_insertion_location()?;

        let mut text = format!("\n\nwerkwijze {function_name}(");
        let mut names_used = HashSet::new();

        for (idx, arg) in expression.arguments.iter().enumerate() {
            if idx != 0 {
                text += ", ";
            }

            let ty = self.analyze_expression(arg).ty;

            let name = match self.find_canonical_name_for_variable(arg.value()) {
                Some(name) => {
                    name
                }

                None => {
                    BabString::new(ty.value_or_field_name_hint().to_lowercase())
                }
            };

            text += &name;

            if !names_used.insert(name) {
                text += &format!("{idx}: {ty}");
            } else {
                text += &format!(": {ty}");
            }
        }

        text += ") {\n\n}";

        let edit = FileEdit::new(location.as_zero_range(), text);

        Some(BabbelaarCodeAction::new(
            BabbelaarCodeActionType::CreateFunction { name: function_name },
            vec![edit]
        ))
    }

    fn find_function_insertion_location(&self) -> Option<FileLocation> {
        let mut location = self.context.current_scope().range.end();

        for scope in &self.context.scope {
            location = scope.range.end();
            if !matches!(scope.kind, SemanticScopeKind::Default) {
                break;
            }
        }

        if location.file_id() != FileId::INTERNAL {
            Some(location)
        } else {
            None
        }
    }

    fn find_canonical_name_for_variable(&self, value: &Expression) -> Option<BabString> {
        match value {
            Expression::Primary(PrimaryExpression::CharacterLiteral(..)) => None,
            Expression::Primary(PrimaryExpression::Boolean(..)) => None,
            Expression::Primary(PrimaryExpression::IntegerLiteral(..)) => Some(BabString::new_static("getal")),
            Expression::Primary(PrimaryExpression::Parenthesized(expr)) => self.find_canonical_name_for_variable(expr.value()),
            Expression::Primary(PrimaryExpression::Reference(reference)) => Some(reference.value().clone()),
            Expression::Primary(PrimaryExpression::ReferenceThis) => {
                Some(self.context.current_scope().this.as_ref()?.name().to_lowercase().into())
            }
            Expression::Primary(PrimaryExpression::StringLiteral(..)) => None,
            Expression::Primary(PrimaryExpression::StructureInstantiation(structure)) => {
                Some(structure.name.value().clone())
            }
            Expression::Primary(PrimaryExpression::TemplateString { .. }) => None,
            Expression::Primary(PrimaryExpression::SizedArrayInitializer { .. }) => None,
            Expression::Unary(expr) => self.find_canonical_name_for_variable(&expr.rhs),
            Expression::Postfix(..) => None, // TODO
            Expression::BiExpression(..) => None, // TODO
        }
    }

    fn analyze_usages(&mut self) {
        for scope in self.context.scope.iter().chain(self.context.previous_scopes.iter()) {
            for (name, local) in &scope.locals {
                if local.usage_count != 0 {
                    continue;
                }

                if local.name_declaration_range.file_id() == FileId::INTERNAL {
                    continue;
                }

                if name.starts_with('_') {
                    // TODO: make this configurable
                    continue;
                }

                let diag = self.create_diagnostic_unused_local(name, local);
                self.diagnostics.create(|| diag);
            }
        }
    }

    fn create_diagnostic_unused_local(&self, name: &BabString, local: &SemanticLocal) -> SemanticDiagnostic {
        let kind = match &local.kind {
            SemanticLocalKind::Function | SemanticLocalKind::FunctionReference =>
                SemanticDiagnosticKind::UnusedFunction { name: name.clone() },
            SemanticLocalKind::Iterator => SemanticDiagnosticKind::UnusedIterator { name: name.clone() },
            SemanticLocalKind::Parameter => SemanticDiagnosticKind::UnusedParameter { name: name.clone() },
            SemanticLocalKind::Variable => SemanticDiagnosticKind::UnusedVariable { name: name.clone() },

            _ => SemanticDiagnosticKind::UnusedVariable { name: name.clone() } // TODO
        };
        let mut diag = SemanticDiagnostic::new(local.name_declaration_range, kind)
            .warn()
            .with_action(BabbelaarCodeAction::new(
                BabbelaarCodeActionType::AppendUnderscoreToName {
                    name: name.clone()
                },
                vec![
                    FileEdit::new(
                        local.name_declaration_range.start().as_zero_range(),
                        '_'
                    )
                ]
            ));

        if let Some(range) = local.full_declaration_range {
            diag = diag.with_action(BabbelaarCodeAction::new(
                BabbelaarCodeActionType::RemoveGenericStatement {
                    kind: local.kind.name(),
                    name: name.clone(),
                },
                vec![
                    FileEdit::new(range, String::new())
                ]
            ));
        }

        diag
    }

    fn analyze_subscript_expression(&mut self, lhs: SemanticType, expression: &Ranged<Expression>, range: FileRange) -> SemanticValue {
        let Some(element_type) = lhs.subscript() else {
            self.diagnostics.create(||
                SemanticDiagnostic::new(range, SemanticDiagnosticKind::CannotSubscriptNonArray { ty: lhs })
            );
            return SemanticValue::null();
        };

        let index_value = self.analyze_expression(expression);
        if index_value.ty != SemanticType::Builtin(BuiltinType::G32) {
            self.diagnostics.create(||
                SemanticDiagnostic::new(range, SemanticDiagnosticKind::CannotIndexArrayWithNonInteger { ty: index_value.ty })
            );
        }

        SemanticValue {
            ty: element_type,
            usage: SemanticUsage::Pure(PureValue::IndexReference)
        }
    }

    fn analyze_assignment_source_dest(&mut self, assign: &AssignStatement, destination_type: SemanticType, source_type: SemanticType) {
        if source_type != destination_type {
            self.emit_diagnostic(|this|
                SemanticDiagnostic::new(assign.equals_sign, SemanticDiagnosticKind::IncompatibleAssignmentTypes)
                    .with_related(SemanticRelatedInformation::new(assign.destination.range(), SemanticRelatedMessage::DestinationOfType { ty: destination_type.clone() }))
                    .with_related(SemanticRelatedInformation::new(assign.source.range(), SemanticRelatedMessage::SourceOfType { ty: source_type.clone() }))
                    .with_actions(this.try_create_conversion_actions(&destination_type, &source_type, &assign.source))
            );
        }
    }

    fn create_actions_create_structure(&self, name: &Ranged<BabString>, body: String) -> Vec<BabbelaarCodeAction> {
        let file_id = name.range().file_id();
        let name = name.value();

        let mut actions = Vec::new();
        let new_text = format!("structuur {name} {{\n{body}\n}}");

        if let Some(location) = self.find_function_insertion_location() {
            let edit = FileEdit::new(location.as_zero_range(), format!("\n\n{new_text}"));

            actions.push(BabbelaarCodeAction::new(
                BabbelaarCodeActionType::CreateStructure { name: name.clone() },
                vec![edit]
            ));
        }

        if let Some(current_file) = self.files.get(&file_id) {
            let mut path = current_file.path().to_path_buf();
            path.pop();
            path.push(format!("{name}.bab"));

            let new_file_id = FileId::from_path(&path);
            let location = FileLocation::new(new_file_id, 0, 0, 0);
            let range = FileRange::new(location, location);

            let edit = FileEdit::new(range, new_text).with_new_file(path);
            actions.push(BabbelaarCodeAction::new(
                BabbelaarCodeActionType::CreateStructureInNewFile { name: name.clone() },
                vec![edit]
            ));
        }

        actions
    }

    fn build_structure_body_based_on_instantiation(&mut self, instantiation: Option<&StructureInstantiationExpression>) -> String {
        let Some(instantiation) = instantiation else {
            log::warn!("Kan structuurlichaam op dit moment alleen bepalen vanuit een `nieuw`-expressie");
            return String::new();
        };

        let mut body = String::new();

        for (idx, field) in instantiation.fields.iter().enumerate() {
            if idx != 0 {
                body += "\n";
            }

            body += "    veld ";
            body += &field.name;
            body += ": ";
            body += &self.analyze_expression(&field.value).ty.name();
            body += ",";
        }

        body
    }

    #[inline]
    fn emit_diagnostic<F: FnOnce(&mut Self) -> SemanticDiagnostic>(&mut self, f: F) {
        if !self.should_produce_diagnostics {
            return;
        }

        let diagnostic = f(self);
        self.diagnostics.create(|| diagnostic);
    }

    #[must_use]
    fn get_interface_implementation_for(&self, typ: &SemanticType, interface: &SemanticInterface) -> Option<&SemanticExtension> {
        self.context.scope
            .iter()
            .rev()
            .flat_map(|scope| scope.extensions.iter())
            .filter(|extension| extension.is_for_type(typ))
            .filter(|extension| extension.interface.as_ref().is_some_and(|i| i.as_ref() == interface))
            .next()
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

    #[must_use]
    fn with_action(mut self, action: impl Into<Option<BabbelaarCodeAction>>) -> Self {
        if let Some(action) = action.into() {
            self.actions.push(action);
        }

        self
    }

    #[must_use]
    fn with_actions(mut self, action: impl AsRef<[BabbelaarCodeAction]>) -> Self {
        self.actions.extend_from_slice(action.as_ref());
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
    #[error("Bestemming is van type `{ty}`")]
    DestinationOfType { ty: SemanticType },

    #[error("`{name}` is hier voor het eerst geïnitialiseerd")]
    DuplicateFieldFirstUse { name: BabString },

    #[error("werkwijze `{name}` is hier voor het eerst aangemaakt")]
    DuplicateMethodFirstDefinedHere { name: BabString },

    #[error("expressie is van het type `{ty}`")]
    ExpressionIsOfType { ty: SemanticType },

    #[error("veld `{name}` is hier gedefinieerd")]
    FieldDefinedHere { name: BabString },

    #[error("werkwijze `{name}` is hier gedefinieerd")]
    FunctionDefinedHere { name: BabString },

    #[error("koppelvlak `{name}` is hier gedefinieerd")]
    InterfaceDefinedHere { name: BabString },

    #[error("parameter `{name}` is hier gedeclareerd")]
    ParameterDeclaredHere { name: BabString },

    #[error("structuur `{name}` is hier gedefinieerd")]
    StructureDefinedHere { name: BabString },

    #[error("werkwijze zit niet in een structuur")]
    WerkwijzeNotInsideStructuur,

    #[error("los gebruik van het veld `{name}` heeft geen effect")]
    UsageOfPureValueField { name: BabString },

    #[error("bekeertype `{typ}` is hier gedefinieerd")]
    ReturnTypeDefinedHere { typ: BabString },

    #[error("Bronwaarde is van type `{ty}`")]
    SourceOfType { ty: SemanticType },

    #[error("type `{typ}` is hier gedefinieerd")]
    TypeDefinedHere { typ: BabString },
}

#[derive(Debug, Clone, Error, AsRefStr)]
#[strum(serialize_all = "kebab-case")]
pub enum SemanticDiagnosticKind {
    #[error("Expressie is geen geldige toewijzing: `{expression:?}`")]
    ExpressionCannotBeUsedAsAssignmentDestination {
        expression: Expression,
    },

    #[error("Werkwijze `{name}` bestaat niet.")]
    InvalidFunctionReference { name: BabString },

    #[error("Kon waarde `{identifier}` niet vinden binnen deze scoop.")]
    InvalidIdentifierReference { identifier: BabString },

    #[error("`_` kan alleen gebruikt worden om een bepaalde waarde weg te gooien.")]
    DiscardingIdentifierUsedAsReference,

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
    UnknownAttribute { name: BabString, range: FileRange },

    #[error("Veldnaam `{name}` wordt meerdere keren gebruikt")]
    DuplicateFieldName { name: BabString },

    #[error("Werkwijzenaam `{name}` in koppelvlak `{interface}` wordt meerdere keren gebruikt")]
    DuplicateMethodNameInInterface { name: BabString, interface: BabString },

    #[error("Werkwijzenaam `{name}` in structuur `{structure}` wordt meerdere keren gebruikt")]
    DuplicateMethodNameInStructure { name: BabString, structure: BabString },

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

    #[error("Pure waarde van type `{ty}` ongebruikt. Stelling heeft geen gevolg.")]
    UnusedPureValue { ty: BabString },

    #[error("`dit` kan uitsluitend gebruikt worden binnen een werkwijze van een `structuur`")]
    ThisOutsideStructure,

    #[error("{field_word} `{names}` {verb} een toewijzing")]
    MissingFieldInitializers {
        names: String,
        field_word: &'static str,
        verb: &'static str,
    },

    #[error("De naam van de uitheemse werkwijze is meerdere keren geven.")]
    AttributeExternDuplicateName,

    #[error("Onbekende argument voor het @uitheems-attribuut.")]
    AttributeExternInvalidArgument,

    #[error("Een uitheemse werkwijze vereist een `naam` argument.")]
    AttributeExternRequiresName,

    #[error("Het attribuut `@uitheems` kan alleen gebruikt worden op werkwijzen.")]
    AttributeExternOnlyOnFunctions,

    #[error("Het attribuut `@uitheems` kan alleen gebruikt worden op werkwijzen zonder lichaam {{ .. }}")]
    AttributeExternOnlyOnFunctionsWithoutBody,

    #[error("De naam van werkwijzeattribuut `@uitheems` moet een slinger zijn.")]
    AttributeExternNameMustBeString,

    #[error("De naam van werkwijzeattribuut `@uitheems` moet een niet-lege slinger zijn.")]
    AttributeExternNameMustBeNonEmpty,

    #[error("Onbekend argument `{name}` is niet toegestaan binnen attribuut `@uitheems`")]
    AttributeExternUnexpectedArgument { name: BabString },

    #[error("Attribuut `@uitheems` kan maar één keer gebruikt worden per werkwijzen.")]
    AttributeExternOnlyOnce,

    #[error("De werkwijze genaamd `{name}` is meerdere keren gedefinieerd.")]
    DuplicateFunction { name: BabString },

    #[error("`bekeer` verwacht een waarde van type `{typ}`")]
    ReturnStatementExpectedValue { typ: BabString },

    #[error("`bekeer` verwacht geen waarde")]
    ReturnStatementExpectedNoValue,

    #[error("`bekeer` verwachtte een waarde van type `{expected}`, maar expressie is van type `{actual}`")]
    ReturnStatementIncompatibleTypes {
        expected: SemanticType,
        actual: SemanticType,
    },

    #[error("Parameternaam `{name}` wordt meerdere keren gedefinieerd.")]
    DuplicateParameterName {
        name: BabString,
    },

    #[error("Werkwijze `{name}` wordt nergens gebruikt.")]
    UnusedFunction { name: BabString },

    #[error("Iterator `{name}` wordt nergens gebruikt.")]
    UnusedIterator { name: BabString },

    #[error("Parameter `{name}` wordt nergens gebruikt.")]
    UnusedParameter { name: BabString },

    #[error("Variabele `{name}` wordt nergens gebruikt.")]
    UnusedVariable { name: BabString },

    #[error("Expressie resulteert niet in een getal, wat nodig is om de grootte van de opeenvolging te bepalen.")]
    SizedArrayInitializerInvalidSize,

    #[error("Een `{ty}` kan niet worden geïndexeerd. Types zoals `Slinger` en opeenvolgingen wel.")]
    CannotSubscriptNonArray { ty: SemanticType },

    #[error("Je kunt opeenvolgingen alleen met getallen indexeren, maar de index is van type {ty}.")]
    CannotIndexArrayWithNonInteger { ty: SemanticType },

    #[error("Toewijzingsbron en -bestemming zijn niet van hetzelfde type.")]
    IncompatibleAssignmentTypes,

    #[error("{name} moet van het type `g32` zijn, maar dit is een `{ty}`")]
    RangeExpectsInteger { name: &'static str, ty: SemanticType },

    #[error("Kan deze waarde niet gebruiken als een doorloper, gebruik een opeenvolging of `reeks`.")]
    ExpressionNotIterable,

    #[error("Deze structuur is niet uitgebreid met het `Doorloper`-koppelvlak. Implementeer dit, gebruik een opeenvolging of `reeks`.")]
    ExpressionNotIterableStructure,

    #[error("Attribuut `@{name}` verwacht geen argumenten.")]
    AttributeCannotHaveArguments { name: &'static str },

    #[error("Attribuut `@{name}` kan alleen gebruikt worden op werkwijzen die `@uitheems` zijn.")]
    AttributeCanOnlyBeUsedOnExternFunctions { name: &'static str },

    #[error("Type `{name}` kan niet uitgebreid worden.")]
    TypeCannotBeExtended { name: BabString },

    #[error("Werkwijzenaam `{name}` bestaat al in structuur `{structure}`")]
    DuplicateMethodNameInExtension { name: BabString, structure: BabString },

    #[error("Bij type `{ty}` waren geen generieke typen verwacht")]
    TypeParametersUnexpected { ty: BabString },

    #[error("Te weinig generieke parameters meegegeven aan `{ty}`")]
    TooFewGenericTypes { ty: BabString },

    #[error("Te veel generieke parameters meegegeven aan `{ty}`")]
    TooManyGenericTypes { ty: BabString },

    #[error("Koppelvlak `{name}` is onbekend")]
    UnknownInterface { name: BabString },

    #[error("werkwijze{} {names} {}", if *count == 1 { "" } else { "n" }, if *count == 1 { "ontbreekt" } else { "ontbreken" })]
    MissingMethodsInInterfaceExtension { names: String, count: usize },

    #[error("Koppelvlak `{interface}` heeft geen werkwijze opgesteld met naam `{name}`")]
    InvalidInterfaceExtensionMethod { name: BabString, interface: BabString },

    #[error("Werkwijze in koppelvlak heeft parameter `{expected}`, maar deze is van type `{actual}`")]
    InterfaceDeclarationHasDifferentParameters { expected: String, actual: String },

    #[error("Werkwijze in koppelvlak heeft bekeertype `{expected}`, maar deze is van type `{actual}`")]
    InterfaceDeclarationHasDifferentReturnType { expected: SemanticType, actual: SemanticType },

    #[error("Werkwijze in koppelvlak heeft {expected} parameter{}, maar deze werkwijze heeft {actual} parameter{}", if *expected == 1 { "" } else { "s" }, if *actual == 1 { "" } else { "s" })]
    TooManyParametersForInterfaceMethod { expected: usize, actual: usize },

    #[error("Werkwijze in koppelvlak heeft {expected} parameter{}, maar deze werkwijze heeft {actual} parameter{}", if *expected == 1 { "" } else { "s" }, if *actual == 1 { "" } else { "s" })]
    TooFewParametersForInterfaceMethod { expected: usize, actual: usize },

    #[error("Kan alleen getallen negatief keren")]
    CannotNegateNonInteger,
}

impl SemanticDiagnosticKind {
    pub fn name(&self) -> &str {
        self.as_ref()
    }
}

#[derive(Default, Debug)]
struct StatementAnalysisState {
    assignment_type: Option<SemanticType>,
}

#[derive(Debug)]
pub struct SemanticContext {
    pub scope: Vec<SemanticScope>,
    pub previous_scopes: Vec<SemanticScope>,

    pub definition_tracker: Option<HashMap<FileRange, SemanticReference>>,
    pub declaration_tracker: Option<Vec<SemanticReference>>,
    pub value_type_tracker: Option<HashMap<FileRange, SemanticType>>,

    statements_state: Vec<StatementAnalysisState>,
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
            value_type_tracker: Some(HashMap::new()),
            statements_state: Vec::new(),
        }
    }

    #[must_use]
    pub fn current(&mut self) -> &mut SemanticScope {
        self.scope.last_mut().unwrap()
    }

    #[must_use]
    pub fn current_scope(&self) -> &SemanticScope {
        self.scope.last().unwrap()
    }

    pub fn announce_file(&mut self, tree: &ParseTree) {
        let location_end = tree.all()
            .map(|statement| statement.range.end())
            .max()
            .unwrap_or_else(|| FileLocation::new(FileId::from_path(tree.path()), 0, 0, 0));

        let location_start = FileLocation::new(location_end.file_id(), 0, 0, 0);

        self.scope[0].range = FileRange::new(location_start, location_end);
    }

    pub fn push_function_scope(&mut self, function: &FunctionStatement, this: Option<SemanticType>) -> &mut SemanticScope {
        self.scope.push(SemanticScope {
            range: function.range,
            locals: HashMap::new(),
            structures: HashMap::new(),
            generic_types: HashMap::new(),
            this,
            return_type: None,
            kind: SemanticScopeKind::Function {
                right_parameter_range: function.parameters_right_paren_range,
            },
            extensions: Vec::new(),
            interfaces: HashMap::new(),
        });
        self.scope.last_mut().expect("we just pushed a scope")
    }

    pub fn push_block_scope(&mut self, range: FileRange) -> &mut SemanticScope {
        let this = self.scope.last().and_then(|x| x.this.clone());
        let return_type = self.scope.last().and_then(|x| x.return_type.clone());
        self.scope.push(SemanticScope {
            range,
            locals: HashMap::new(),
            structures: HashMap::new(),
            generic_types: HashMap::new(),
            this,
            return_type,
            kind: SemanticScopeKind::Default,
            extensions: Vec::new(),
            interfaces: HashMap::new(),
        });
        self.scope.last_mut().expect("we just pushed a scope")
    }

    fn push_structure_scope(&mut self, structure: &Structure) {
        let this = self.scope.last().and_then(|x| x.this.clone());
        let return_type = self.scope.last().and_then(|x| x.return_type.clone());
        self.scope.push(SemanticScope {
            range: FileRange::new(structure.left_curly_range.start(), structure.right_curly_range.end()),
            locals: HashMap::new(),
            structures: HashMap::new(),
            generic_types: structure.generic_types
                .iter()
                .enumerate()
                .map(|(index, x)| {
                    let ty = SemanticGenericType {
                        index,
                        name: x.value().clone(),
                        declaration_range: x.range(),
                    };
                    (x.value().clone(), ty)
                })
                .collect(),
            this,
            return_type,
            kind: SemanticScopeKind::Structure,
            extensions: Vec::new(),
            interfaces: HashMap::new(),
        });
    }

    fn push_interface_scope(&mut self, interface: &InterfaceStatement) {
        let this = self.scope.last().and_then(|x| x.this.clone());
        let return_type = self.scope.last().and_then(|x| x.return_type.clone());
        self.scope.push(SemanticScope {
            range: FileRange::new(interface.left_curly_range.start(), interface.right_curly_range.end()),
            locals: HashMap::new(),
            structures: HashMap::new(),
            generic_types: interface.generic_types
                .iter()
                .enumerate()
                .map(|(index, x)| {
                    let ty = SemanticGenericType {
                        index,
                        name: x.value().clone(),
                        declaration_range: x.range(),
                    };
                    (x.value().clone(), ty)
                })
                .collect(),
            this,
            return_type,
            kind: SemanticScopeKind::Structure,
            extensions: Vec::new(),
            interfaces: HashMap::new(),
        });
    }

    fn push_extension_scope(&mut self, extension: &ExtensionStatement, range: FileRange) {
        let this = self.scope.last().and_then(|x| x.this.clone());
        let return_type = self.scope.last().and_then(|x| x.return_type.clone());
        self.scope.push(SemanticScope {
            range,
            locals: HashMap::new(),
            structures: HashMap::new(),
            generic_types: extension.generic_types
                .iter()
                .enumerate()
                .map(|(index, x)| {
                    let ty = SemanticGenericType {
                        index,
                        name: x.value().clone(),
                        declaration_range: x.range(),
                    };
                    (x.value().clone(), ty)
                })
                .collect(),
            this,
            return_type,
            kind: SemanticScopeKind::Structure,
            extensions: Vec::new(),
            interfaces: HashMap::new(),
        });
    }

    fn push_function(&mut self, function: SemanticFunction, range: FileRange) {
        let declaration_range = function.name.range();

        if let Some(tracker) = &mut self.declaration_tracker {
            tracker.push(SemanticReference {
                local_name: function.name.value().clone(),
                local_kind: SemanticLocalKind::Function,
                declaration_range,
                typ: SemanticType::Function(function.clone()),
            });
        }

        self.scope.last_mut().unwrap().locals.insert(
            function.name.value().clone(),
            SemanticLocal::new(
                SemanticLocalKind::Function,
                SemanticType::Function(function),
                declaration_range,
            ).with_declaration_range(range)
        );
    }

    fn push_structure(&mut self, structure: Arc<SemanticStructure>) {
        if let Some(tracker) = &mut self.declaration_tracker {
            tracker.push(SemanticReference {
                local_name: structure.name.value().clone(),
                local_kind: SemanticLocalKind::StructureReference,
                declaration_range: structure.name.range(),
                typ: SemanticType::Custom {
                    base: Arc::clone(&structure),
                    parameters: Vec::new(),
                },
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

        let previous_idx = self.scope.len() - 2;
        self.scope[previous_idx].structures.insert(structure.name.value().clone(), structure);
    }

    fn push_interface(&mut self, interface: Arc<SemanticInterface>) {
        if let Some(tracker) = &mut self.declaration_tracker {
            tracker.push(SemanticReference {
                local_name: interface.name.value().clone(),
                local_kind: SemanticLocalKind::StructureReference,
                declaration_range: interface.name.range(),
                typ: SemanticType::Interface {
                    base: Arc::clone(&interface),
                    parameters: Vec::new(),
                },
            });

            for method in &interface.methods {
                tracker.push(SemanticReference {
                    local_name: method.name().clone(),
                    local_kind: SemanticLocalKind::Method,
                    declaration_range: method.function.name.range(),
                    typ: SemanticType::FunctionReference(FunctionReference::Custom(method.function.clone())), // is this okay?
                });
            }
        }

        let previous_idx = self.scope.len() - 2;
        self.scope[previous_idx].interfaces.insert(interface.name.value().clone(), interface);
    }

    fn pop_scope(&mut self) {
        debug_assert!(self.scope.len() > 1);
        let scope = self.scope.pop().unwrap();

        self.previous_scopes.push(scope);
    }

    fn push_local(&mut self, name: &Ranged<BabString>, local: SemanticLocal) {
        if name.value() == &Constants::DISCARDING_IDENT {
            return;
        }

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

#[derive(Debug, Clone, PartialEq)]
pub struct SemanticGenericType {
    pub index: usize,
    pub name: BabString,
    pub declaration_range: FileRange,
}

impl Display for SemanticGenericType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.name.fmt(f)
    }
}

#[derive(Debug)]
pub struct SemanticInterface {
    pub attributes: AttributeList,
    pub name: Ranged<BabString>,
    pub generic_types: Vec<Ranged<BabString>>,
    pub left_curly_range: FileRange,
    pub right_curly_range: FileRange,
    pub methods: Vec<SemanticMethod>,
}

impl Display for SemanticInterface {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)
    }
}

impl PartialEq for SemanticInterface {
    fn eq(&self, other: &Self) -> bool {
        self.name.range() == other.name.range() && self.name.value() == other.name.value()
    }
}

#[derive(Debug)]
pub struct SemanticScope {
    pub range: FileRange,
    pub locals: HashMap<BabString, SemanticLocal>,
    pub structures: HashMap<BabString, Arc<SemanticStructure>>,
    pub generic_types: HashMap<BabString, SemanticGenericType>,
    pub this: Option<SemanticType>,
    pub return_type: Option<Ranged<SemanticType>>,
    pub kind: SemanticScopeKind,
    pub extensions: Vec<SemanticExtension>,
    pub interfaces: HashMap<BabString, Arc<SemanticInterface>>,
}

impl SemanticScope {
    fn new_top_level() -> Self {
        let mut this = Self {
            range: FileRange::new(
                FileLocation::new(FileId::INTERNAL, 0, 0, 0),
                FileLocation::new(FileId::INTERNAL, usize::MAX, usize::MAX, usize::MAX),
            ),
            structures: HashMap::new(),
            locals: HashMap::default(),
            this: None,
            return_type: None,
            generic_types: HashMap::new(),
            kind: SemanticScopeKind::TopLevel,
            extensions: Vec::new(),
            interfaces: HashMap::new(),
        };

        for func in Builtin::FUNCTIONS {
            this.locals.insert(BabString::new_static(func.name), SemanticLocal::new(
                SemanticLocalKind::Function,
                SemanticType::FunctionReference(FunctionReference::Builtin(func)),
                FileRange::INTERNAL,
            ));
        }

        this
    }

    #[must_use]
    pub fn is_location_inside(&self, location: FileLocation) -> bool {
        match &self.kind {
            SemanticScopeKind::TopLevel => true,
            _ => self.range.contains(location),
        }
    }

    #[must_use]
    pub fn get_function_mut(&mut self, name: &BabString) -> Option<&mut SemanticFunction> {
        let local = self.locals.get_mut(name)?;
        match &mut local.typ {
            SemanticType::Function(semantic_function) => Some(semantic_function),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SemanticExtension {
    pub ty: SemanticType,
    pub generic_types: HashSet<BabString>,
    pub interface: Option<Arc<SemanticInterface>>,
    pub methods: HashMap<BabString, SemanticMethod>,
    pub range: FileRange,
    pub right_curly_bracket: FileRange,
}

impl SemanticExtension {
    #[must_use]
    pub fn is_for_type(&self, typ: &SemanticType) -> bool {
        let SemanticType::Custom { base: req_base, parameters: req_params } = typ else {
            return &self.ty == typ;
        };

        let SemanticType::Custom { base: ext_base, parameters: ext_params } = &self.ty else {
            return &self.ty == typ;
        };

        if req_base != ext_base {
            // TODO: i think this makes things like Lijst<Lijst<T>> not work... we should do generic type erasure for `ext_base` too
            return false;
        }

        if req_params.len() != ext_params.len() {
            return false;
        }

        for (req_param, ext_param) in req_params.iter().zip(ext_params.iter()) {
            if req_param == ext_param {
                continue;
            }

            if self.generic_types.contains(&ext_param.name()) {
                continue;
            }

            log::warn!("Trace: typen {req_param} en {ext_param} komen niet overeen");
            return false;
        }

        true
    }
}

#[derive(Debug, Clone, Default)]
pub enum SemanticScopeKind {
    #[default]
    Default,

    TopLevel,
    Structure,
    Werkwijze,

    Function {
        right_parameter_range: FileRange,
    },
}

#[derive(Debug, Clone)]
pub struct SemanticParameter {
    pub name: Ranged<BabString>,
    pub ty: Ranged<SemanticType>,
}

#[derive(Debug, Clone)]
pub struct SemanticFunction {
    pub name: Ranged<BabString>,
    pub parameters: Vec<SemanticParameter>,
    pub parameters_right_paren_range: FileRange,
    pub extern_function: Option<SemanticExternFunction>,
    pub return_type: Box<SemanticType>,
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
    pub name_declaration_range: FileRange,
    pub typ: SemanticType,
    pub usage_count: usize,
    pub full_declaration_range: Option<FileRange>,
}

impl SemanticLocal {
    #[must_use]
    pub fn new(kind: SemanticLocalKind, typ: SemanticType, declaration_range: FileRange) -> Self {
        Self {
            kind,
            name_declaration_range: declaration_range,
            typ,
            usage_count: 0,
            full_declaration_range: None,
        }
    }

    pub fn with_declaration_range(self, range: FileRange) -> Self {
        Self {
            full_declaration_range: Some(range),
            ..self
        }
    }

    pub fn add_usage(&mut self) {
        self.usage_count += 1;
    }
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
            SemanticType::Array(..) => todo!(),
            SemanticType::Builtin(..) => todo!(),
            SemanticType::Custom { .. } => todo!(),
            SemanticType::Function(func) => BabString::clone(&func.name),
            SemanticType::FunctionReference(func) => func.name(),
            SemanticType::IndexReference(..) => todo!(),
            SemanticType::Interface { .. } => todo!(),
            SemanticType::Generic(..) => todo!(),
            SemanticType::Pointer(..) => todo!(),
        }
    }

    pub fn documentation(&self) -> Option<BabString> {
        match &self.typ {
            SemanticType::Array(..) => None,
            SemanticType::Builtin(builtin) => Some(builtin.documentation().into_bab_string()),
            SemanticType::Custom { .. } => None,
            SemanticType::Function(..) => None,
            SemanticType::FunctionReference(func) => func.documentation(),
            SemanticType::IndexReference(..) => None,
            SemanticType::Interface { .. } => None,
            SemanticType::Generic(..) => None,
            SemanticType::Pointer(..) => None,
        }
    }

    pub fn inline_detail(&self) -> Option<BabString> {
        match &self.typ {
            SemanticType::Array(..) => None,
            SemanticType::Builtin(builtin) => Some(builtin.inline_detail()),
            SemanticType::Custom { .. } => None,
            SemanticType::Function(..) => None,
            SemanticType::FunctionReference(func) => func.inline_detail(),
            SemanticType::IndexReference(..) => None,
            SemanticType::Interface { .. } => None,
            SemanticType::Generic(..) => None,
            SemanticType::Pointer(..) => None,
        }
    }

    pub fn lsp_completion(&self) -> BabString {
        match &self.typ {
            SemanticType::Array(ty) => format!("{}[]", ty.name()).into(),
            SemanticType::Builtin(builtin) => builtin.name(),
            SemanticType::Custom { .. } => self.typ.to_string().into(),
            SemanticType::Function(func) => BabString::new(format!("{}($1);$0", func.name.value())),
            SemanticType::FunctionReference(func) => func.lsp_completion(),
            SemanticType::IndexReference(..) => BabString::empty(),
            SemanticType::Interface { .. } => BabString::empty(),
            SemanticType::Generic(..) => BabString::empty(),
            SemanticType::Pointer(ty) => format!("{}*", ty.name()).into(),
        }
    }

    pub fn hover(&self) -> String {
        match self.local_kind {
            SemanticLocalKind::Function | SemanticLocalKind::FunctionReference => {
                format!("werkwijze {}(..)\n```", self.local_name)
            }

            SemanticLocalKind::Method => {
                let mut str = format!("werkwijze {}(", self.local_name);

                if let SemanticType::FunctionReference(func) = &self.typ {
                    match func {
                        FunctionReference::Custom(custom) => {
                            for (idx, param) in custom.parameters.iter().enumerate() {
                                if idx != 0 {
                                    str += ", ";
                                }
                                str += &param.name;
                                str += ": ";
                                str += &param.ty.to_string();
                            }
                        }
                        FunctionReference::Builtin(..) => str += "..",
                    }
                }

                str += ")";
                str
            }

            SemanticLocalKind::FieldReference => {
                format!("veld {}: {}", self.local_name, self.typ)
            }

            SemanticLocalKind::StructureReference => {
                let mut fields = String::new();

                if let SemanticType::Custom { base: typ, .. } = &self.typ {
                    for field in &typ.fields {
                        fields += &format!("\n    veld {}: {}", field.name.value(), field.ty);
                    }

                    for method in &typ.methods {
                        fields += &format!("\n    werkwijze {}(..) {{ /* ... */ }}", method.function.name.value());
                    }
                }

                format!("structuur {} {{{fields}\n}}", self.typ.to_string())
            }

            SemanticLocalKind::Variable => {
                format!("stel {}: {}", self.local_name, self.typ)
            }

            _ => {
                format!("{}: {}", self.local_name, self.typ)
            }
        }
    }

    #[must_use]
    fn return_value(&self) -> SemanticType {
        match &self.typ {
            SemanticType::Function(f) => {
                SemanticType::clone(&f.return_type)
            }

            SemanticType::FunctionReference(FunctionReference::Builtin(builtin)) => {
                SemanticType::Builtin(builtin.return_type)
            }

            SemanticType::FunctionReference(FunctionReference::Custom(f)) => {
                SemanticType::clone(&f.return_type)
            }

            _ => SemanticType::null()
        }
    }
}

#[derive(Debug, Clone)]
pub struct SemanticField {
    pub attributes: AttributeList,
    pub name: Ranged<BabString>,
    pub ty: SemanticType,
    pub has_default_value: bool,
}

#[derive(Debug, Clone)]
pub struct SemanticMethod {
    pub range: FileRange,
    pub function: SemanticFunction,
}

impl SemanticMethod {
    #[must_use]
    fn name(&self) -> &BabString {
        &self.function.name
    }

    #[must_use]
    fn return_type(&self) -> SemanticType {
        self.function.return_type.as_ref().clone()
    }

    #[must_use]
    fn return_type_usage(&self) -> SemanticUsage {
        SemanticUsage::Pure(PureValue::ReturnValue)
    }
}

#[derive(Debug, Clone)]
pub struct SemanticStructure {
    pub attributes: AttributeList,
    pub name: Ranged<BabString>,
    pub generic_types: Vec<Ranged<BabString>>,
    pub left_curly_range: FileRange,
    pub right_curly_range: FileRange,
    pub fields: Vec<SemanticField>,
    pub methods: Vec<SemanticMethod>,
}

impl SemanticStructure {
    pub fn index_of_generic_type(&self, name: &BabString) -> Option<usize> {
        for (idx, generic_name) in self.generic_types.iter().enumerate() {
            if generic_name.value() == name {
                return Some(idx);
            }
        }

        None
    }
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
    Array(Box<SemanticType>),
    Builtin(BuiltinType),
    Custom { base: Arc<SemanticStructure>, parameters: Vec<SemanticType> },
    Function(SemanticFunction),
    FunctionReference(FunctionReference),
    Interface { base: Arc<SemanticInterface>, parameters: Vec<SemanticType> },
    Generic(SemanticGenericType),
    IndexReference(Box<SemanticType>),
    Pointer(Box<SemanticType>),
}

impl SemanticType {
    #[must_use]
    pub fn null() -> Self {
        Self::Builtin(BuiltinType::Null)
    }

    #[must_use]
    pub const fn is_null(&self) -> bool {
        matches!(self, Self::Builtin(BuiltinType::Null))
    }

    pub fn declaration_range(&self) -> FileRange {
        match self {
            Self::Array(ty) => ty.declaration_range(),
            Self::Builtin(..) => FileRange::default(),
            Self::Custom { base, .. } => base.name.range(),
            Self::Function(func) => func.name.range(),
            Self::FunctionReference(func) => func.declaration_range(),
            Self::IndexReference(ty) => ty.declaration_range(),
            Self::Interface { base, .. } => base.name.range(),
            Self::Generic(ty) => ty.declaration_range,
            Self::Pointer(ty) => ty.declaration_range(),
        }
    }

    pub fn parameter_count(&self) -> Option<usize> {
        match self {
            Self::Array(..) => None,
            Self::Builtin(..) => None,
            Self::Custom { .. } => None,
            Self::Function(func) => Some(func.parameters.len()),
            Self::FunctionReference(func) => Some(func.parameter_count()),
            Self::IndexReference(..) => None,
            Self::Interface { .. } => None,
            Self::Generic(..) => None,
            Self::Pointer(..) => None,
        }
    }

    /// A hint to a name that could be used as the name for a field or value.
    #[must_use]
    pub fn value_or_field_name_hint(&self) -> BabString {
        match self {
            Self::Array(..) => BabString::new_static("opeenvolging"),
            Self::Builtin(BuiltinType::Slinger) => BabString::new_static("tekst"),
            Self::Builtin(BuiltinType::G32) => BabString::new_static("getal"),
            Self::Builtin(builtin) => builtin.name().to_lowercase().into(),
            Self::Custom { base, .. } => base.name.value().to_lowercase().into(),
            Self::Interface { base, .. } => base.name.value().to_lowercase().into(),

            Self::Function(..) => BabString::empty(),
            Self::FunctionReference(..) => BabString::empty(),
            Self::IndexReference(ty) => ty.value_or_field_name_hint(),
            Self::Generic(ty) => ty.name.clone(),
            Self::Pointer(..) => BabString::new_static("wijzer"),
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
            Self::Array(..) => BabString::new_static("opeenvolging-naam"),
            Self::Builtin(builtin) => builtin.name().into(),
            Self::Custom { base, .. } => base.name.value().clone(),
            Self::Function(func) => func.name.value().clone(),
            Self::FunctionReference(func) => func.name(),
            Self::IndexReference(ty) => ty.name(),
            Self::Interface { base, .. } => base.name.value().clone(),
            Self::Generic(ty) => ty.name.clone(),
            Self::Pointer(..) => BabString::new_static("wijzer-naam"),
        }
    }

    /// Returns the element type, if it is subscriptable.
    pub fn subscript(&self) -> Option<SemanticType> {
        match self {
            Self::Array(ty) => Some(ty.as_ref().clone()),
            Self::Builtin(BuiltinType::Slinger) => Some(SemanticType::Builtin(BuiltinType::Teken)),
            _ => None,
        }
    }

    pub fn resolve_against(self, ty: &SemanticType) -> Self {
        let generic_index = match self {
            Self::Array(element_type) => {
                let element_type = *element_type;
                let element_type = element_type.resolve_against(ty);
                return Self::Array(Box::new(element_type));
            }

            Self::Generic(ref generic) => generic.index,

            other => return other,
        };

        let SemanticType::Custom { parameters, .. } = ty else {
            log::error!("Kan generieke parameters niet resolveren met type: {ty:#?}");
            return self;
        };

        if parameters.len() <= generic_index {
            // Te doen: kan ik vreemde omstandigheden gebeuren.
            return self;
        }

        parameters[generic_index].clone()
    }

    #[must_use]
    fn can_be_extended(&self) -> bool {
        !self.is_null()
    }

    #[must_use]
    fn is_primitive_number(&self) -> bool {
        match self {
            Self::Builtin(BuiltinType::G8) => true,
            Self::Builtin(BuiltinType::G16) => true,
            Self::Builtin(BuiltinType::G32) => true,
            _ => false,
        }
    }

    #[must_use]
    fn is_compatible_with(&self, other: &SemanticType) -> bool {
        if self == other {
            return true;
        }

        self.is_primitive_number() && other.is_primitive_number()
    }
}

impl Display for SemanticType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Array(arr) => {
                arr.fmt(f)?;
                f.write_str("[]")
            }
            Self::Builtin(typ) => typ.fmt(f),
            Self::Custom { base, parameters } => {
                base.fmt(f)?;

                if parameters.is_empty() {
                    return Ok(());
                }

                f.write_char('<')?;

                for (idx, param) in parameters.iter().enumerate() {
                    if idx != 0 {
                        f.write_str(", ")?;
                    }

                    param.fmt(f)?;
                }

                f.write_char('>')
            }
            Self::Function(func) => func.fmt(f),
            Self::FunctionReference(func) => func.fmt(f),
            Self::IndexReference(ty) => ty.fmt(f),
            Self::Interface { base, parameters } => {
                base.fmt(f)?;

                if parameters.is_empty() {
                    return Ok(());
                }

                f.write_char('<')?;

                for (idx, param) in parameters.iter().enumerate() {
                    if idx != 0 {
                        f.write_str(", ")?;
                    }

                    param.fmt(f)?;
                }

                f.write_char('>')
            }
            Self::Generic(ty) => ty.fmt(f),
            Self::Pointer(ty) => {
                ty.fmt(f)?;
                f.write_char('*')
            }
        }
    }
}

impl PartialEq<BuiltinType> for SemanticType {
    fn eq(&self, other: &BuiltinType) -> bool {
        self == &Self::Builtin(*other)
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

    #[must_use]
    fn name(&self) -> &'static str {
        match self {
            Self::Parameter => "parameter",
            Self::FieldReference => "veld",
            Self::StructureReference => "structuur",
            Self::Iterator => "iterator",
            Self::Function => "werkwijze",
            Self::FunctionReference => "werkwijze",
            Self::Variable => "variabele",
            Self::Method => "structuurwerkwijze",
            Self::ReferenceThis => "dit",
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

#[derive(Debug, Clone)]
pub struct SemanticExternFunction {
    #[allow(unused)]
    name: BabString,
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
    IndexReference,
}

trait StructureOrInterface {
    #[must_use]
    fn fields(&self) -> &[SemanticField];

    #[must_use]
    fn name(&self) -> &Ranged<BabString>;

    #[must_use]
    fn left_curly_range(&self) -> FileRange;
}

impl StructureOrInterface for SemanticStructure {
    fn fields(&self) -> &[SemanticField] {
        &self.fields
    }

    fn name(&self) -> &Ranged<BabString> {
        &self.name
    }

    fn left_curly_range(&self) -> FileRange {
        self.left_curly_range
    }
}

impl StructureOrInterface for SemanticInterface {
    fn fields(&self) -> &[SemanticField] {
        &[]
    }

    fn name(&self) -> &Ranged<BabString> {
        &self.name
    }

    fn left_curly_range(&self) -> FileRange {
        self.left_curly_range
    }
}

#[derive(Debug, Default)]
struct SemanticFunctionAnalysis {
    parameters: Vec<SemanticType>,
    return_type: Option<SemanticType>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, EnumIter)]
pub enum SemanticAnalysisPhase {
    Phase1,
    Phase2,
    Phase3,
    Phase4,
}

impl SemanticAnalysisPhase {
    pub fn iter() -> impl Iterator<Item = SemanticAnalysisPhase> {
        <Self as IntoEnumIterator>::iter()
    }
}

#[derive(Default, Debug)]
struct SemanticDiagnosticsList {
    contents: Option<Vec<SemanticDiagnostic>>,
}

impl SemanticDiagnosticsList {
    fn new(should_produce_diagnostics: bool) -> Self {
        Self {
            contents: if should_produce_diagnostics {
                Some(Vec::new())
            } else {
                None
            }
        }
    }

    #[inline]
    pub fn create<F: FnOnce() -> SemanticDiagnostic>(&mut self, f: F) {
        let Some(contents) = &mut self.contents else { return };

        contents.push(f());
    }

    fn to_vec(self) -> Vec<SemanticDiagnostic> {
        self.contents.unwrap_or_default()
    }

    fn as_slice(&self) -> &[SemanticDiagnostic] {
        self.contents.as_ref().map(|x| x.as_slice()).unwrap_or_default()
    }
}
