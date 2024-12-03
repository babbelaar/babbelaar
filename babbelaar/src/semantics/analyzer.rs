// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::{HashMap, HashSet}, fmt::Write, sync::Arc};

use log::warn;

use crate::*;
use super::*;

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

        if !statement.is_freestanding() && !self.is_in_function_scope() {
            self.diagnostics.create(|| {
                SemanticDiagnostic::new(statement.range, SemanticDiagnosticKind::StatementOutsideFunction)
            });
            return;
        }

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

            if !argument_type.is_compatible_with(&parameter_type) {
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

        let ty = match unary.kind.value() {
            UnaryExpressionKind::AddressOf => {
                if !matches!(unary.rhs.value(), Expression::Primary(PrimaryExpression::Reference(..))) {
                    self.diagnostics.create(|| {
                        SemanticDiagnostic::new(operator_range, SemanticDiagnosticKind::CannotNegateNonInteger)
                    });
                }

                SemanticType::Pointer(Box::new(ty))
            }

            UnaryExpressionKind::Negate => {
                if ty != SemanticType::Builtin(BuiltinType::G32) {
                    self.diagnostics.create(|| {
                        SemanticDiagnostic::new(operator_range, SemanticDiagnosticKind::CannotNegateNonInteger)
                    });
                }

                ty
            }
        };

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

    /// Returns whether or not we are in a function scope, or a parent scope is a function scope.
    ///
    /// E.g.
    /// ```plain
    /// function -> true
    /// function if -> true
    /// function if if -> true
    /// if -> false
    /// if if -> false
    /// ```
    #[must_use]
    fn is_in_function_scope(&self) -> bool {
        self.context.scope.iter().find(|x| x.kind.is_function()).is_some()
    }
}
