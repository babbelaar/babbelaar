// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::{HashMap, HashSet}, fmt::Write, mem::replace, sync::{Arc, Mutex, atomic::{AtomicBool, Ordering}}};

use log::{trace, warn};

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

    #[must_use]
    pub fn with_diagnostic_settings(self, settings: SemanticDiagnosticSettings) -> Self {
        Self {
            diagnostics: self.diagnostics.with_settings(settings),
            ..self
        }
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
                for statement in tree.functions() {
                    if let StatementKind::Function(function) = &statement.kind {
                        self.analyze_function_declaration(function, statement);
                    }
                }
            }

            SemanticAnalysisPhase::Phase3 => {
                self.analyze_statements(tree.extensions());
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
                self.analyze_function_declaration(function, statement);
            }
        }

        for statement in statements {
            self.analyze_statement(statement);
        }
    }

    /// Analyze a function declaration (signature) without analyzing the statements inside
    fn analyze_function_declaration(&mut self, function: &FunctionStatement, statement: &Statement) {
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
                attributes: statement.attributes.clone(),
                name: function.name.clone(),
                parameters,
                has_variable_arguments: AtomicBool::new(false),
                parameters_right_paren_range: function.parameters_right_paren_range,
                extern_function: Mutex::new(None),
                return_type,
            },
            statement.range,
        );

        for attribute in &statement.attributes {
            if *attribute.name == AttributeName::Extern {
                self.analyze_attribute_extern(statement, attribute);
                continue;
            }

            if *attribute.name == AttributeName::VarArgs {
                self.analyze_attribute_var_args(statement, attribute);
                continue;
            }

            self.diagnostics.create(|| SemanticDiagnostic::new(
                attribute.name.range(),
                SemanticDiagnosticKind::UnknownAttribute { name: attribute.name.as_str(), range: attribute.range() },
            ));
        }
    }

    fn analyze_expression(&mut self, expression: &Ranged<Expression>, resolution: &SemanticTypeResolution) -> SemanticValue {
        let value = match expression.value() {
            Expression::BiExpression(bi) => self.analyze_bi_expression(bi, resolution),
            Expression::Postfix(postfix) => self.analyze_postfix_expression(postfix, resolution),
            Expression::Primary(primary) => self.analyze_primary_expression(primary, expression.range(), resolution),
            Expression::Unary(unary) => self.analyze_unary_expression(unary, resolution),
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

        let ext = self.context.push_extension(SemanticExtension {
            ty: ty.clone(),
            interface,
            generic_types: extension.generic_types.iter().map(|x| x.value().clone()).collect(),
            methods: HashMap::new(),
            range,
            right_curly_bracket: extension.right_curly_bracket,
        });

        let mut method_info = Vec::with_capacity(extension.methods.len());
        for method in &extension.methods {
            let (function, scope_list) = self.analyze_function_with_callback(&method.function, Some(ty.clone()), |this, _| this.context.scope.scope.clone());

            let is_valid = !self.is_invalid_method(ext, method, function);
            method_info.push((is_valid, scope_list));

            if !is_valid {
                continue;
            }

            let name = method.function.name.value().clone();
            let method = self.create_semantic_method(method);

            self.context.extensions[ext.id()].methods.insert(name, method);
        }

        for (method, (is_valid, scope_list)) in extension.methods.iter().zip(method_info.into_iter()) {
            if !is_valid {
                continue;
            }

            let scope_list_to_restore = replace(&mut self.context.scope.scope, scope_list);
            self.analyze_statements(method.function.body.as_inner_slice());
            self.context.scope.scope = scope_list_to_restore;
        }

        if let Some(interface) = self.context.extensions[ext.id()].interface.clone() {
            self.analyze_missing_interface_methods(ext, extension, &interface);
        }

        self.context.pop_scope();
    }

    /// Returns whether or not this method is invalid.
    fn is_invalid_method(&mut self, ext: SemanticExtensionId, method: &Method, function: SemanticFunctionAnalysis) -> bool {
        let name = method.function.name.value();
        let interface;

        {
            let ext = &self.context.extensions[ext.id()];
            if let Some(existing) = ext.methods.get(name) {
                let name = name.clone();
                self.diagnostics.create(||
                    SemanticDiagnostic::new(method.function.name.range(), SemanticDiagnosticKind::DuplicateMethodNameInExtension { name: name.clone(), structure: ext.ty.name() })
                        .with_related(SemanticRelatedInformation::new(existing.function.name.range(), SemanticRelatedMessage::DuplicateMethodFirstDefinedHere { name }))
                        .with_action(BabbelaarCodeAction::new_command(method.function.name.range(), BabbelaarCommand::RenameFunction))
                );
                return true;
            }

            interface = ext.interface.clone();
        }

        if let Some(interface) = interface {
            self.is_invalid_method_in_interface_extension(name, method, &interface, function)
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
    fn is_invalid_method_in_normal_extension(&self, ext: SemanticExtensionId, name: &BabString, method: &Method) -> bool {
        let ty = self.context.extensions[ext.id()].ty.clone();
        let SemanticType::Custom { base, .. } = ty else { return false };

        let structure = self.context.structure(base);
        if let Some(existing) = structure.methods.iter().find(|x| x.name() == name) {
            let name = name.clone();
            self.diagnostics.create(||
                SemanticDiagnostic::new(method.function.name.range(), SemanticDiagnosticKind::DuplicateMethodNameInExtension { name: name.clone(), structure: ty.name() })
                    .with_related(SemanticRelatedInformation::new(existing.function.name.range(), SemanticRelatedMessage::DuplicateMethodFirstDefinedHere { name }))
                    .with_action(BabbelaarCodeAction::new_command(method.function.name.range(), BabbelaarCommand::RenameFunction))
            );
            return true;
        }

        false
    }

    fn analyze_missing_interface_methods(&mut self, ext: SemanticExtensionId, ast: &ExtensionStatement, interface: &SemanticInterface) {
        let mut methods = Vec::new();
        let ext = &mut self.context.extensions[ext.id()];

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
        self.analyze_function_with_callback(function, this, Self::analyze_function_body_now).0
    }

    fn analyze_function_with_callback<F, R>(&mut self, function: &FunctionStatement, this: Option<SemanticType>, f: F) -> (SemanticFunctionAnalysis, R)
            where F: FnOnce(&mut Self, &FunctionStatement) -> R {
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

        let result = f(self, function);

        self.context.pop_scope();

        (analysis, result)
    }

    fn analyze_function_body_now(&mut self, function: &FunctionStatement) {
        self.analyze_statements(function.body.as_inner_slice());
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
            StatementKind::Break => {

            }

            StatementKind::Continue => {

            }

            StatementKind::Expression(expr) => {
                let value = self.analyze_expression(expr, &SemanticTypeResolution::default());

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
                let destination_type = if assign.destination.value().as_identifier() == Some(&Constants::DISCARDING_IDENT) {
                    SemanticType::null()
                } else {
                    self.analyze_expression(&assign.destination, &SemanticTypeResolution::basic_hints_from_expression(assign.source.value())).ty
                };

                let source_type = self.analyze_expression(&assign.source, &SemanticTypeResolution::with_type_hint(destination_type.clone())).ty;

                if !destination_type.is_null() {
                    self.analyze_assignment_destination(assign.range(), &assign.destination);
                    self.analyze_assignment_source_dest(assign, destination_type, source_type);
                }
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
                let ty = self.analyze_expression(expression, &SemanticTypeResolution::default()).ty;
                self.analyze_for_statement_iterable_expression(expression, ty)
            }

            ForIterableKind::Range(range) => {
                self.analyze_range(range)
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

            SemanticType::Builtin(BuiltinType::Slinger) => {
                return SemanticType::Builtin(BuiltinType::Teken);
            }

            SemanticType::Custom { base, ref parameters, .. } => {
                if let Some(interface) = self.resolve_interface_by_name(&BabString::new_static("Doorloper")) {
                    if let Some(extension) = self.get_interface_implementation_for(&ty, &interface) {
                        let ty_name = interface.generic_types[0].value().clone();
                        let ty_idx = self.context.structure(base).index_of_generic_type(&ty_name).expect("Het vinden van de generieke parameter van `Doorloper`");
                        _ = extension; // Dit zou gebruikt moeten worden?
                        return parameters[ty_idx].clone();
                    }
                }

                self.emit_diagnostic(|this| {
                    SemanticDiagnostic::new(expression.range(), SemanticDiagnosticKind::ExpressionNotIterable)
                        .with_related(SemanticRelatedInformation::new(expression.range(), SemanticRelatedMessage::ExpressionIsOfType { ty: ty.clone() }))
                        .with_action(this.create_actions_extend_structure_with_interface_by_name(base, expression.range().file_id(), "Doorloper"))
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

    #[must_use]
    fn analyze_range(&mut self, range: &RangeExpression) -> SemanticType {
        let mut resolution = SemanticTypeResolution::with_getal_type_hints();
        let start_ty = self.analyze_range_param("Startwaarde", &range.start, &resolution);

        if !start_ty.is_null() {
            resolution.type_hints = vec![start_ty];
        }

        let end_ty = self.analyze_range_param("Eindwaarde", &range.end, &resolution);

        if !end_ty.is_null() {
            return end_ty;
        }

        resolution.type_hints.remove(0)
    }

    #[must_use]
    fn analyze_range_param(&mut self, name: &'static str, expression: &Ranged<Expression>, resolution: &SemanticTypeResolution) -> SemanticType {
        let ty = self.analyze_expression(expression, resolution).ty;

        if ty == SemanticType::null() {
            return ty;
        }

        if !ty.is_primitive_number() {
            let conversion_actions = self.try_create_conversion_actions(&SemanticType::Builtin(BuiltinType::G32), &ty, expression);
            self.diagnostics.create(||
                SemanticDiagnostic::new(expression.range(), SemanticDiagnosticKind::RangeExpectsInteger { name, ty: ty.clone() })
                    .with_actions(conversion_actions)
            );
        }

        ty
    }

    fn analyze_if_statement(&mut self, statement: &IfStatement) {
        self.context.push_block_scope(statement.range);

        self.analyze_expression(&statement.condition, &SemanticTypeResolution::with_type_hint(SemanticType::Builtin(BuiltinType::Bool)));

        self.analyze_statements(&statement.body);

        self.context.pop_scope();
    }

    fn analyze_return_statement(&mut self, statement: &ReturnStatement) {
        match (&statement.expression, self.context.current().return_type.clone()) {
            (Some(actual), Some(expected)) => {
                let actual_type = self.analyze_expression(actual, &SemanticTypeResolution::with_type_hint(expected.value().clone()));
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
                let actual_type = self.analyze_expression(actual, &SemanticTypeResolution::default());

                if actual_type.ty == SemanticType::Builtin(BuiltinType::Null) {
                    return;
                }

                let right_parameter_range = self.context.scope.iter().rev()
                    .filter_map(|x| match &x.kind {
                        SemanticScopeKind::Default => None,
                        SemanticScopeKind::TopLevel => None,
                        SemanticScopeKind::Structure { .. } => None,
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
                        SemanticScopeKind::Structure { .. } => None,
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

        let semantic_structure = SemanticStructure {
            attributes: statement.attributes.clone(),
            name: structure.name.clone(),
            generic_types: structure.generic_types.clone(),
            left_curly_range: structure.left_curly_range,
            right_curly_range: structure.right_curly_range,
            fields,
            methods,
        };

        let field_count = semantic_structure.fields.len();
        let structure_id = structure.id;
        self.context.push_structure(semantic_structure, structure_id);

        for (idx, field) in structure.fields.iter().enumerate() {
            if let Some(default_value) = &field.default_value {
                let field_type = self.context.field(structure_id, SemanticFieldId::new(idx)).ty.clone();
                self.analyze_expression(default_value, &SemanticTypeResolution::with_type_hint(field_type));
            }
        }

        let mut names = HashSet::new();
        for field in &self.context.structure(structure_id).fields {
            if !names.insert(field.name.value()) {
                self.diagnostics.create(||
                    SemanticDiagnostic::new(
                        field.name.range(),
                        SemanticDiagnosticKind::DuplicateFieldName { name: field.name.value().clone() },
                    )
                    .with_action(BabbelaarCodeAction::new_command(field.name.range(), BabbelaarCommand::RenameField))
                );
            }
        }

        for field in 0..field_count {
            let field = SemanticFieldId::new(field);
            self.analyze_attributes_for_field(structure_id, field);
        }

        let this_type = SemanticType::Custom {
            base: structure_id,
            parameters: Vec::new(),
            name: structure.name.clone(),
        };

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

            let this_type = if !method.is_static { Some(this_type.clone()) } else { None };
            self.analyze_function(&method.function, this_type);
        }

        self.context.pop_scope();
    }

    #[must_use]
    fn create_semantic_method(&mut self, method: &Method) -> SemanticMethod {
        SemanticMethod {
            range: method.range,
            function: Arc::new(self.create_semantic_function(&method.function, Vec::new())),
            is_static: method.is_static,
        }
    }

    #[must_use]
    fn create_semantic_function(&mut self, function: &FunctionStatement, attributes: Vec<Ranged<Attribute>>) -> SemanticFunction {
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
            attributes,
            name: function.name.clone(),
            parameters,
            has_variable_arguments: AtomicBool::new(false),
            parameters_right_paren_range: function.parameters_right_paren_range,
            extern_function: Mutex::new(None),
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
        let mut resolution = SemanticTypeResolution::default();

        let typ = match &statement.typ {
            Some(type_spec) => {
                resolution.type_hints.push(self.resolve_type(type_spec));

                let actual_ty = self.analyze_expression(&statement.expression, &resolution).ty;
                let expected_ty = resolution.type_hints.remove(0);

                if expected_ty != actual_ty {
                    self.diagnostics.create(||
                        SemanticDiagnostic::new(
                            statement.expression.range(),
                            SemanticDiagnosticKind::VariableStatementIncompatibleTypes {
                                actual: actual_ty.clone(),
                                expected: expected_ty.clone(),
                            },
                        )
                        .with_action(BabbelaarCodeAction::new(
                            BabbelaarCodeActionType::ChangeVariableType {
                                typ: actual_ty.name(),
                            },
                            vec![
                                FileEdit::new(type_spec.range(), actual_ty.name().to_string())
                            ]
                        ))
                    );
                }

                expected_ty
            }

            None => {
                self.analyze_expression(&statement.expression, &resolution).ty
            }
        };

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

    fn analyze_bi_expression(&mut self, expression: &BiExpression, resolution: &SemanticTypeResolution) -> SemanticValue {
        let lhs_type = self.analyze_expression(&expression.lhs, resolution).ty;
        let rhs_type = self.analyze_expression(&expression.rhs, resolution).ty;

        if !lhs_type.is_null() && !rhs_type.is_null() && !lhs_type.is_compatible_with(&rhs_type) {
            self.diagnostics.create(|| SemanticDiagnostic::new(
                expression.operator.range(),
                SemanticDiagnosticKind::IncompatibleTypes {
                    lhs_type: lhs_type.clone(),
                    rhs_type,
                }
            ));
        }

        let ty = match expression.operator.value() {
            BiOperator::Comparison(..) => SemanticType::Builtin(BuiltinType::Bool),
            BiOperator::Math(..) => lhs_type,
        };

        SemanticValue {
            ty,
            usage: SemanticUsage::Pure(PureValue::Operator {
                operator_range: expression.operator.range(),
            }),
        }
    }

    fn analyze_function_call_expression_function_ref(&mut self, lhs: SemanticType, expression: &FunctionCallExpression, postfix: &PostfixExpression) -> Option<(SemanticReference, BabString)> {
        // TODO: why do we this again? we only need the function and function ref...
        let function_name = match lhs {
            SemanticType::Array(..) => postfix.lhs.value().to_string().into(),
            SemanticType::Builtin(BuiltinType::Null) => postfix.lhs.value().to_string().into(),
            SemanticType::Builtin(builtin) => builtin.name(),
            SemanticType::Custom { base, .. } => self.context.structure(base).name.value().clone(),
            SemanticType::Function(func) => func.name.value().clone(),
            SemanticType::FunctionReference(FunctionReference::Custom(function)) => {
                let function_name = function.name.value().clone();
                return Some((SemanticReference {
                    local_name: function.name.value().clone(),
                    local_kind: SemanticLocalKind::Function,
                    declaration_range: function.name.range(),
                    typ: SemanticType::Function(function),
                }, function_name));
            }
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
            return None;
        };

        Some((function, function_name))
    }

    fn analyze_function_call_expression(&mut self, lhs: SemanticType, expression: &FunctionCallExpression, postfix: &PostfixExpression) -> SemanticValue {
        let Some((function, function_name)) = self.analyze_function_call_expression_function_ref(lhs, expression, postfix) else {
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
        let has_var_args = function.has_variable_arguments();

        if param_count > arg_count {
            self.diagnostics.create(|| SemanticDiagnostic::new(
                expression.token_right_paren,
                SemanticDiagnosticKind::TooFewArguments { function_name: function_name.clone(), param_count, arg_count },
            ).with_related(function_hint.clone()));
        }

        if !has_var_args && param_count < arg_count {
            let mut add_parameter_action = None;
            let mut remove_parameter_action = None;

            if let SemanticType::FunctionReference(FunctionReference::Custom(func)) = &function.typ {
                let residual_args = arg_count - param_count;
                let mut params = String::new();

                for (idx, arg) in expression.arguments.iter().enumerate().skip(param_count) {
                    if idx != 0 {
                        params += ", ";
                    }

                    let argument_type = self.analyze_expression(arg, &SemanticTypeResolution::default()).ty;
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
            } else if let SemanticType::FunctionReference(FunctionReference::Builtin(..)) = &function.typ {
                let residual_args = arg_count - param_count;
                let mut params = String::new();

                for (idx, arg) in expression.arguments.iter().enumerate().skip(param_count) {
                    if idx != 0 {
                        params += ", ";
                    }

                    let argument_type = self.analyze_expression(arg, &SemanticTypeResolution::default()).ty;
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
            let parameter_type = self.resolve_parameter_type(&function, arg_idx);

            let resolution = if let Some(parameter_type) = parameter_type.clone() {
                SemanticTypeResolution::with_type_hint(parameter_type.clone())
            } else {
                SemanticTypeResolution::default()
            };
            let argument_type = self.analyze_expression(arg, &resolution).ty;
            trace!("Matching {parameter_type:?} to argument {argument_type:?}");

            let Some(parameter_type) = parameter_type else {
                if has_var_args {
                    continue;
                }

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

    fn analyze_primary_expression(&mut self, expression: &PrimaryExpression, range: FileRange, resolution: &SemanticTypeResolution) -> SemanticValue {
        let ty = match expression {
            PrimaryExpression::CharacterLiteral(..) => {
                SemanticType::Builtin(BuiltinType::Teken)
            }

            PrimaryExpression::Boolean(..) => {
                SemanticType::Builtin(BuiltinType::Bool)
            }

            PrimaryExpression::IntegerLiteral(..) => {
                resolution.type_hints.iter()
                    .find(|x| x.is_primitive_number())
                    .cloned()
                    .unwrap_or(SemanticType::Builtin(BuiltinType::G32))
            }

            PrimaryExpression::StringLiteral(..) => {
                SemanticType::Builtin(BuiltinType::Slinger)
            }

            PrimaryExpression::ReferenceThis => {
                let scope = self.context.scope.current();

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
                            self.analyze_expression(expr, &&SemanticTypeResolution::with_type_hint(SemanticType::Builtin(BuiltinType::Slinger)));
                            // TODO analyze string convertible.
                        }

                        _ => (),
                    }
                }

                SemanticType::Builtin(BuiltinType::Slinger)
            }

            PrimaryExpression::Reference(reference, computed) => {
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

                *computed.ty.lock().unwrap() = Some(local.typ.clone());

                let typ = local.typ.clone();
                if let Some(tracker) = &mut self.context.definition_tracker {
                    tracker.insert(reference.range(), local_reference);
                }

                typ
            }

            PrimaryExpression::ReferencePath(path) => {
                self.analyze_path_expression(path)
            }

            PrimaryExpression::Parenthesized(expr) => return self.analyze_expression(expr, &SemanticTypeResolution::default()),

            PrimaryExpression::SizedArrayInitializer { typ, size } => {
                let size_value = self.analyze_expression(&size, &SemanticTypeResolution::with_getal_type_hints());
                if !size_value.ty.is_primitive_number(){
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
        let field_types: Vec<SemanticType> = instantiation.fields.iter()
            .map(|field| {
                self.analyze_expression(&field.value, &SemanticTypeResolution::default()).ty
            })
            .collect();
        let ty = self.resolve_type_by_name(&instantiation.name, &instantiation.type_parameters, Some((instantiation, &field_types)));
        let SemanticType::Custom { base, .. } = &ty else {
            return SemanticValue::null();
        };

        let structure = self.context.structures.get(&base).unwrap();
        let struct_hint = SemanticRelatedInformation::new(
            structure.name.range(),
            SemanticRelatedMessage::StructureDefinedHere { name: structure.name.value().clone() }
        );

        let all_valid_fields: HashMap<BabString, FileRange> = structure.fields.iter().map(|x| (x.name.value().clone(), x.name.range())).collect();
        let mut fields_left: HashMap<BabString, SemanticFieldId> = structure.fields.iter().enumerate().map(|(idx, x)| (x.name.value().clone(), SemanticFieldId::new(idx))).collect();

        if let Some(tracker) = &mut self.context.definition_tracker {
            tracker.insert(instantiation.name.range(), SemanticReference {
                local_name: instantiation.name.value().clone(),
                local_kind: SemanticLocalKind::StructureReference,
                declaration_range: structure.name.range(),
                typ: ty.clone(),
            });
        }

        let structure_id = *base;
        let struct_name = structure.name.value().clone();
        for field_instantiation in &instantiation.fields {
            let name = &field_instantiation.name;
            match fields_left.remove(name.value()) {
                Some(field_id) => {
                    let declaration_range = self.context.field(structure_id, field_id).name.range();
                    let declaration_type = self.context.field(structure_id, field_id).ty.clone().resolve_against(&ty);
                    let definition_type = self.analyze_expression(&field_instantiation.value, &SemanticTypeResolution::with_type_hint(declaration_type.clone())).ty;
                    if !declaration_type.is_compatible_with(&definition_type) {
                        let actions = self.try_create_conversion_actions(&declaration_type, &definition_type, &field_instantiation.value);

                        self.diagnostics.create(|| SemanticDiagnostic::new(
                            field_instantiation.value.range(),
                            SemanticDiagnosticKind::IncompatibleFieldTypes {
                                struct_name: struct_name.clone(),
                                field_name: name.value().clone(),
                                declaration_type: declaration_type.to_string(),
                                definition_type: definition_type.to_string(),
                            },
                        ).with_related(struct_hint.clone()).with_actions(actions));
                    }

                    if let Some(tracker) = &mut self.context.definition_tracker {
                        tracker.insert(field_instantiation.name.range(), SemanticReference {
                            local_name: name.value().clone(),
                            local_kind: SemanticLocalKind::FieldReference,
                            declaration_range,
                            typ: declaration_type.clone(),
                        });
                    }
                }
                None => {
                    if let Some(duplicate_field_name_range) = all_valid_fields.get(name.value()).copied() {
                        let field_def_hint = SemanticRelatedInformation::new(
                            duplicate_field_name_range,
                            SemanticRelatedMessage::DuplicateFieldFirstUse { name: name.value().clone() }
                        );

                        let diag = SemanticDiagnostic::new(
                            name.range(),
                            SemanticDiagnosticKind::DuplicateFieldInstantiation { name: name.value().clone()},
                        );

                        self.diagnostics.create(|| diag.with_related(field_def_hint).with_related(struct_hint.clone()));
                    } else {
                        let definition_type = self.analyze_expression(&field_instantiation.value, &SemanticTypeResolution::default()).ty;
                        let create_field_actions = self.create_actions_create_field(&ty, &field_instantiation.name, definition_type);

                        self.diagnostics.create(||
                            SemanticDiagnostic::new(
                                name.range(),
                                SemanticDiagnosticKind::InvalidFieldInstantiation {
                                    struct_name: self.context.structure(*base).name.value().clone(),
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
            .filter(|(_, field_id)| !self.context.field(structure_id, **field_id).has_default_value)
            .count();

        if count_fields_left != 0 {
            let names = fields_left.iter()
                .filter(|(_, field_id)| !self.context.field(structure_id, **field_id).has_default_value)
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

    fn analyze_unary_expression(&mut self, unary: &UnaryExpression, resolution: &SemanticTypeResolution) -> SemanticValue {
        let ty = self.analyze_expression(&unary.rhs, resolution).ty;

        let operator_range = unary.kind.range();

        let ty = match unary.kind.value() {
            UnaryExpressionKind::AddressOf => {
                if !unary.rhs.value().can_be_taken_address_of() {
                    self.diagnostics.create(|| {
                        SemanticDiagnostic::new(operator_range, SemanticDiagnosticKind::CannotTakeAddressOfExpression)
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

            UnaryExpressionKind::Not => {
                if ty != SemanticType::Builtin(BuiltinType::Bool) {
                    self.diagnostics.create(|| {
                        SemanticDiagnostic::new(operator_range, SemanticDiagnosticKind::CannotLogicalNotNonBool)
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
    pub fn diagnostics(&self) -> Vec<SemanticDiagnostic> {
        self.diagnostics.collect()
    }

    pub fn iterate_diagnostics<F: FnMut(&SemanticDiagnostic)>(&self, f: F) {
        self.diagnostics.iterate(f)
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
    pub fn resolve_type(&self, ty: &Ranged<Type>) -> SemanticType {
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
    fn resolve_type_specifier(&self, specifier: &TypeSpecifier) -> SemanticType {
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

            SemanticType::Custom { base, parameters, name } => {
                let parameters = parameters.iter().map(|x| self.refine_type(x)).collect();
                SemanticType::Custom { base: *base, parameters, name: name.clone() }
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
    fn resolve_type_by_name(&self, name: &Ranged<BabString>, params: &Ranged<Vec<Ranged<Type>>>, instantiation: Option<(&StructureInstantiationExpression, &[SemanticType])>) -> SemanticType {
        for scope in self.context.scope.iter().rev() {
            if let Some(generic) = scope.generic_types.get(&name) {
                return SemanticType::Generic(generic.clone());
            }

            if let Some(structure_id) = scope.structures.get(name.value()).copied() {
                let mut parameters = Vec::new();
                for parameter in params.value() {
                    parameters.push(self.resolve_type(parameter));
                }

                let structure = self.context.structure(structure_id);
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
                    base: structure_id,
                    parameters,
                    name: structure.name.clone(),
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
        for scope in &self.context.scope.all_scopes {
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

    fn analyze_postfix_expression(&mut self, postfix: &PostfixExpression, resolution: &SemanticTypeResolution) -> SemanticValue {
        let lhs = self.analyze_expression(&postfix.lhs, resolution).ty;
        match postfix.kind.value() {
            PostfixExpressionKind::Call(call) => self.analyze_function_call_expression(lhs, call, postfix),
            PostfixExpressionKind::Member(member) => self.analyze_member_expression(lhs, member, resolution),
            PostfixExpressionKind::MethodCall(method) => self.analyze_method_expression(lhs, method),
            PostfixExpressionKind::Subscript(expr) => self.analyze_subscript_expression(lhs, &expr, postfix.kind.range()),
        }
    }

    fn analyze_member_expression(&mut self, typ: SemanticType, member: &Ranged<BabString>, resolution: &SemanticTypeResolution) -> SemanticValue {
        let typ = match typ {
            SemanticType::Pointer(inner) => *inner,
            other => other,
        };

        let SemanticType::Custom { base, .. } = &typ else {
            self.diagnostics.create(|| SemanticDiagnostic::new(
                member.range(),
                SemanticDiagnosticKind::InvalidMember { typ, name: member.value().clone() }
            ));

            return SemanticValue::null()
        };

        for field in 0..self.context.structure(*base).fields.len() {
            let field = &self.context.structure(*base).fields[field];
            if field.name.value() != member.value() {
                continue;
            }
            let ty = field.ty.clone().resolve_against(&typ);

            let field_range = field.name.range();
            let field_name = field.name.value().clone();

            if let Some(tracker) = &mut self.context.definition_tracker {
                tracker.insert(member.range(), SemanticReference {
                    local_name: member.value().clone(),
                    local_kind: SemanticLocalKind::FieldReference,
                    declaration_range: field_range,
                    typ: ty.clone(),
                });
            }

            return SemanticValue {
                ty,
                usage: SemanticUsage::Pure(PureValue::FieldReference {
                    declaration: field_range,
                    name: field_name,
                }),
            };
        }

        let base = self.context.structure(*base);
        let struct_hint = SemanticRelatedInformation::new(
            base.name.range(),
            SemanticRelatedMessage::StructureDefinedHere { name: base.name.value().clone() }
        );

        let actions = if let Some(ty) = resolution.type_hints.first() {
            self.create_actions_create_field(&typ, &member, ty.clone())
        } else {
            Vec::new()
        };

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
                        let name = BabString::new_static(method.name);
                        let function = SemanticReference {
                            local_name: name.clone(),
                            local_kind: SemanticLocalKind::Method,
                            declaration_range: FileRange::default(),
                            typ: SemanticType::FunctionReference(FunctionReference::Builtin(method)),
                        };

                        let this_structure = Some(&SemanticType::Builtin(builtin));

                        self.analyze_function_parameters(name, function, &expression.call, this_structure);
                        return SemanticValue {
                            ty: SemanticType::Builtin(method.return_type),
                            usage: if method.must_use { SemanticUsage::Pure(PureValue::ReturnValue) } else { SemanticUsage::Indifferent },
                        };
                    }
                }

                if let Some(value) = self.analyze_method_expression_with_extensions(&typ, expression) {
                    return value;
                }

                let args: Vec<SemanticType> = expression.call.arguments.iter().map(|x| self.analyze_expression(x, &SemanticTypeResolution::default()).ty).collect();
                let create_method_extension_actions = self.create_actions_create_method_extension(&typ, expression, &args);

                self.diagnostics.create(|| SemanticDiagnostic::new(
                    expression.method_name.range(),
                    SemanticDiagnosticKind::InvalidMethod { typ, name: expression.method_name.value().clone()}
                ).with_actions(create_method_extension_actions));

                SemanticValue::null()
            }

            SemanticType::Custom { ref base, .. } => {
                for method in 0..self.context.structure(*base).methods.len() {
                    if self.context.structure(*base).methods[method].name() != expression.method_name.value() {
                        continue;
                    }

                    let local_reference;
                    let method_return_type;
                    let usage;
                    let method_name;

                    {
                        let method = &self.context.structure(*base).methods[method];
                        local_reference = SemanticReference {
                            local_name: method.name().clone(),
                            local_kind: SemanticLocalKind::Method,
                            declaration_range: method.function.name.range(),
                            typ: SemanticType::FunctionReference(FunctionReference::Custom(Arc::clone(&method.function))), // is this okay?
                        };
                        method_return_type = method.return_type();
                        usage = method.return_type_usage();
                        method_name = method.name().clone();
                    }

                    if let Some(tracker) = &mut self.context.definition_tracker {
                        tracker.insert(expression.method_name.range(), local_reference.clone());
                    }

                    self.analyze_function_parameters(method_name, local_reference, &expression.call, Some(&typ));

                    let ty = method_return_type.resolve_against(&typ);
                    if ty.is_null() {
                        return SemanticValue::null();
                    }

                    return SemanticValue {
                        ty,
                        usage,
                    };
                }

                if let Some(value) = self.analyze_method_expression_with_extensions(&typ, expression) {
                    return value;
                }

                let struct_hint = SemanticRelatedInformation::new(
                    self.context.structure(*base).name.range(),
                    SemanticRelatedMessage::StructureDefinedHere { name: self.context.structure(*base).name.value().clone() }
                );

                let args: Vec<SemanticType> = expression.call.arguments.iter().map(|x| self.analyze_expression(x, &SemanticTypeResolution::default()).ty).collect();
                let create_method_action = self.create_action_create_method(self.context.structure(*base), expression, &args);
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

                let args: Vec<SemanticType> = expression.call.arguments.iter().map(|x| self.analyze_expression(x, &SemanticTypeResolution::default()).ty).collect();
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
        let x = self.context.scope.iter().rev().flat_map(|x| &x.extensions)
            .map(|x| &self.context.extensions[x.id()])
            .filter(|e| e.is_for_type(typ))
            .filter_map(|e| e.methods.get(&expression.method_name))
            .map(|method| {
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
                let name = method.name().clone();
                (return_type, usage, name, local_reference)
            })
            .next()?;

        trace!("Expr={}", expression.method_name.value());
        let (return_type, usage, name, local_reference) = x;
        self.analyze_function_parameters(name, local_reference, &expression.call, Some(typ));

        if return_type.is_null() {
            return Some(SemanticValue::null());
        }

        Some(SemanticValue {
            ty: return_type,
            usage,
        })
    }

    fn analyze_attributes_for_statement(&mut self, statement: &Statement) {
        if statement.kind.is_function() {
            return;
        }

        for attribute in &statement.attributes {
            self.diagnostics.create(|| SemanticDiagnostic::new(
                attribute.name.range(),
                SemanticDiagnosticKind::UnknownAttribute { name: attribute.name.value().as_str(), range: attribute.range() },
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
                        SemanticDiagnosticKind::UnknownAttribute { name: attribute.name.as_str(), range: attribute.range() },
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
                BabbelaarCodeActionType::RemoveAttribute { name: attr.name.as_str() },
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

        let mut func = func.extern_function.lock().unwrap();

        if func.is_some() {
            let diag = SemanticDiagnostic::new(
                attr.name.range().as_full_line(),
                SemanticDiagnosticKind::AttributeExternOnlyOnce,
            );
            let diag = diag.with_action(BabbelaarCodeAction::new(
                BabbelaarCodeActionType::RemoveAttribute { name: attr.name.as_str() },
                vec![FileEdit::new(attr.name.range().as_full_line(), "")],
            ));
            self.diagnostics.create(|| diag);
            return;
        }

        *func = Some(extern_func);
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

    fn analyze_attribute_var_args(&mut self, statement: &Statement, attr: &Attribute) {
        let StatementKind::Function(function) = &statement.kind else {
            let diag = SemanticDiagnostic::new(
                attr.name.range().as_full_line(),
                SemanticDiagnosticKind::AttributeExternOnlyOnFunctions,
            );
            let diag = diag.with_action(BabbelaarCodeAction::new(
                BabbelaarCodeActionType::RemoveAttribute { name: attr.name.as_str() },
                vec![FileEdit::new(attr.name.range().as_full_line(), "")],
            ));
            self.diagnostics.create(|| diag);
            return;
        };

        if function.body.is_some() {
            let diag = SemanticDiagnostic::new(
                attr.name.range().as_full_line(),
                SemanticDiagnosticKind::AttributeVarArgsOnlyOnFunctionsWithoutBody,
            );
            self.diagnostics.create(|| diag);
        }

        let Some(func) = self.context.current().get_function_mut(&function.name) else {
            log::warn!("Expected function '{}' to be defined earlier", function.name.value());
            return;
        };

        if func.extern_function.lock().unwrap().is_some() {
            let diag = SemanticDiagnostic::new(
                attr.name.range().as_full_line(),
                SemanticDiagnosticKind::AttributeExternOnlyOnce,
            );
            let diag = diag.with_action(BabbelaarCodeAction::new(
                BabbelaarCodeActionType::RemoveAttribute { name: attr.name.as_str() },
                vec![FileEdit::new(attr.name.range().as_full_line(), "")],
            ));
            self.diagnostics.create(|| diag);
            return;
        }

        func.has_variable_arguments.store(true, Ordering::SeqCst);
    }

    fn analyze_attributes_for_field(&mut self, structure: StructureId, field: SemanticFieldId) {
        let field = self.context.field(structure, field);
        for attribute in &field.attributes {
            self.diagnostics.create(|| SemanticDiagnostic::new(
                attribute.name.range(),
                SemanticDiagnosticKind::UnknownAttribute { name: attribute.name.as_str(), range: attribute.range() },
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

        let structure = self.context.structure(*structure);
        let (add_location, indent) = self.calculate_new_field_or_method_location(structure);

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
    fn create_actions_extend_structure_with_interface_by_name(&self, structure: StructureId, file_id: FileId, name: &'static str) -> Option<BabbelaarCodeAction> {
        let structure = self.context.structure(structure);
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
                let extension = &self.context.extensions[extension.id()];
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
                let ext = &self.context.extensions[ext.id()];
                if ext.range.file_id() == file_id {
                    last_extension = Some(ext.range);
                }
            }

            let last_structure_definition = scope.structures.values()
                    .copied()
                    .map(|id| self.context.structure(id).right_curly_range)
                    .max();

            if let Some(structure) = last_structure_definition {
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

            let ty = self.analyze_expression(arg, &SemanticTypeResolution::default()).ty;

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

        for scope in self.context.scope.iter() {
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
            Expression::Primary(PrimaryExpression::Reference(reference, _)) => Some(reference.value().clone()),
            Expression::Primary(PrimaryExpression::ReferencePath(path)) => Some(path.base.value().clone()),
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
        for scope in &self.context.scope.all_scopes {
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

                if name.is_empty() {
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

        let index_value = self.analyze_expression(expression, &SemanticTypeResolution::with_getal_type_hints());
        if !index_value.ty.is_primitive_number() {
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
                SemanticDiagnostic::new(assign.kind.range(), SemanticDiagnosticKind::IncompatibleAssignmentTypes)
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

    fn build_structure_body_based_on_instantiation(&self, instantiation: Option<(&StructureInstantiationExpression, &[SemanticType])>) -> String {
        let Some((instantiation, field_types)) = instantiation else {
            log::warn!("Kan structuurlichaam op dit moment alleen bepalen vanuit een `nieuw`-expressie");
            return String::new();
        };

        let mut body = String::new();

        for ((idx, field), field_ty) in instantiation.fields.iter().enumerate().zip(field_types.iter()) {
            if idx != 0 {
                body += "\n";
            }

            body += "    veld ";
            body += &field.name;
            body += ": ";
            body += &field_ty.name();
            body += ",";
        }

        body
    }

    #[inline]
    fn emit_diagnostic<F: FnOnce(&Self) -> SemanticDiagnostic>(&self, f: F) {
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
            .map(|x| &self.context.extensions[x.id()])
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

    fn iter_extensions<'a>(&'a self, ty: &'a SemanticType) -> impl Iterator<Item = &'a SemanticExtension> + 'a {
        self.context.scope.iter()
            .rev()
            .flat_map(|x| &x.extensions)
            .map(|x| &self.context.extensions[x.id()])
            .filter(|x| x.ty == *ty)
    }

    fn analyze_path_expression(&mut self, path: &PathExpression) -> SemanticType {
        let mut current = CurrentPathReference::Root;

        let parts = path.parts.iter()
            .chain(std::iter::once(&path.base));

        let mut computed_parts = Vec::new();
        for part in parts {
            match self.analyze_path_expression_part(current, part) {
                Some(new_reference) => {
                    computed_parts.push(new_reference.kind());
                    self.track_new_path_segment(part, &new_reference);
                    current = new_reference;
                }

                None => {
                    return SemanticType::null();
                }
            }
        }

        match current {
            CurrentPathReference::Root => unreachable!(),

            CurrentPathReference::Ty { ty, .. } => {
                self.emit_diagnostic(|_| {
                    SemanticDiagnostic::new(path.range(), SemanticDiagnosticKind::PathStructureIsNotAnExpression {
                        name: ty.name(),
                    })
                });
                SemanticType::null()
            }

            CurrentPathReference::StaticMethod { reference, name, parent_type_name, .. } => {
                *path.computed.lock().unwrap() = Some(ComputedPathExpression {
                    name: BabString::new(format!("{parent_type_name}__{name}")),
                    return_type: reference.return_type(),
                    parts: computed_parts,
                });
                SemanticType::FunctionReference(reference)
            }
        }
    }

    fn analyze_path_expression_part(&mut self, current: CurrentPathReference, part: &Ranged<BabString>) -> Option<CurrentPathReference> {
        let name = part.value().clone();

        Some(match current {
            CurrentPathReference::Root => {
                let ty = self.context.scope.iter().rev()
                    .filter_map(|x| x.structures.get(&name))
                    .cloned()
                    .map(|base| SemanticType::Custom { base, parameters: Vec::new(), name: self.context.structure(base).name.clone() })
                    .next()
                    .or_else(|| {
                        Builtin::type_by_name(&name).map(|x| SemanticType::Builtin(x))
                    });

                let Some(ty) = ty else {
                    self.emit_diagnostic(|_| {
                        SemanticDiagnostic::new(part.range(), SemanticDiagnosticKind::UnknownPathPart { name })
                    });
                    return None;
                };

                if !ty.generic_type_names(&self.context).is_empty() {
                    self.emit_diagnostic(|_| {
                        SemanticDiagnostic::new(part.range(), SemanticDiagnosticKind::PathPartRequiresGenerics { name })
                    });
                    return None;
                }

                CurrentPathReference::Ty {
                    ty,
                    range: part.range(),
                }
            }

            CurrentPathReference::Ty { ty, range } => {
                let extension_method = self.iter_extensions(&ty).find_map(|x| x.methods.get(&name));

                if let Some(method) = extension_method {
                    if !method.is_static {
                        self.emit_diagnostic(|_| {
                            SemanticDiagnostic::new(part.range(), SemanticDiagnosticKind::PathMethodNotStatic {
                                name,
                                structure: ty.name(),
                            })
                        });
                        return None;
                    }

                    return Some(CurrentPathReference::StaticMethod {
                        reference: FunctionReference::Custom(Arc::clone(&method.function)),
                        name: method.name().clone(),
                        parent_type_name: ty.name(),
                    });
                }

                match &ty {
                    SemanticType::Custom { base, parameters, .. } => {
                        assert!(parameters.is_empty());

                        let Some(method) = self.context.structure(*base).methods.iter().find(|x| *x.name() == name).cloned() else {
                            self.emit_diagnostic(|_| {
                                SemanticDiagnostic::new(part.range(), SemanticDiagnosticKind::PathMethodNotFound {
                                    name,
                                    structure: ty.name(),
                                })
                            });
                            return None;
                        };

                        if !method.is_static {
                            self.emit_diagnostic(|_| {
                                SemanticDiagnostic::new(part.range(), SemanticDiagnosticKind::PathMethodNotStatic {
                                    name,
                                    structure: ty.name(),
                                })
                            });
                            return None;
                        }

                        CurrentPathReference::StaticMethod {
                            reference: FunctionReference::Custom(Arc::clone(&method.function)),
                            name: method.name().clone(),
                            parent_type_name: ty.name(),
                        }
                    }

                    _ => {
                        self.emit_diagnostic(|_| {
                            SemanticDiagnostic::new(range, SemanticDiagnosticKind::PathNotStructure { name: ty.name() })
                        });

                        return None;
                    }
                }
            }

            CurrentPathReference::StaticMethod { name: method_name, .. } => {
                self.emit_diagnostic(|_| {
                    SemanticDiagnostic::new(part.range(), SemanticDiagnosticKind::PathMethodCannotBeFurtherQualified {
                        name,
                        method: method_name,
                    })
                });
                return None;
            }
        })
    }

    fn track_new_path_segment(&mut self, part: &Ranged<BabString>, reference: &CurrentPathReference) {
        let Some(tracker) = &mut self.context.definition_tracker else {
            return;
        };

        match reference {
            CurrentPathReference::Root => unreachable!(),

            CurrentPathReference::StaticMethod { reference, .. } => {
                tracker.insert(part.range(), SemanticReference {
                    local_name: reference.name(),
                    local_kind: SemanticLocalKind::FunctionReference,
                    declaration_range: reference.declaration_range(),
                    typ: SemanticType::FunctionReference(reference.clone()),
                });
            }

            CurrentPathReference::Ty { ty, .. } => {
                tracker.insert(part.range(), SemanticReference {
                    local_name: ty.name(),
                    local_kind: SemanticLocalKind::StructureReference,
                    declaration_range: ty.declaration_range(),
                    typ: ty.clone(),
                });
            }
        }
    }
}

enum CurrentPathReference {
    Root,
    Ty {
        ty: SemanticType,
        range: FileRange,
    },
    StaticMethod {
        reference: FunctionReference,
        name: BabString,
        parent_type_name: BabString,
    },
}

impl CurrentPathReference {
    #[must_use]
    fn kind(&self) -> SemanticLocalKind {
        match self {
            Self::Root => unreachable!(),
            Self::Ty { .. } => SemanticLocalKind::StructureReference,
            Self::StaticMethod { .. } => SemanticLocalKind::FunctionReference,
        }
    }
}
