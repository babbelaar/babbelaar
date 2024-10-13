// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::sync::Arc;

use babbelaar::*;
use tower_lsp::lsp_types::{InlayHint, InlayHintKind, InlayHintParams};

use crate::{BabbelaarLspResult as Result, Backend, Converter};

pub struct InlayHintsEngine {
    target_file: FileId,
    converter: Converter,
    params: InlayHintParams,
    analyzer: Arc<SemanticAnalyzer>,
    hints: Vec<InlayHint>,
}

impl InlayHintsEngine {
    pub async fn hint(server: &Backend, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let (analyzer, source_code) = server.with_semantics(&params.text_document, |analyzer, source_code| {
            Ok((Arc::clone(&analyzer), source_code.clone()))
        }).await?;

        let mut this = Self {
            target_file: source_code.file_id(),
            converter: server.converter(&source_code),
            params,
            hints: Vec::new(),
            analyzer: Arc::clone(&analyzer),
        };

        this.visit_syntax_tree(server).await?;
        this.visit_semantics(analyzer.as_ref());

        Ok(Some(this.hints))
    }

    async fn visit_syntax_tree(&mut self, server: &Backend) -> Result<()> {
        let document = self.params.text_document.clone();
        server.with_syntax(&document, |tree, _| {
            for statement in tree.all() {
                self.visit_statement(statement);
            }

            Ok(())
        }).await
    }

    fn visit_semantics(&mut self, analyzer: &SemanticAnalyzer) {
        for reference in analyzer.context.declaration_tracker.as_ref().unwrap_or(&Vec::new()) {
            self.visit_semantic_reference(reference);
        }
    }

    fn visit_semantic_reference(&mut self, reference: &SemanticReference) {
        if reference.declaration_range.file_id() != self.target_file {
            return;
        }

        match reference.local_kind {
            SemanticLocalKind::Method => return,
            SemanticLocalKind::Function => return,
            SemanticLocalKind::FunctionReference => return,
            SemanticLocalKind::FieldReference => return,
            SemanticLocalKind::StructureReference => return,
            SemanticLocalKind::Parameter => return,

            SemanticLocalKind::Iterator => (),
            SemanticLocalKind::Variable => (),
            SemanticLocalKind::ReferenceThis => (),
        }

        self.hints.push(InlayHint {
            position: self.converter.convert_position(reference.declaration_range.end()),
            label: format!(": {}", reference.typ).into(),
            kind: Some(InlayHintKind::TYPE),
            text_edits: None,
            tooltip: None,
            padding_left: None,
            padding_right: None,
            data: None,
        });
    }

    fn visit_statement(&mut self, statement: &Statement) {
        match &statement.kind {
            StatementKind::Assignment(assignment) => self.visit_assignment(assignment),
            StatementKind::Expression(expression) => self.visit_expression(expression),
            StatementKind::Extension(extension) => self.visit_extension(extension),
            StatementKind::Function(function) => self.visit_function(function),
            StatementKind::For(for_statement) => self.visit_for_statement(for_statement),
            StatementKind::If(if_statement) => self.visit_if_statement(if_statement),
            StatementKind::Return(return_statement) => self.visit_return_statement(return_statement),
            StatementKind::Structure(structure) => self.visit_structure(structure),
            StatementKind::Variable(variable) => self.visit_variable(variable),
        }
    }

    fn visit_assignment(&mut self, assignment: &AssignStatement) {
        self.visit_expression(&assignment.destination);
        self.visit_expression(&assignment.source);
    }

    fn visit_expression(&mut self, expression: &Expression) {
        match expression {
            Expression::BiExpression(expression) => self.visit_bi_expression(expression),
            Expression::Postfix(expression) => self.visit_postfix_expression(expression),
            Expression::Primary(expression) => self.visit_primary_expression(expression),
        }
    }

    fn visit_primary_expression(&mut self, expression: &PrimaryExpression) {
        match expression {
            PrimaryExpression::StructureInstantiation(instantiation) => {
                self.visit_structure_instantiation(instantiation);
            }

            PrimaryExpression::SizedArrayInitializer { size, .. } => {
                self.visit_expression(&size);
            }

            PrimaryExpression::Parenthesized(expression) => {
                self.visit_expression(&expression);
            }

            PrimaryExpression::TemplateString { parts } => {
                for part in parts {
                    if let TemplateStringExpressionPart::Expression(expression) = part {
                        self.visit_expression(&expression);
                    }
                }
            }

            _ => (),
        }
    }

    fn visit_bi_expression(&mut self, expression: &BiExpression) {
        self.visit_expression(&expression.lhs);
        self.visit_expression(&expression.rhs);
    }

    fn visit_postfix_expression(&mut self, expression: &PostfixExpression) {
        self.visit_expression(&expression.lhs);

        match expression.kind.value() {
            PostfixExpressionKind::Call(call) => {
                self.visit_function_call(expression, call);
            }

            PostfixExpressionKind::Member(..) => (),

            PostfixExpressionKind::MethodCall(method_call) => {
                if let Some(ty) = &self.analyzer.context.value_type_tracker.as_ref().and_then(|x| x.get(&expression.lhs.range())) {
                    let SemanticType::Custom { base, .. } = ty else { return };
                    let Some(method) = base.methods.iter().find(|x| x.function.name.value() == method_call.method_name.value()) else { return };

                    for (argument, parameter) in method_call.call.arguments.iter().zip(method.function.parameters.iter()) {
                        self.hints.push(InlayHint {
                            position: self.converter.convert_position(argument.range().start()),
                            label: format!("{}: ", parameter.name.value()).into(),
                            kind: Some(InlayHintKind::PARAMETER),
                            text_edits: None,
                            tooltip: None,
                            padding_left: None,
                            padding_right: None,
                            data: None,
                        });
                    }
                }
            }

            PostfixExpressionKind::Subscript(subscript) => {
                self.visit_expression(&subscript);
            }
        }
    }

    fn visit_extension(&mut self, extension: &ExtensionStatement) {
        for method in &extension.methods {
            self.visit_function(&method.function);
        }
    }

    fn visit_function(&mut self, function: &FunctionStatement) {
        if let Some(body) = &function.body {
            for statement in body {
                self.visit_statement(statement);
            }
        }
    }

    fn visit_for_statement(&mut self, for_statement: &ForStatement) {
        match for_statement.iterable.value() {
            ForIterableKind::Expression(expression) => self.visit_expression(&expression),
            ForIterableKind::Range(range) => {
                self.visit_expression(&range.start);
                self.visit_expression(&range.end);
            }
        }

        for statement in &for_statement.body {
            self.visit_statement(statement);
        }
    }

    fn visit_if_statement(&mut self, if_statement: &IfStatement) {
        self.visit_expression(&if_statement.condition);

        for statement in &if_statement.body {
            self.visit_statement(statement);
        }
    }

    fn visit_return_statement(&mut self, return_statement: &ReturnStatement) {
        if let Some(expression) = &return_statement.expression {
            self.visit_expression(&expression);
        }
    }

    fn visit_structure(&mut self, structure: &Structure) {
        for field in &structure.fields {
            if let Some(expression) = &field.default_value {
                self.visit_expression(&expression);
            }
        }

        for method in &structure.methods {
            self.visit_function(&method.function);
        }
    }

    fn visit_variable(&mut self, variable: &VariableStatement) {
        self.visit_expression(&variable.expression);
    }

    fn visit_structure_instantiation(&mut self, instantiation: &StructureInstantiationExpression) {
        for field in &instantiation.fields {
            self.visit_expression(&field.value);
        }
    }

    fn visit_function_call(&mut self, expression: &PostfixExpression, call: &FunctionCallExpression) {
        for param in &call.arguments {
            self.visit_expression(&param);
        }

        let Some(tracker) = &self.analyzer.context.value_type_tracker else { return };
        let Some(function) = tracker.get(&expression.lhs.range()) else { return };

        log::info!("FUNC: {function:#?}");

        match function {
            SemanticType::Function(func) => {
                for (argument, parameter) in call.arguments.iter().zip(func.parameters.iter()) {
                    self.hints.push(InlayHint {
                        position: self.converter.convert_position(argument.range().start()),
                        label: format!("{}: ", parameter.name.value()).into(),
                        kind: Some(InlayHintKind::PARAMETER),
                        text_edits: None,
                        tooltip: None,
                        padding_left: None,
                        padding_right: None,
                        data: None,
                    });
                }
            }

            _ => (),
        }
    }
}
