// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{borrow::Cow, collections::HashMap};

use strum::AsRefStr;
use thiserror::Error;

use crate::{BiExpression, Builtin, BuiltinFunction, Expression, FileRange, ForStatement, FunctionCallExpression, FunctionStatement, Parameter, PrimaryExpression, ReturnStatement, Statement, StatementKind, TemplateStringExpressionPart};

pub struct SemanticAnalyzer<'source_code> {
    context: SemanticContext<'source_code>,
    diagnostics: Vec<SemanticDiagnostic<'source_code>>,
}

impl<'source_code> SemanticAnalyzer<'source_code> {
    #[must_use]
    pub fn new() -> Self {
        Self {
            context: SemanticContext {
                scope: vec![SemanticScope::default()],
            },
            diagnostics: Vec::new(),
        }
    }

    pub fn analyze_expression(&mut self, expression: &'source_code Expression<'source_code>) {
        match expression {
            Expression::BiExpression(bi) => self.analyze_bi_expression(bi),
            Expression::Function(function) => self.analyze_function_call_expression(function),
            Expression::Primary(primary) => self.analyze_primary_expression(primary),
        }
    }

    pub fn analyze_function(&mut self, function: &'source_code FunctionStatement<'source_code>) {
        self.context.scope.last_mut().unwrap().functions.push(SemanticFunction {
            name: &function.name,
            parameters: &function.parameters,
        });

        self.context.scope.push(SemanticScope::default());
        let scope = self.context.scope.last_mut().expect("we just pushed a scope");

        for param in &function.parameters {
            scope.locals.insert(&param.name, SemanticLocal {
                kind: SemanticLocalKind::Parameter,
                declaration_range: param.name.range(),
            });
        }

        for statement in &function.body {
            self.analyze_statement(statement);
        }

        self.context.scope.pop().expect("we pushed a scope, we should pop the scope");
    }

    pub fn analyze_statement(&mut self, statement: &'source_code Statement<'source_code>) {
        match &statement.kind {
            StatementKind::Expression(expr) => self.analyze_expression(expr),
            StatementKind::For(statement) => self.analyze_for_statement(statement),
            StatementKind::Function(function) => self.analyze_function(function),
            StatementKind::Return(function) => self.analyze_return_statement(function),
        }
    }

    fn analyze_for_statement(&mut self, statement: &'source_code ForStatement<'source_code>) {
        self.context.scope.push(SemanticScope::default());

        let scope = self.context.scope.last_mut().expect("we just pushed a scope");
        scope.locals.insert(&statement.iterator_name, SemanticLocal {
            kind: SemanticLocalKind::Iterator,
            declaration_range: statement.iterator_name.range(),
        });

        for statement in &statement.body {
            self.analyze_statement(statement);
        }

        self.context.scope.pop().expect("we pushed a scope, we should pop the scope");
    }

    fn analyze_return_statement(&mut self, statement: &'source_code ReturnStatement<'source_code>) {
        if let Some(expression) = &statement.expression {
            self.analyze_expression(expression);
        }

        // TODO analyze return type
    }

    fn analyze_bi_expression(&mut self, expression: &'source_code BiExpression<'source_code>) {
        self.analyze_expression(&expression.lhs);
        self.analyze_expression(&expression.rhs);

        // TODO analyze types & combinations
    }

    fn analyze_function_call_expression(&mut self, expression: &'source_code FunctionCallExpression<'source_code>) {
        let function_name = expression.function_identifier.value();
        let Some(function) = self.find_function(&function_name) else {
            self.diagnostics.push(SemanticDiagnostic {
                range: expression.function_identifier.range(),
                kind: SemanticDiagnosticKind::InvalidFunctionReference { name: &expression.function_identifier }
            });
            return;
        };

        let param_count = function.parameter_count();
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

        for arg in &expression.arguments {
            self.analyze_expression(arg);
        }

        // TODO type check arguments vs parameters
    }

    fn analyze_primary_expression(&mut self, expression: &'source_code PrimaryExpression<'source_code>) {
        match expression {
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
            }

            PrimaryExpression::Reference(reference) => {
                let Some(local) = self.find_local_by_name(|name| &name == reference.value()) else {
                    self.diagnostics.push(SemanticDiagnostic {
                        range: reference.range(),
                        kind: SemanticDiagnosticKind::InvalidIdentifierReference { identifier: &reference }
                    });
                    return;
                };

                // TODO
                _ = local;
            }

            _ => (), // TODO
        }
    }

    fn find_local_by_name<'this, P>(&'this self, predicate: P) -> Option<&SemanticLocal>
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

    pub fn find_function_by_name<'this, P>(&'this self, predicate: P) -> Option<FunctionReference<'this, 'source_code>>
            where P: Fn(&str) -> bool {
        for scope in self.context.scope.iter().rev() {
            for func in &scope.functions {
                if predicate(func.name) {
                    return Some(FunctionReference::Custom(func));
                }
            }
        }

        for func in Builtin::FUNCTIONS {
            if predicate(func.name) {
                return Some(FunctionReference::Builtin(func));
            }
        }

        None
    }

    pub fn find_function<'this>(&'this self, name: &str) -> Option<FunctionReference<'this, 'source_code>> {
        self.find_function_by_name(|func| func == name)
    }

    #[must_use]
    pub fn into_diagnostics(self) -> Vec<SemanticDiagnostic<'source_code>> {
        self.diagnostics
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
}

impl<'source_code> SemanticDiagnosticKind<'source_code> {
    pub fn name(&self) -> &str {
        self.as_ref()
    }
}

struct SemanticContext<'source_code> {
    scope: Vec<SemanticScope<'source_code>>,
}

#[derive(Default)]
struct SemanticScope<'source_code> {
    functions: Vec<SemanticFunction<'source_code>>,
    locals: HashMap<&'source_code str, SemanticLocal>,
}

#[derive(Debug)]
pub struct SemanticFunction<'source_code> {
    name: &'source_code str,
    parameters: &'source_code [Parameter<'source_code>],
    // type ...
}

struct SemanticLocal {
    kind: SemanticLocalKind,
    declaration_range: FileRange,
    // type ...
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SemanticLocalKind {
    Parameter,
    Iterator,
}

pub enum FunctionReference<'this, 'source_code> {
    Builtin(&'static BuiltinFunction),
    Custom(&'this SemanticFunction<'source_code>),
}

impl<'this, 'source_code> FunctionReference<'this, 'source_code> {
    #[must_use]
    pub fn parameter_count(&self) -> usize {
        match self {
            Self::Builtin(func) => func.parameters.len(),
            Self::Custom(func) => func.parameters.len(),
        }
    }

    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Builtin(func) => func.name,
            Self::Custom(func) => func.name,
        }
    }

    #[must_use]
    pub fn documentation(&self) -> Option<&str> {
        match self {
            Self::Builtin(func) => Some(func.documentation),
            Self::Custom(..) => None,
        }
    }

    #[must_use]
    pub fn lsp_completion(&self) -> Cow<'_, str> {
        if let Some(completion) = self.lsp_completion_raw() {
            return Cow::Borrowed(completion);
        }

        let mut insert = format!("{}(", self.name());

        for i in 0..self.parameter_count() {
            let comma = if i == 0 { "" } else { ", " };
            insert += &format!("{comma}${}", i + 1);
        }

        insert += ")$0";
        Cow::Owned(insert)
    }

    #[must_use]
    fn lsp_completion_raw(&self) -> Option<&str> {
        match self {
            Self::Builtin(func) => func.lsp_completion,
            Self::Custom(..) => None,
        }
    }
}
