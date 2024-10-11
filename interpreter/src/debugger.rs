// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use babbelaar::{Expression, FileRange, Interpreter, Ranged, Statement, Value};

use crate::RuntimeError;

#[derive(Debug, Clone, Copy)]
pub struct DebuggerFunction<'interpreter> {
    pub ty: DebuggerFunctionType,
    pub name: &'interpreter str,
    pub caller_location: FileRange,
    pub callee_location: Option<FileRange>,
}

#[derive(Debug, Clone, Copy)]
pub enum DebuggerFunctionType {
    Program,
    Native,
    Normal,
}

pub trait Debugger {
    fn initialize(&mut self, interpreter: &dyn Interpreter) { _ = interpreter }
    fn on_exit(&mut self) {}

    fn on_statement(&mut self, statement: &Statement) { _ = statement }
    fn on_expression(&mut self, expression: &Ranged<Expression>) { _ = expression }

    fn enter_function(&mut self, function: DebuggerFunction<'_>, args: &[Value]) {
        _ = function;
        _ = args;
    }

    fn leave_function(&mut self, function: DebuggerFunction<'_>) {
        _ = function;
    }

    fn on_runtime_error(&mut self, error: &RuntimeError) {
        _ = error;
    }
}

impl Debugger for () {
}
