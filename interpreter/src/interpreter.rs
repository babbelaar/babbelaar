// Copyright (C) 2023 - 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{
    collections::HashMap,
    rc::Rc,
};

use babbelaar::*;

use crate::*;

pub struct Interpreter<'source_code> {
    functions: HashMap<FunctionId, Rc<FunctionStatement<'source_code>>>,
    scope: Scope,
}

impl<'source_code> Interpreter<'source_code> {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            scope: Scope::new_top_level(),
        }
    }

    pub fn execute(&mut self, statement: &Statement<'source_code>) {
        _ = self.execute_statement(statement);
    }

    fn execute_statement(&mut self, statement: &Statement<'source_code>) -> StatementResult {
        match &statement.kind {
            StatementKind::Expression(expression) => {
                self.execute_expression(expression);
                StatementResult::Continue
            }

            StatementKind::For(statement) => {
                self.execute_for_statement(statement)
            }

            StatementKind::Function(func) => {
                let id = FunctionId::from(func);
                self.functions.insert(id, Rc::new(func.clone()));
                self.scope.variables.insert(func.name.to_string(), Value::Function { name: func.name.to_string(), id });
                StatementResult::Continue
            }

            StatementKind::If(statement) => {
                self.execute_if_statement(statement)
            }

            StatementKind::Return(statement) => {
                let value = statement.expression.as_ref()
                    .map(|expr| self.execute_expression(expr));

                StatementResult::Return(value)
            }

            StatementKind::Variable(variable) => {
                let value = self.execute_expression(&variable.expression);
                self.scope.variables.insert(variable.name.to_string(), value);
                StatementResult::Continue
            }
        }
    }

    fn execute_expression(&mut self, expression: &Expression<'source_code>) -> Value {
        match expression {
            Expression::BiExpression(expr) => self.execute_bi_expression(expr),
            Expression::Postfix(expr) => self.execute_postfix_expression(expr),

            Expression::Primary(PrimaryExpression::Boolean(boolean)) => {
                Value::Bool(*boolean)
            }

            Expression::Primary(PrimaryExpression::Reference(reference)) => {
                self.scope.find(reference)
            }

            Expression::Primary(PrimaryExpression::IntegerLiteral(integer)) => {
                Value::Integer(*integer)
            }

            Expression::Primary(PrimaryExpression::StringLiteral(str)) => {
                Value::String(str.to_string())
            }

            Expression::Primary(PrimaryExpression::TemplateString{ parts }) => {
                let mut string = String::new();

                for part in parts {
                    match part {
                        TemplateStringExpressionPart::String(str) => {
                            string += str;
                        }

                        TemplateStringExpressionPart::Expression(expression) => {
                            string += &self.execute_expression(expression).to_string();
                        }
                    }
                }

                Value::String(string)
            }
        }
    }

    fn execute_for_statement(&mut self, statement: &ForStatement<'source_code>) -> StatementResult {
        let PrimaryExpression::IntegerLiteral(start) = *statement.range.start else {
            panic!("Invalid start");
        };

        let PrimaryExpression::IntegerLiteral(end) = *statement.range.end else {
            panic!("Invalid end");
        };

        self.scope = std::mem::take(&mut self.scope).push();

        for x in start..end {
            self.scope.variables.insert(statement.iterator_name.to_string(), Value::Integer(x));

            for statement in &statement.body {
                if let StatementResult::Return(value) = self.execute_statement(statement) {
                    return StatementResult::Return(value);
                }
            }
        }

        self.scope = std::mem::take(&mut self.scope).pop();

        StatementResult::Continue
    }

    fn execute_if_statement(&mut self, statement: &IfStatement<'source_code>) -> StatementResult {
        self.scope = std::mem::take(&mut self.scope).push();

        if !self.execute_expression(&statement.condition).is_true() {
            return StatementResult::Continue;
        }

        for statement in &statement.body {
            if let StatementResult::Return(value) = self.execute_statement(statement) {
                return StatementResult::Return(value);
            }
        }

        self.scope = std::mem::take(&mut self.scope).pop();

        StatementResult::Continue
    }

    fn execute_bi_expression(&mut self, expression: &BiExpression<'source_code>) -> Value {
        let lhs = self.execute_expression(&expression.lhs);
        let rhs = self.execute_expression(&expression.rhs);

        match *expression.operator {
            BiOperator::Add => self.execute_expression_add(lhs, rhs),
            BiOperator::Subtract => self.execute_bi_expression_numeric(lhs, rhs, |a, b| a - b),
            BiOperator::Multiply => self.execute_bi_expression_numeric(lhs, rhs, |a, b| a * b),
            BiOperator::Modulo => self.execute_bi_expression_numeric(lhs, rhs, |a, b| a % b),
            BiOperator::Divide => self.execute_bi_expression_numeric(lhs, rhs, |a, b| a / b),

            BiOperator::Comparison(comparison) => {
                Value::Bool(lhs.compare(&rhs, comparison))
            }
        }
    }

    fn execute_expression_add(&mut self, lhs: Value, rhs: Value) -> Value {
        match (&lhs, &rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs + rhs),
            (Value::String(lhs), Value::String(rhs)) => Value::String(format!("{lhs}{rhs}")),
            _ => panic!("Invalid operands for add: {lhs:?} and {rhs:?}"),
        }
    }

    fn execute_bi_expression_numeric(&self, lhs: Value, rhs: Value, f: impl FnOnce(i64, i64) -> i64) -> Value {
        match (&lhs, &rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(f(*lhs, *rhs)),
            _ => panic!("ICE: Invalid operands for numeric: {lhs:?} and {rhs:?}"),
        }
    }

    fn execute_function_call(&mut self, lhs: Value, func: &FunctionCallExpression<'source_code>) -> Value {
        let mut arguments: Vec<Value> = Vec::with_capacity(func.arguments.len());

        match lhs {
            Value::MethodReference { lhs, method } => {
                arguments.push(*lhs);
                for argument in &func.arguments {
                    arguments.push(self.execute_expression(argument));
                }

                (method.function)(self, arguments)
            }

            Value::Function { id, .. } => {
                for argument in &func.arguments {
                    arguments.push(self.execute_expression(argument));
                }

                self.execute_function_by_id(id, arguments).unwrap()
            }

            _ => panic!("Unexpected lhs: {lhs:#?}"),
        }
    }

    fn execute_function_by_id(&mut self, id: FunctionId, arguments: Vec<Value>) -> Option<Value> {
        if id.namespace == usize::MAX {
            let function = Builtin::FUNCTIONS[id.id];
            return Some((function.function)(self, arguments));
        }

        let func = self.functions.get(&id).cloned().expect("Invalid FunctionId");
        Some(self.execute_function(func, arguments))
    }

    fn execute_function(&mut self, func: Rc<FunctionStatement<'source_code>>, arguments: Vec<Value>) -> Value {
        self.scope = std::mem::take(&mut self.scope).push();

        for idx in 0..func.parameters.len() {
            let name = func.parameters[idx].name.to_string();
            self.scope.variables.insert(name, arguments[idx].clone());
        }

        for statement in &func.body {
            match self.execute_statement(statement) {
                StatementResult::Continue => (),
                StatementResult::Return(value) => {
                    self.scope = std::mem::take(&mut self.scope).pop();
                    return value.unwrap_or(Value::Null);
                }
            }
        }

        self.scope = std::mem::take(&mut self.scope).pop();
        Value::Null
    }

    fn execute_postfix_expression(&mut self, expression: &PostfixExpression<'source_code>) -> Value {
        let lhs = self.execute_expression(&expression.lhs);
        match &expression.kind {
            PostfixExpressionKind::Call(call) => self.execute_function_call(lhs, call),
            PostfixExpressionKind::Member(member) => self.execute_member_reference(lhs, member),
            PostfixExpressionKind::MethodCall(method) => self.execute_method_invocation(lhs, method),
        }
    }

    fn execute_member_reference(&mut self, lhs: Value, member: &Ranged<&'source_code str>) -> Value {
        _ = lhs;
        _ = member;
        todo!()
    }

    fn execute_method_invocation(&mut self, lhs: Value, expression: &MethodCallExpression<'source_code>) -> Value {
        let method = lhs.get_method(&expression.method_name).unwrap();
        self.execute_function_call(method, &expression.call)
    }
}

impl<'source_code> babbelaar::Interpreter for Interpreter<'source_code> {

}

#[must_use]
enum StatementResult {
    Continue,
    Return(Option<Value>),
}
