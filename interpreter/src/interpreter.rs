// Copyright (C) 2023 - 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{
    cell::RefCell, collections::HashMap, process::exit, rc::Rc, sync::Arc
};

use babbelaar::*;
use log::error;

use crate::*;

pub struct Interpreter<D>
        where D: Debugger {
    functions: HashMap<FunctionId, Arc<InterpreterFunction>>,
    structures: HashMap<StructureId, Rc<InterpreterStructure>>,
    debugger: D,
    scope: Scope,
    ffi: FFIManager,
}

impl<D> Interpreter<D>
        where D: Debugger {
    pub fn new(debugger: D) -> Self {
        Self {
            functions: HashMap::new(),
            structures: HashMap::new(),
            scope: Scope::new_top_level(),
            debugger,
            ffi: FFIManager::new(),
        }
    }

    pub fn execute_tree(&mut self, tree: &ParseTree) {
        for statement in tree.structures() {
            _ = self.execute_statement(statement);
        }

        for statement in tree.functions() {
            _ = self.execute_statement(statement);
        }

        for statement in tree.statements() {
            match self.execute_statement(statement) {
                StatementResult::Continue => continue,
                StatementResult::Return(..) => break,
            }
        }
    }

    pub fn execute(&mut self, statement: &Statement) {
        self.debugger.initialize(&InterpreterAdapter);
        _ = self.execute_statement(statement);
    }

    fn execute_statement(&mut self, statement: &Statement) -> StatementResult {
        self.debugger.on_statement(statement);
        match &statement.kind {
            StatementKind::Assignment(assignment) => {
                let new_value = self.execute_expression(&assignment.source);
                self.execute_assign(&assignment.destination, new_value);
                StatementResult::Continue
            }

            StatementKind::Expression(expression) => {
                self.execute_expression(expression);
                StatementResult::Continue
            }

            StatementKind::For(statement) => {
                self.execute_for_statement(statement)
            }

            StatementKind::Function(func) => {
                let id = FunctionId::from(func);
                self.functions.insert(id, Arc::new(InterpreterFunction {
                    attributes: statement.attributes.clone(),
                    function: func.clone(),
                }));
                self.scope.variables.insert(BabString::clone(&func.name), Value::Function { name: func.name.to_string(), id });
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

            StatementKind::Structure(structure) => {
                let id = StructureId::from(structure);

                let structure = Rc::new(InterpreterStructure {
                    structure: structure.clone(),
                    methods: structure.methods.iter()
                        .map(|x| Arc::new(
                            InterpreterFunction {
                                attributes: Vec::new(),
                                function: x.function.clone()
                            }
                        ))
                        .collect()
                });

                let prev = self.structures.insert(id, Rc::clone(&structure));
                assert!(prev.is_none(), "Illegal double value: {prev:#?} and now: {structure:#?}");
                self.scope.structures.insert(BabString::clone(structure.name()), structure);
                StatementResult::Continue
            }

            StatementKind::Variable(variable) => {
                let value = self.execute_expression(&variable.expression);
                self.scope.variables.insert(BabString::clone(&variable.name), value);
                StatementResult::Continue
            }
        }
    }

    pub fn execute_assign(&mut self, expression: &Expression, new_value: Value) {
        match expression {
            Expression::Primary(PrimaryExpression::Reference(reference)) => {
                if let Some(variable) = self.scope.find_mut(reference) {
                    *variable = new_value;
                    return;
                }
            }

            Expression::Postfix(postfix) => {
                let value = self.execute_expression(&postfix.lhs);
                match postfix.kind.value() {
                    PostfixExpressionKind::Member(member) => {
                        let Value::Object { fields, .. } = value else {
                            panic!("Cannot assign to non-Object Value");
                        };

                        let mut fields = fields.borrow_mut();
                        fields.insert(member.to_string(), new_value);
                        return;
                    }

                    PostfixExpressionKind::Subscript(..) => {
                        let value = self.execute_postfix_expression(postfix);
                        let Value::ArrayElementReference { array, index } = value else {
                            panic!("ICE: unexpected outcome of subscript postfix-expression: {value:#?}");
                        };

                        array.borrow_mut()[index] = new_value;
                        return;
                    }

                    _ => (),
                }
            }

            _ => (),
        }

        panic!("Invalid reference: {expression:#?}")
    }

    pub fn execute_expression(&mut self, expression: &Ranged<Expression>) -> Value {
        self.debugger.on_expression(expression);

        match expression.value() {
            Expression::BiExpression(expr) => self.execute_bi_expression(expr),
            Expression::Postfix(expr) => self.execute_postfix_expression(expr),
            Expression::Primary(expr) => self.execute_expression_primary(expr),
        }
    }

    fn execute_expression_primary(&mut self, expression: &PrimaryExpression) -> Value {
        match expression {
            PrimaryExpression::Boolean(boolean) => {
                Value::Bool(*boolean)
            }

            PrimaryExpression::Reference(reference) => {
                self.scope.find(reference)
            }

            PrimaryExpression::IntegerLiteral(integer) => {
                Value::Integer(*integer)
            }

            PrimaryExpression::StringLiteral(str) => {
                Value::String(str.to_string())
            }

            PrimaryExpression::StructureInstantiation(structure) => {
                Value::Object {
                    structure: *self.structures.iter()
                        .find(|(_, registered)| registered.name() == structure.name.value())
                        .unwrap_or_else(|| panic!("failed to find structure `{}`, structures: {:#?}", structure.name.value(), self.structures))
                        .0,
                    fields: Rc::new(RefCell::new(structure.fields.iter()
                        .map(|field| {
                            (field.name.to_string(), self.execute_expression(&field.value))
                        })
                        .collect()))
                }
            }

            PrimaryExpression::TemplateString{ parts } => {
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

            PrimaryExpression::ReferenceThis => {
                self.scope.this.clone().unwrap()
            }

            PrimaryExpression::Parenthesized(expression) => self.execute_expression(expression),

            PrimaryExpression::SizedArrayInitializer { typ, size } => {
                let size = self.execute_expression(&size);
                let Value::Integer(size) = size else {
                    panic!("Ongeldige opeensommingsgrootte: {size:#?}");
                };

                let (ty, default_value) = self.resolve_type(typ);

                Value::Array {
                    ty,
                    values: Rc::new(RefCell::new(vec![default_value; size as usize])),
                }
            }
        }
    }

    fn execute_for_statement(&mut self, statement: &ForStatement) -> StatementResult {
        let Value::Integer(start) = self.execute_expression_primary(&statement.range.start) else {
            panic!("Invalid start");
        };

        let Value::Integer(end) = self.execute_expression_primary(&statement.range.end) else {
            panic!("Invalid end");
        };

        self.scope = std::mem::take(&mut self.scope).push();

        for x in start..end {
            self.scope.variables.insert(BabString::clone(&statement.iterator_name), Value::Integer(x));

            for statement in &statement.body {
                if let StatementResult::Return(value) = self.execute_statement(statement) {
                    return StatementResult::Return(value);
                }
            }
        }

        self.scope = std::mem::take(&mut self.scope).pop();

        StatementResult::Continue
    }

    fn execute_if_statement(&mut self, statement: &IfStatement) -> StatementResult {
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

    fn execute_bi_expression(&mut self, expression: &BiExpression) -> Value {
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

    fn execute_function_call(&mut self, lhs: Value, func: &FunctionCallExpression) -> Value {
        let mut arguments: Vec<Value> = Vec::with_capacity(func.arguments.len());
        for argument in &func.arguments {
            arguments.push(self.execute_expression(argument));
        }

        match lhs {
            Value::MethodReference { lhs, method } => {
                (method.function)(self, arguments, Some(*lhs))
            }

            Value::MethodIdReference { lhs, method } => {
                let structure = self.structures.get(&method.structure).unwrap();
                let method = Arc::clone(&structure.methods[method.index]);

                self.execute_function(method.clone(), arguments, Some(*lhs))
            }

            Value::Function { id, .. } => {
                self.execute_function_by_id(id, arguments, None, func.token_left_paren).unwrap()
            }

            _ => panic!("Unexpected lhs: {lhs:#?}"),
        }
    }

    fn execute_function_by_id(&mut self, id: FunctionId, arguments: Vec<Value>, this: Option<Value>, caller_location: FileRange) -> Option<Value> {
        if id.namespace == usize::MAX {
            let function = Builtin::FUNCTIONS[id.id];

            self.debugger.enter_function(DebuggerFunction {
                ty: DebuggerFunctionType::Normal,
                name: function.name,
                caller_location,
                callee_location: None,
            }, &arguments);

            let value = (function.function)(self, arguments, this);

            self.debugger.leave_function(DebuggerFunction {
                ty: DebuggerFunctionType::Normal,
                name: function.name,
                caller_location,
                callee_location: None,
            });

            Some(value)
        } else {
            let function = self.functions.get(&id).cloned().expect("Invalid FunctionId");
            let func = &function.function;

            self.debugger.enter_function(DebuggerFunction {
                ty: DebuggerFunctionType::Normal,
                name: func.name.value(),
                caller_location,
                callee_location: Some(func.name.range()),
            }, &arguments);

            let value = self.execute_function(Arc::clone(&function), arguments, this);

            self.debugger.leave_function(DebuggerFunction {
                ty: DebuggerFunctionType::Normal,
                name: func.name.value(),
                caller_location,
                callee_location: Some(func.name.range()),
            });

            Some(value)
        }
    }

    fn execute_function(&mut self, func: Arc<InterpreterFunction>, arguments: Vec<Value>, this: Option<Value>) -> Value {
        for attrib in &func.attributes {
            if attrib.name.value() == "uitheems" {
                debug_assert!(this.is_none());
                return self.ffi.execute(attrib, arguments);
            }
        }

        self.scope = std::mem::take(&mut self.scope).push_function(this);

        for idx in 0..func.function.parameters.len() {
            let name = BabString::clone(&func.function.parameters[idx].name);
            self.scope.variables.insert(name, arguments[idx].clone());
        }

        for statement in func.function.body.as_ref().unwrap() {
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

    fn execute_postfix_expression(&mut self, expression: &PostfixExpression) -> Value {
        let lhs = self.execute_expression(&expression.lhs);
        match expression.kind.value() {
            PostfixExpressionKind::Call(call) => self.execute_function_call(lhs, call),
            PostfixExpressionKind::Member(member) => self.execute_member_reference(lhs, member),
            PostfixExpressionKind::MethodCall(method) => self.execute_method_invocation(lhs, method),
            PostfixExpressionKind::Subscript(subscript) => self.execute_subscript(lhs, subscript),
        }
    }

    fn execute_member_reference(&mut self, lhs: Value, member: &Ranged<BabString>) -> Value {
        let Value::Object { fields, .. } = lhs else {
            todo!("Invalid member reference: {member:?} for value {lhs:#?}");
        };

        let fields = fields.borrow();
        let name = member.value().as_str();
        match fields.get(name) {
            Some(field) => field.clone(),
            None => {
                panic!("Kon veld `{name}` niet vinden binnen {fields:#?}");
            }
        }
    }

    fn execute_method_invocation(&mut self, lhs: Value, expression: &MethodCallExpression) -> Value {
        let method = self.get_method(&lhs, &expression.method_name).unwrap();
        self.execute_function_call(method, &expression.call)
    }

    fn get_method(&self, value: &Value, method_name: &str) -> Option<Value> {
        match value.typ() {
            ValueType::Array(..) => (),

            ValueType::Builtin(builtin) => {
                for method in builtin.methods() {
                    if method.name == method_name {
                        return Some(Value::MethodReference {
                            lhs: Box::new(value.clone()),
                            method,
                        });
                    }
                }
            }

            ValueType::Structure(structure_id) => {
                let structure = self.structures.get(&structure_id).expect("illegal StructureId");
                for (index, method) in structure.methods.iter().enumerate() {
                    if *method.function.name == method_name {
                        return Some(Value::MethodIdReference {
                            lhs: Box::new(value.clone()),
                            method: MethodId {
                                structure: structure_id,
                                index,
                            }
                        });
                    }
                }
            }
        }

        None
    }

    fn resolve_type(&self, typ: &Type) -> (ValueType, Value) {
        assert!(typ.qualifiers.len() == 0);
        match typ.specifier.value() {
            TypeSpecifier::BuiltIn(ty) => {
                let default_value = match ty {
                    BuiltinType::Bool => Value::Bool(false),
                    BuiltinType::G32 => Value::Integer(0),
                    BuiltinType::Null => Value::Null,
                    BuiltinType::Slinger => Value::String(String::new()),
                };

                (ValueType::Builtin(*ty), default_value)
            }

            TypeSpecifier::Custom { .. } => todo!(),
        }
    }

    fn execute_subscript(&mut self, lhs: Value, subscript: &Ranged<Expression>) -> Value {
        let subscript = self.execute_expression(&subscript);

        let Value::Integer(index) = subscript else {
            panic!("ICE: subscript index is not a number");
        };

        let Value::Array { values: array, .. } = lhs else {
            panic!("ICE: subscript operand is not an array");
        };

        let array_size = array.borrow().len();
        if index < 0 || index as usize >= array_size {
            let error = RuntimeError::array_out_of_bounds(array_size, index);
            self.debugger.on_runtime_error(&error);
            error!("Fout: {error}");
            exit(1);
        }

        Value::ArrayElementReference {
            array,
            index: index as usize,
        }
    }
}

impl<D> babbelaar::Interpreter for Interpreter<D>
        where D: Debugger {

}

struct InterpreterAdapter;

impl babbelaar::Interpreter for InterpreterAdapter {}

#[must_use]
enum StatementResult {
    Continue,
    Return(Option<Value>),
}
