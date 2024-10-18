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
    structures: HashMap<StructureId, InterpreterStructure>,
    interfaces: HashMap<InterfaceId, InterpreterInterface>,
    debugger: D,
    scope: Scope,
    ffi: FFIManager,
    methods: HashMap<MethodId, Arc<InterpreterFunction>>,
}

impl<D> Interpreter<D>
        where D: Debugger {
    pub fn new(debugger: D) -> Self {
        Self {
            functions: HashMap::new(),
            structures: create_top_level_structures(),
            interfaces: HashMap::new(),
            scope: Scope::new_top_level(),
            debugger,
            ffi: FFIManager::new(),
            methods: HashMap::new(),
        }
    }

    pub fn execute_tree(&mut self, tree: &ParseTree) {
        for statement in tree.structures() {
            _ = self.execute_statement(statement);
        }

        for statement in tree.interfaces() {
            _ = self.execute_statement(statement);
        }

        for statement in tree.extensions() {
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

            StatementKind::Extension(ext) => {
                let fqn = ext.type_specifier.unqualified_name();
                let Some(structure_id) = self.scope.find_structure_id(&fqn) else {
                    panic!("Kon structuur met FQN `{fqn}` niet vinden!");
                };

                let structure = self.structures.get_mut(&structure_id).unwrap();

                for method in &ext.methods {
                    let id = MethodId { owner: structure_id.into(), index: structure.method_ids.len() };
                    let prev = structure.extension_ids.insert(BabString::clone(&method.function.name), id);
                    debug_assert!(prev.is_none());

                    self.methods.insert(id, Arc::new(InterpreterFunction {
                        attributes: AttributeList::new(),
                        function: method.function.clone(),
                    }));
                }

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

            StatementKind::Interface(ast_interface) => {
                let id = InterfaceId::from(ast_interface);

                let mut interface = InterpreterInterface {
                    interface: ast_interface.clone(),
                    method_ids: HashMap::new(),
                };

                for method in &ast_interface.methods {
                    let func = Arc::new(
                        InterpreterFunction {
                            attributes: AttributeList::new(),
                            function: method.function.clone(),
                        }
                    );

                    let id = MethodId { owner: id.into(), index: interface.method_ids.len() };
                    self.methods.insert(id, func);
                    interface.method_ids.insert(BabString::clone(&method.function.name), id);
                }

                let prev = self.scope.interfaces.insert(BabString::clone(interface.name()), id);
                debug_assert!(prev.is_none(), "Illegal double value: {prev:#?}");

                let prev = self.interfaces.insert(id, interface);
                debug_assert!(prev.is_none(), "Illegal double value: {prev:#?}");

                StatementResult::Continue
            }

            StatementKind::Return(statement) => {
                let value = statement.expression.as_ref()
                    .map(|expr| self.execute_expression(expr));

                StatementResult::Return(value)
            }

            StatementKind::Structure(ast_structure) => {
                let id = StructureId::from(ast_structure);

                let mut structure = InterpreterStructure {
                    structure: ast_structure.clone(),
                    method_ids: HashMap::new(),
                    extension_ids: HashMap::new(),
                };

                for method in &ast_structure.methods {
                    let func = Arc::new(
                        InterpreterFunction {
                            attributes: AttributeList::new(),
                            function: method.function.clone(),
                        }
                    );

                    let id = MethodId { owner: id.into(), index: structure.method_ids.len() };
                    self.methods.insert(id, func);
                    structure.method_ids.insert(BabString::clone(&method.function.name), id);
                }

                let prev = self.scope.structures.insert(BabString::clone(structure.name()), id);
                debug_assert!(prev.is_none(), "Illegal double value: {prev:#?}");

                let prev = self.structures.insert(id, structure);
                debug_assert!(prev.is_none(), "Illegal double value: {prev:#?}");

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

            PrimaryExpression::CharacterLiteral(char) => {
                Value::Character(*char)
            }

            PrimaryExpression::StringLiteral(str) => {
                Value::String(str.to_string())
            }

            PrimaryExpression::StructureInstantiation(instantiation) => {
                let (id, structure) = self.structures.iter()
                        .find(|(_, registered)| registered.name() == instantiation.name.value())
                        .unwrap_or_else(|| panic!("failed to find structure `{}`, structures: {:#?}", instantiation.name.value(), self.structures))
                        .clone();

                let id = *id;

                self.scope = std::mem::take(&mut self.scope).push();

                for (generic_decl, generic_def) in structure.structure.generic_types.iter().zip(instantiation.type_parameters.iter()) {
                    let generic_type = self.resolve_type(&generic_def).0;
                    self.scope.generic_types.insert(generic_decl.value().clone(), generic_type);
                }

                let fields = structure.structure.fields.iter()
                    .map(|field| {
                        let instantiation = instantiation.fields.iter().find(|x| x.name.value() == field.name.value());

                        let expression = if let Some(instantiation) = instantiation {
                            &instantiation.value
                        } else {
                            field.default_value.as_ref().unwrap()
                        };

                        (field.name.to_string(), expression.clone())
                    })
                    .collect::<Vec<_>>()
                    .into_iter()
                    .map(|(name, expression)| {
                        let value = self.execute_expression(&expression);
                        (name, value)
                    })
                    .collect();

                let generic_types = std::mem::take(&mut self.scope.generic_types);
                self.scope = std::mem::take(&mut self.scope).pop();

                Value::Object {
                    structure: id,
                    generic_types,
                    fields: Rc::new(RefCell::new(fields))
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
        match statement.iterable.value() {
            ForIterableKind::Expression(expression) => self.execute_iterating_for_statement(statement, expression),
            ForIterableKind::Range(range) => self.execute_ranged_for_statement(statement, range),
        }
    }

    fn execute_iterating_for_statement(&mut self, statement: &ForStatement, expression: &Ranged<Expression>) -> StatementResult {
        let Value::Array { values, .. } = self.execute_expression(expression) else {
            panic!("Invalid iterable");
        };

        self.scope = std::mem::take(&mut self.scope).push();
        let values = values.borrow().clone();

        for x in values {
            self.scope.variables.insert(BabString::clone(&statement.iterator_name), x.actual_value().into_owned());

            for statement in &statement.body {
                if let StatementResult::Return(value) = self.execute_statement(statement) {
                    return StatementResult::Return(value);
                }
            }
        }

        self.scope = std::mem::take(&mut self.scope).pop();

        StatementResult::Continue
    }

    fn execute_ranged_for_statement(&mut self, statement: &ForStatement, range: &RangeExpression) -> StatementResult {
        let Value::Integer(start) = self.execute_expression(&range.start) else {
            panic!("Invalid start");
        };

        let Value::Integer(end) = self.execute_expression(&range.end) else {
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
                (method.function())(self, arguments, Some(*lhs))
            }

            Value::MethodIdReference { lhs, method } => {
                let method = self.methods.get(&method).unwrap().clone();

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
            if attrib.name.value() == Attribute::NAME_EXTERN {
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
        if let Value::Object { generic_types, .. } = &lhs {
            self.scope = std::mem::take(&mut self.scope).push();
            self.scope.generic_types = generic_types.clone();
        }

        let method = self.get_method(&lhs, &expression.method_name).unwrap();
        let return_value = self.execute_function_call(method, &expression.call);

        if let Value::Object { .. } = &lhs {
            self.scope = std::mem::take(&mut self.scope).pop();
        }

        return_value
    }

    fn get_method(&self, value: &Value, method_name: &BabString) -> Option<Value> {
        match value.typ() {
            ValueType::Array(..) => {
                for method in Builtin::array().methods() {
                    if method.name == method_name {
                        return Some(Value::MethodReference {
                            lhs: Box::new(value.clone()),
                            method: method.into(),
                        });
                    }
                }
            }

            ValueType::Builtin(builtin) => {
                for method in builtin.methods() {
                    if method.name == method_name {
                        return Some(Value::MethodReference {
                            lhs: Box::new(value.clone()),
                            method: method.into(),
                        });
                    }
                }

                let structure_id = StructureId::from(builtin);
                if let Some(structure) = self.structures.get(&structure_id) {
                    if let Some(method) = structure.get_method_by_name(method_name) {
                        return Some(Value::MethodIdReference {
                            lhs: Box::new(value.clone()),
                            method,
                        });
                    }
                }
            }

            ValueType::Structure(structure_id, ..) => {
                let structure = self.structures.get(&structure_id).expect("illegal StructureId");
                let method = structure.get_method_by_name(method_name)?;

                return Some(Value::MethodIdReference {
                    lhs: Box::new(value.clone()),
                    method,
                });
            }
        }

        None
    }

    fn resolve_type(&self, typ: &Type) -> (ValueType, Value) {
        assert!(typ.qualifiers.len() == 0);
        match typ.specifier.value() {
            TypeSpecifier::BuiltIn(ty) => {
                let ty = ValueType::Builtin(*ty.value());
                let default_value = self.get_default_value(&ty);
                (ty, default_value)
            }

            TypeSpecifier::Custom { name, .. } => {
                if let Some(generic) = self.scope.find_generic_type(&name) {
                    let default_value = self.get_default_value(&generic);
                    return (generic, default_value);
                }

                log::info!("Scope: {:#?}", self.scope);
                todo!("Resolve typ: {typ:#?}")
            }
        }
    }

    fn get_default_value(&self, typ: &ValueType) -> Value {
        match typ {
            ValueType::Builtin(BuiltinType::Bool) => Value::Bool(false),
            ValueType::Builtin(BuiltinType::G32) => Value::Integer(0),
            ValueType::Builtin(BuiltinType::Null) => Value::Null,
            ValueType::Builtin(BuiltinType::Slinger) => Value::String(String::new()),
            ValueType::Builtin(BuiltinType::Teken) => Value::Character('\0'),

            _ => todo!("Get default value of type {typ:#?}"),
        }
    }

    fn execute_subscript(&mut self, lhs: Value, subscript: &Ranged<Expression>) -> Value {
        let subscript = self.execute_expression(&subscript);

        let Value::Integer(index) = subscript else {
            panic!("ICE: subscript index is not a number");
        };

        if let Value::String(s) = lhs {
            return match s.chars().nth(index as _) {
                Some(c) => Value::Character(c),
                None => {
                    let error = RuntimeError::array_out_of_bounds(s.len(), index);
                    self.debugger.on_runtime_error(&error);
                    error!("Fout: {error}");
                    exit(1);
                }
            };
        }

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

fn create_top_level_structures() -> HashMap<StructureId, InterpreterStructure> {
    let mut map = HashMap::new();

    for ty in Builtin::TYPES {
        let structure = InterpreterStructure {
            structure: Structure::from_builtin_type(*ty),
            method_ids: HashMap::new(),
            extension_ids: HashMap::new(),
        };

        map.insert(StructureId::from(*ty), structure);
    }

    map
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