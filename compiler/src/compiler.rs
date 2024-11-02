// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

//! The compiler class takes a [`ParseTree`] as input, turns it into IR, runs
//! the optimizer, and generates machine code.

use std::rc::Rc;

use babbelaar::*;

use crate::{optimize_program, FunctionBuilder, Immediate, MathOperation, Operand, PrimitiveType, Program, ProgramBuilder, Register, TypeId};

#[derive(Debug)]
pub struct Compiler {
    program_builder: ProgramBuilder,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            program_builder: ProgramBuilder::new(),
        }
    }

    pub fn compile_trees(&mut self, trees: &[ParseTree]) {
        self.layout_structures(trees);
        self.compile_functions(trees);
    }

    fn layout_structures(&mut self, trees: &[ParseTree]) {
        for statement in trees.iter().flat_map(|t| t.structures()) {
            let StatementKind::Structure(structure) = &statement.kind else {
                panic!();
            };

            self.program_builder.add_structure(structure);
        }
    }

    fn compile_functions(&mut self, trees: &[ParseTree]) {
        for statement in trees.iter().flat_map(|t| t.functions()) {
            let StatementKind::Function(func) = &statement.kind else {
                panic!();
            };

            self.compile_function(func);
        }
    }

    #[must_use]
    pub fn finish(self) -> Program {
        let mut program = self.program_builder.build();
        optimize_program(&mut program);
        program
    }

    fn compile_function(&mut self, func: &FunctionStatement) {
        let Some(body) = &func.body else {
            return;
        };

        self.program_builder.build_function(func.name.value().clone(),|builder| {
            for statement in body {
                statement.compile(builder);
            }

            builder.ret();
        });
    }
}

trait CompileStatement {
    fn compile(&self, builder: &mut FunctionBuilder);
}

impl CompileStatement for Statement {
    fn compile(&self, builder: &mut FunctionBuilder) {
        match &self.kind {
            StatementKind::Assignment(statement) => {
                statement.compile(builder);
            }

            StatementKind::Expression(expression) => {
                _ = expression.compile(builder);
            }

            StatementKind::Extension(statement) => {
                statement.compile(builder);
            }

            StatementKind::Function(statement) => {
                statement.compile(builder);
            }

            StatementKind::For(statement) => {
                statement.compile(builder);
            }

            StatementKind::If(statement) => {
                statement.compile(builder);
            }

            StatementKind::Interface(statement) => {
                statement.compile(builder);
            }

            StatementKind::Return(statement) => {
                statement.compile(builder);
            }

            StatementKind::Structure(statement) => {
                _ = statement;
            }

            StatementKind::Variable(statement) => {
                statement.compile(builder);
            }
        }
    }
}

impl CompileStatement for AssignStatement {
    fn compile(&self, builder: &mut FunctionBuilder) {
        _ = builder;
        todo!();
    }
}

impl CompileStatement for ExtensionStatement {
    fn compile(&self, builder: &mut FunctionBuilder) {
        _ = builder;
        todo!();
    }
}

impl CompileStatement for FunctionStatement {
    fn compile(&self, builder: &mut FunctionBuilder) {
        _ = builder;
        todo!();
    }
}

impl CompileStatement for ForStatement {
    fn compile(&self, builder: &mut FunctionBuilder) {
        let after = builder.create_label("na-volg");

        match self.iterable.value() {
            ForIterableKind::Expression(expression) => {
                _ = expression;
                todo!("ondersteun Doorloper-gebaseerde expressies met `volg`");
            }

            ForIterableKind::Range(range) => {
                let (current_value, ty) = range.start.compile(builder).to_readable_and_type(builder);

                builder.associate_register_to_local(current_value, self.iterator_name.value(), ty);

                let end = range.end.compile(builder).to_readable(builder);

                builder.compare(current_value, end);
                builder.jump_if_greater_or_equal(after);

                let body = builder.create_label_and_link_here("volg-lichaam");

                for statement in &self.body {
                    statement.compile(builder);
                }

                builder.increment(current_value);

                builder.compare(current_value, end);
                builder.jump_if_less(body);
                // otherwise, fall through to the after the for-statement
            }
        }

        builder.link_label_here(after);
    }
}

impl CompileStatement for IfStatement {
    fn compile(&self, builder: &mut FunctionBuilder) {
        let after_block = builder.create_label(format!("na-als"));
        let then_block = builder.create_label(format!("als {}", self.condition.value()));

        let comparison = self.condition.compile(builder).to_comparison(builder);

        match comparison {
            Comparison::Equality => {
                builder.jump_if_equal(then_block);
                builder.jump(after_block);
            }

            Comparison::Inequality => {
                builder.jump_if_not_equal(then_block);
                builder.jump(after_block);
            }

            _ => todo!(),
        };

        builder.link_label_here(then_block);
        for statement in &self.body {
            statement.compile(builder);
        }

        builder.link_label_here(after_block);
    }
}

impl CompileStatement for InterfaceStatement {
    fn compile(&self, builder: &mut FunctionBuilder) {
        _ = builder;
        todo!();
    }
}

impl CompileStatement for ReturnStatement {
    fn compile(&self, builder: &mut FunctionBuilder) {
        match &self.expression {
            Some(expression) => {
                let register = expression.compile(builder).to_readable(builder);
                builder.ret_with(register);
            }

            None => {
                builder.ret();
            }
        }
    }
}

impl CompileStatement for VariableStatement {
    fn compile(&self, builder: &mut FunctionBuilder) {
        let (register, ty) = self.expression.compile(builder).to_readable_and_type(builder);
        builder.associate_register_to_local(register, self.name.value(), ty);
    }
}

trait CompileExpression {
    fn compile(&self, builder: &mut FunctionBuilder) -> ExpressionResult;
}

impl CompileExpression for Expression {
    fn compile(&self, builder: &mut FunctionBuilder) -> ExpressionResult {
        match self {
            Expression::BiExpression(expression) => {
                expression.compile(builder)
            }

            Expression::Postfix(expression) => {
                expression.compile(builder)
            }

            Expression::Primary(expression) => {
                expression.compile(builder)
            }

            Expression::Unary(expression) => {
                expression.compile(builder)
            }
        }
    }
}

impl CompileExpression for BiExpression {
    fn compile(&self, builder: &mut FunctionBuilder) -> ExpressionResult {
        let lhs = self.lhs.compile(builder).to_readable(builder);
        let rhs = self.rhs.compile(builder).to_readable(builder);

        match self.operator.value() {
            BiOperator::Math(math) => {

                let math_operation = match math {
                    MathOperator::Add => MathOperation::Add,
                    MathOperator::Subtract => MathOperation::Subtract,

                    _ => todo!("Ondersteun {:?}", self.operator.value()),
                };

                builder.math(math_operation, lhs, rhs).into()
            }

            BiOperator::Comparison(comparison) => {
                builder.compare(lhs, rhs);
                comparison.into()
            }
        }
    }
}

impl CompileExpression for PostfixExpression {
    fn compile(&self, builder: &mut FunctionBuilder) -> ExpressionResult {
        let lhs = self.lhs.compile(builder);

        match self.kind.value() {
            PostfixExpressionKind::Call(call) => {
                _ = lhs;
                _ = call;
                todo!()
            }

            PostfixExpressionKind::Member(member) => {
                let (base_ptr, ty) = lhs.to_readable_and_type(builder);

                let field = builder.layout_of(ty).field(&member);
                let offset = field.offset() as isize;
                let field_type = field.type_id();
                let primitive_typ = field.primitive_type();

                ExpressionResult::pointer(base_ptr, offset, field_type, primitive_typ)
            }

            PostfixExpressionKind::MethodCall(call) => {
                _ = call;
                todo!()
            }

            PostfixExpressionKind::Subscript(subscript) => {
                _ = subscript;
                todo!()
            }
        }
    }
}

impl CompileExpression for PrimaryExpression {
    fn compile(&self, builder: &mut FunctionBuilder) -> ExpressionResult {
        match self {
            Self::Boolean(b) => {
                builder.load_immediate(Immediate::Integer32(*b as i32)).into()
            }

            Self::CharacterLiteral(c) => {
                builder.load_immediate(Immediate::Integer32(*c as i32)).into()
            }

            Self::IntegerLiteral(i) => {
                builder.load_immediate(Immediate::Integer64(*i)).into()
            }

            Self::Parenthesized(expression) => {
                expression.compile(builder)
            }

            Self::Reference(ranged) => {
                builder.load_local(ranged.value()).into()
            }

            Self::ReferenceThis => {
                todo!()
            }

            Self::SizedArrayInitializer { typ, size } => {
                _ = typ;
                _ = size;
                todo!()
            }

            Self::StringLiteral(literal) => {
                builder.load_string(&literal).into()
            }

            Self::StructureInstantiation(expression) => {
                expression.compile(builder)
            }

            Self::TemplateString { parts } => {
                _ = parts;
                todo!()
            }
        }
    }
}

impl CompileExpression for StructureInstantiationExpression {
    fn compile(&self, builder: &mut FunctionBuilder) -> ExpressionResult {
        let (layout, register) = builder.allocate_structure(self.name.value());

        let ty = layout.type_id().clone();

        let default_values: Vec<(usize, PrimitiveType, Rc<Expression>)> = layout
            .fields()
            .iter()
            .filter_map(|field| {
                let offset = field.offset();
                let typ = field.primitive_type();
                let default_value_expression = Rc::clone(field.default_value_expression()?);
                Some((offset, typ, default_value_expression))
            })
            .collect();

        let fields: Vec<(usize, PrimitiveType, &FieldInstantiation)> = self.fields
            .iter()
            .map(|field| {
                let layout = layout.field(&field.name);
                (layout.offset(), layout.primitive_type(), field)
            })
            .collect();

        for (offset, typ, expression) in default_values {
            let value = expression.compile(builder).to_readable(builder);
            builder.store_ptr(register, Operand::Immediate(Immediate::Integer64(offset as _)), value, typ);
        }

        for (offset, typ,field) in fields {
            let value = field.value.compile(builder).to_readable(builder);
            builder.store_ptr(register, Operand::Immediate(Immediate::Integer64(offset as _)), value, typ);
        }

        ExpressionResult::typed(register, ty)
    }
}

impl CompileExpression for UnaryExpression {
    fn compile(&self, builder: &mut FunctionBuilder) -> ExpressionResult {
        _ = builder;
        todo!()
    }
}

#[derive(Debug, Clone)]
struct ExpressionResult {
    kind: ExpressionResultKind,
    type_id: TypeId,
}

impl ExpressionResult {
    #[must_use]
    pub fn typed(register: Register, type_id: TypeId) -> Self {
        Self {
            kind: ExpressionResultKind::Register(register),
            type_id,
        }
    }

    #[must_use]
    pub fn pointer(base_ptr: Register, offset: isize, type_id: TypeId, typ: PrimitiveType) -> Self {
        Self {
            kind: ExpressionResultKind::PointerRegister { base_ptr, offset, typ },
            type_id,
        }
    }

    #[must_use]
    pub fn to_comparison(self, builder: &mut FunctionBuilder) -> Comparison {
        self.kind.to_comparison(builder)
    }

    #[must_use]
    pub fn to_readable(self, builder: &mut FunctionBuilder) -> Register {
        self.kind.to_readable(builder)
    }

    #[must_use]
    pub fn to_readable_and_type(self, builder: &mut FunctionBuilder) -> (Register, TypeId) {
        (self.kind.to_readable(builder), self.type_id)
    }
}

impl From<Comparison> for ExpressionResult {
    fn from(value: Comparison) -> Self {
        Self {
            kind: ExpressionResultKind::Comparison(value),
            type_id: TypeId::G32,
        }
    }
}

impl From<&Comparison> for ExpressionResult {
    fn from(value: &Comparison) -> Self {
        Self {
            kind: ExpressionResultKind::Comparison(*value),
            type_id: TypeId::G32,
        }
    }
}

impl From<Register> for ExpressionResult {
    fn from(value: Register) -> Self {
        Self {
            kind: ExpressionResultKind::Register(value),
            type_id: TypeId::G32,
        }
    }
}

impl From<&Register> for ExpressionResult {
    fn from(value: &Register) -> Self {
        Self {
            kind: ExpressionResultKind::Register(*value),
            type_id: TypeId::G32,
        }
    }
}

impl From<(TypeId, Register)> for ExpressionResult {
    fn from(value: (TypeId, Register)) -> Self {
        Self::typed(value.1, value.0)
    }
}

#[derive(Debug, Clone)]
enum ExpressionResultKind {
    Comparison(Comparison),
    Register(Register),
    PointerRegister { base_ptr: Register, offset: isize, typ: PrimitiveType },
}

impl ExpressionResultKind {
    #[must_use]
    fn to_comparison(self, builder: &mut FunctionBuilder<'_>) -> Comparison {
        match self {
            Self::Comparison(comparison) => comparison,
            Self::Register(register) => {
                // if the value is a register, we want to branch/jump when the value isn't 0 (false)
                builder.compare(register, Operand::Immediate(Immediate::Integer64(0)));
                Comparison::Inequality
            }
            Self::PointerRegister { base_ptr, offset, typ } => {
                // if the value is a register, we want to branch/jump when the value isn't 0 (false)
                let register = builder.load_ptr(base_ptr, Immediate::Integer64(offset as _), typ);
                builder.compare(register, Operand::Immediate(Immediate::Integer64(0)));
                Comparison::Inequality
            }
        }
    }

    #[must_use]
    fn to_readable(self, builder: &mut FunctionBuilder<'_>) -> Register {
        match self {
            Self::Comparison(..) => {
                _ = builder; // Use the builder to load the comparison flags (NZCV / FLAGS) to a register, somehow
                todo!("Hoe moet ik een Vergelijkresultaat omzetten naar een Register?")
            }

            Self::Register(reg) => reg,

            Self::PointerRegister { base_ptr, offset, typ } => {
                builder.load_ptr(base_ptr, Immediate::Integer64(offset as _), typ)
            }
        }
    }
}
