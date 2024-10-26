// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

//! The compiler class takes a [`ParseTree`] as input, turns it into IR, runs
//! the optimizer, and generates machine code.

use babbelaar::*;

use crate::{optimize_program, FunctionBuilder, Immediate, MathOperation, Operand, Program, ProgramBuilder, Register};

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
                statement.compile(builder);
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
        _ = builder;
        todo!();
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
                let register = expression.compile(builder).to_register(builder);
                builder.ret_with(register);
            }

            None => {
                builder.ret();
            }
        }
    }
}

impl CompileStatement for Structure {
    fn compile(&self, builder: &mut FunctionBuilder) {
        _ = builder;
        todo!();
    }
}

impl CompileStatement for VariableStatement {
    fn compile(&self, builder: &mut FunctionBuilder) {
        _ = builder;
        todo!();
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
        let lhs = self.lhs.compile(builder).to_register(builder);
        let rhs = self.rhs.compile(builder).to_register(builder);

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
                ExpressionResult::Comparison(*comparison)
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
                _ = member;
                todo!()
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
        _ = builder;
        todo!()
    }
}

impl CompileExpression for UnaryExpression {
    fn compile(&self, builder: &mut FunctionBuilder) -> ExpressionResult {
        _ = builder;
        todo!()
    }
}

#[derive(Debug, Clone, Copy)]
enum ExpressionResult {
    Comparison(Comparison),
    Register(Register),
}

impl ExpressionResult {
    #[must_use]
    fn to_comparison(self, builder: &mut FunctionBuilder<'_>) -> Comparison {
        match self {
            Self::Comparison(comparison) => comparison,
            Self::Register(register) => {
                // if the value is a register, we want to branch/jump when the value isn't 0 (false)
                builder.compare(register, Operand::Immediate(Immediate::Integer64(0)));
                Comparison::Inequality
            }
        }
    }

    #[must_use]
    fn to_register(self, builder: &mut FunctionBuilder<'_>) -> Register {
        match self {
            Self::Comparison(..) => {
                _ = builder; // Use the builder to load the comparison flags (NZCV / FLAGS) to a register, somehow
                todo!("Hoe moet ik een Vergelijkresultaat omzetten naar een Register?")
            }

            Self::Register(reg) => reg,
        }
    }
}

impl From<Register> for ExpressionResult {
    fn from(value: Register) -> Self {
        Self::Register(value)
    }
}
