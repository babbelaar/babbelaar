// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

//! The compiler class takes a [`ParseTree`] as input, turns it into IR, runs
//! the optimizer, and generates machine code.

use babbelaar::*;

use crate::{FunctionBuilder, Immediate, Program, ProgramBuilder, Register};

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
        self.program_builder.build()
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
        _ = builder;
        todo!();
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
                let register = expression.compile(builder);
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
    fn compile(&self, builder: &mut FunctionBuilder) -> Register;
}

impl CompileExpression for Expression {
    fn compile(&self, builder: &mut FunctionBuilder) -> Register {
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
    fn compile(&self, builder: &mut FunctionBuilder) -> Register {
        _ = builder;
        todo!()
    }
}

impl CompileExpression for PostfixExpression {
    fn compile(&self, builder: &mut FunctionBuilder) -> Register {
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
    fn compile(&self, builder: &mut FunctionBuilder) -> Register {
        match self {
            Self::Boolean(b) => {
                builder.load_immediate(Immediate::Integer32(*b as i32))
            }

            Self::CharacterLiteral(c) => {
                builder.load_immediate(Immediate::Integer32(*c as i32))
            }

            Self::IntegerLiteral(i) => {
                builder.load_immediate(Immediate::Integer64(*i))
            }

            Self::Parenthesized(expression) => {
                expression.compile(builder)
            }

            Self::Reference(ranged) => {
                builder.load_local(ranged.value())
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
                builder.load_string(&literal)
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
    fn compile(&self, builder: &mut FunctionBuilder) -> Register {
        _ = builder;
        todo!()
    }
}

impl CompileExpression for UnaryExpression {
    fn compile(&self, builder: &mut FunctionBuilder) -> Register {
        _ = builder;
        todo!()
    }
}
