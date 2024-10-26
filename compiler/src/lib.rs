// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

mod compiler;
mod interpreter;
mod ir;
mod optimization;

pub use self::{
    compiler::Compiler,
    interpreter::Interpreter,
    ir::{
        Function,
        FunctionBuilder,
        Immediate,
        Instruction,
        JumpCondition,
        Label,
        MathOperation,
        Operand,
        Program,
        ProgramBuilder,
        Register,
        Structure,
        StructureBuilder,
    },
    optimization::{
        FunctionOptimizer,
        optimize_function,
        optimize_program,
    },
};
