// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

mod backend;
mod compiler;
mod interpreter;
mod ir;
mod memory;
mod optimization;

pub use self::{
    backend::{
        AArch64CodeGenerator,
        CompiledFunction,
        CompiledObject,
    },
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
        PrimitiveType,
        Program,
        ProgramBuilder,
        Register,
    },
    memory::{
        FieldLayout,
        StructureLayout,
        TypeId,
        TypeManager,
    },
    optimization::{
        FunctionOptimizer,
        optimize_function,
        optimize_program,
    },
};
