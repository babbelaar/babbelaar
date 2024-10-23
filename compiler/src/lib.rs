// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

mod compiler;
mod interpreter;
mod ir;

pub use self::{
    compiler::Compiler,
    interpreter::Interpreter,
    ir::{
        Function,
        FunctionBuilder,
        Immediate,
        Instruction,
        Label,
        Operand,
        Program,
        ProgramBuilder,
        Register,
        Structure,
        StructureBuilder,
    },
};
