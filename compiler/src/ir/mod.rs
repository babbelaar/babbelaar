// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

mod function;
mod function_builder;
mod instruction;
mod operand;
mod program;
mod program_builder;
mod structure;
mod structure_builder;

pub use self::{
    function::Function,
    function_builder::FunctionBuilder,
    instruction::{
        Instruction,
        Label,
        MathOperation,
    },
    operand::{
        Immediate,
        Operand,
        Register,
        RegisterAllocator,
    },
    program::Program,
    program_builder::ProgramBuilder,
    structure::Structure,
    structure_builder::StructureBuilder,
};
