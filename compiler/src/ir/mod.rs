// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

mod function;
mod function_builder;
mod instruction;
mod operand;
mod program;
mod program_builder;

pub use self::{
    function::{
        ArgumentName,
        ArgumentList,
        Function,
    },
    function_builder::FunctionBuilder,
    instruction::{
        Instruction,
        JumpCondition,
        Label,
        MathOperation,
        PrimitiveType,
    },
    operand::{
        Immediate,
        Operand,
        Register,
        RegisterAllocator,
    },
    program::Program,
    program_builder::ProgramBuilder,
};
