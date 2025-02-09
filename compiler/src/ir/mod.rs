// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

mod function;
mod function_argument;
mod function_attribute;
mod function_builder;
mod function_parameter;
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
    function_argument::FunctionArgument,
    function_attribute::{
        FunctionAttribute,
        FunctionAttributesExt,
    },
    function_builder::FunctionBuilder,
    function_parameter::FunctionParameter,
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
