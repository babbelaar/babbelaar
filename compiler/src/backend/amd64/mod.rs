// Copyright (C) 2024 - 2025 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

mod address;
mod code_generator;
mod condition_code;
mod fixup;
mod function_characteristics;
mod instruction;
mod instruction_selector;
mod register;

pub use self::code_generator::Amd64CodeGenerator;

use self::{
    address::{
        Amd64Address,
        Amd64Displacement,
    },
    condition_code::Amd64ConditionCode,
    fixup::Amd64FixUp,
    function_characteristics::Amd64FunctionCharacteristics,
    instruction::{
        Amd64Instruction,
        Amd64ModRMMode,
        SibScale,
    },
    register::Amd64Register,
};
