// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

mod code_generator;
mod function_characteristics;
mod instruction;
mod register;

pub use self::code_generator::Amd64CodeGenerator;

use self::{
    instruction::Amd64Instruction,
    function_characteristics::Amd64FunctionCharacteristics,
    register::{
        Amd64Register,
        Amd64RegisterNameMode,
    },
};
