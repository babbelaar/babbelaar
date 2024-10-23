// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use super::{Instruction, Register};

#[derive(Debug)]
pub struct Function {
    pub(super) argument_registers: Vec<Register>,
    pub(super) instructions: Vec<Instruction>,
}

impl Function {
    #[must_use]
    pub fn argument_registers(&self) -> &[Register] {
        &self.argument_registers
    }

    #[must_use]
    pub fn instructions(&self) -> &[Instruction] {
        &self.instructions
    }
}
