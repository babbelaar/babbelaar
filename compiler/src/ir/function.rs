// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::Display;

use super::{Instruction, Label, Register};

#[derive(Debug)]
pub struct Function {
    pub(crate) name: babbelaar::BabString,
    pub(super) argument_registers: Vec<Register>,
    pub(super) instructions: Vec<Instruction>,
    pub(super) labels: Vec<Label>,
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

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("werkwijze ")?;
        self.name.fmt(f)?;

        for instruction in &self.instructions {
            f.write_str("\n    ")?;
            instruction.fmt(f)?;
        }

        f.write_str("\n")?;

        Ok(())
    }
}
