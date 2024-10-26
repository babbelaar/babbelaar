// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::HashMap, fmt::Display};

use babbelaar::BabString;

use super::{Instruction, Label, Register};

#[derive(Debug)]
pub struct Function {
    pub(crate) name: babbelaar::BabString,
    pub(crate) argument_registers: Vec<Register>,
    pub(crate) instructions: Vec<Instruction>,
    pub(crate) label_names: HashMap<Label, BabString>,
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
            f.write_str("\n")?;

            if should_indent_for(instruction) {
                f.write_str("    ")?;
            }

            instruction.fmt(f)?;

            if let Instruction::Label(label) = instruction {
                let label_name = self.label_names.get(label).unwrap();
                f.write_fmt(format_args!(r#" "{label_name}":"#))?;
            }
        }

        f.write_str("\n")?;

        Ok(())
    }
}

#[must_use]
fn should_indent_for(instruction: &Instruction) -> bool {
    !matches!(instruction, Instruction::Label(..))
}
