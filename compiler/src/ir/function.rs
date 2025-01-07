// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::HashMap, fmt::Display};

use babbelaar::BabString;

use crate::{TypeId, TypeInfo};

use super::{Instruction, Label, Register, RegisterAllocator};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArgumentName {
    This,
    Name(BabString),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArgumentList {
    items: Vec<(ArgumentName, TypeInfo)>,
}

impl ArgumentList {
    #[must_use]
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
        }
    }

    pub fn add(&mut self, name: impl Into<BabString>, ty: TypeInfo) {
        let name = name.into();
        self.items.push((ArgumentName::Name(name), ty));
    }

    pub fn add_this(&mut self, ty: TypeId) {
        self.items.push((ArgumentName::This, TypeInfo::Plain(ty)));
    }

    #[must_use]
    pub fn iter(&self) -> impl Iterator<Item = &(ArgumentName, TypeInfo)> {
        self.items.iter()
    }
}

#[derive(Debug)]
pub struct Function {
    pub(crate) name: babbelaar::BabString,
    pub(crate) argument_registers: Vec<Register>,
    pub(crate) instructions: Vec<Instruction>,
    pub(crate) label_names: HashMap<Label, BabString>,
    pub(crate) ir_register_allocator: RegisterAllocator,
}

impl Function {
    #[must_use]
    pub fn name(&self) -> &BabString {
        &self.name
    }

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
