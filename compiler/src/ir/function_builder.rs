// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::collections::HashMap;

use babbelaar::BabString;

use super::{Function, Immediate, Instruction, Label, MathOperation, Operand, Program, Register, RegisterAllocator};

#[derive(Debug)]
pub struct FunctionBuilder<'program> {
    pub(super) program: &'program mut Program,
    pub(super) name: BabString,
    pub(super) register_allocator: RegisterAllocator,
    pub(super) argument_registers: Vec<Register>,
    pub(super) instructions: Vec<Instruction>,
    pub(super) locals: HashMap<BabString, Register>,
    pub(super) label_counter: usize,
    pub(crate) label_names: HashMap<Label, BabString>,
    pub(super) label_positions: HashMap<Label, usize>,
}

impl<'program> FunctionBuilder<'program> {
    pub fn call(&mut self, name: BabString, arguments: impl Into<Vec<Register>>) -> Register {
        let ret_val_reg = self.register_allocator.next();

        self.instructions.push(Instruction::Call {
            name,
            arguments: arguments.into(),
            ret_val_reg,
        });

        ret_val_reg
    }

    pub fn compare(&mut self, lhs: Register, rhs: impl Into<Operand>) {
        self.instructions.push(Instruction::Compare {
            lhs,
            rhs: rhs.into()
        });
    }

    pub fn increment(&mut self, register: Register) {
        self.instructions.push(Instruction::Increment { register });
    }

    pub fn jump(&mut self, location: Label) {
        self.instructions.push(Instruction::Jump { location });
    }

    pub fn jump_if_equal(&mut self, location: Label) {
        self.instructions.push(Instruction::JumpIfEqual { location });
    }

    pub fn jump_if_less(&mut self, location: Label) {
        self.instructions.push(Instruction::JumpIfLess { location });
    }

    pub fn jump_if_less_or_equal(&mut self, location: Label) {
        self.instructions.push(Instruction::JumpIfLessOrEqual { location });
    }

    pub fn jump_if_greater(&mut self, location: Label) {
        self.instructions.push(Instruction::JumpIfGreater { location });
    }

    pub fn jump_if_greater_or_equal(&mut self, location: Label) {
        self.instructions.push(Instruction::JumpIfGreaterOrEqual { location });
    }

    pub fn jump_if_not_equal(&mut self, location: Label) {
        self.instructions.push(Instruction::JumpIfNotEqual { location });
    }

    #[must_use]
    pub fn load_immediate(&mut self, immediate: Immediate) -> Register {
        let destination_reg = self.register_allocator.next();

        self.instructions.push(Instruction::LoadImmediate {
            immediate,
            destination_reg,
        });

        destination_reg
    }

    pub fn associate_register_to_local(&mut self, register: Register, local_name: impl Into<BabString>) {
        let local_name = local_name.into();

        debug_assert_eq!(self.locals.get(&local_name), None, "Lokale had al een waarde!");

        self.locals.insert(local_name, register);
    }

    #[must_use]
    pub fn load_string(&mut self, string: &str) -> Register {
        let immediate = self.program.add_string(string);
        self.load_immediate(immediate)
    }

    #[must_use]
    pub fn load_local(&mut self, name: &BabString) -> Register {
        let source_reg = *self.locals.get(name).expect("Local name is not valid");
        let reg = self.register_allocator.next();

        self.instructions.push(Instruction::Move {
            source: source_reg,
            destination: reg,
        });

        reg
    }

    #[must_use]
    pub fn math(&mut self, operation: MathOperation, lhs: impl Into<Operand>, rhs: impl Into<Operand>) -> Register {
        let destination = self.register_allocator.next();

        let lhs = lhs.into();
        let rhs = rhs.into();

        self.instructions.push(Instruction::MathOperation { operation, destination, lhs, rhs });

        destination
    }

    pub fn ret(&mut self) {
        self.instructions.push(Instruction::Return {
            value_reg: None,
        });
    }

    pub fn ret_with(&mut self, register: Register) {
        self.instructions.push(Instruction::Return {
            value_reg: Some(register),
        });
    }

    #[must_use]
    pub fn create_label(&mut self, name: impl Into<BabString>) -> Label {
        self.label_counter += 1;
        let label = Label {
            id: self.label_counter
        };

        self.label_names.insert(label, name.into());

        label
    }

    fn link_label(&mut self, label: Label, offset: usize) {
        debug_assert_eq!(self.label_positions.get(&label), None, "Cannot link a label twice!");
        self.label_positions.insert(label, offset);
        self.instructions.push(Instruction::Label(label));
    }

    pub fn link_label_here(&mut self, label: Label) {
        self.link_label(label, self.instructions.len() + 1);
    }

    #[must_use]
    pub fn create_label_and_link_here(&mut self, name: impl Into<BabString>) -> Label {
        let label = self.create_label(name);
        self.link_label_here(label.clone());
        label
    }

    #[must_use]
    pub fn build(self) -> Function {
        Function {
            name: self.name,
            argument_registers: self.argument_registers,
            instructions: self.instructions,
            label_names: self.label_names,
        }
    }
}

#[cfg(test)]
mod tests {

    use babbelaar::BabString;

    use crate::ProgramBuilder;

    #[test]
    fn test_empty_function() {
        ProgramBuilder::new()
            .build_function(BabString::new("Hallo"), |builder| {
                _ = builder;
            });
    }

}
