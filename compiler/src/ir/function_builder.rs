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
    pub(super) labels: Vec<Label>,
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

    #[must_use]
    pub fn load_immediate(&mut self, immediate: Immediate) -> Register {
        let destination_reg = self.register_allocator.next();

        self.instructions.push(Instruction::LoadImmediate {
            immediate,
            destination_reg,
        });

        destination_reg
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
    pub fn build(self) -> Function {
        Function {
            name: self.name,
            argument_registers: self.argument_registers,
            instructions: self.instructions,
            labels: self.labels,
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
