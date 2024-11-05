// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::collections::HashMap;

use crate::{Function, FunctionOptimizer, Instruction, Operand, Register};

#[derive(Debug, Default)]
pub struct DeadStoreEliminator {
    writing_instructions_per_register: HashMap<Register, usize>,
    instructions_to_remove: Vec<usize>,
}

impl FunctionOptimizer for DeadStoreEliminator {
    fn optimize(&mut self, function: &mut Function) {
        loop {
            *self = Self::default();

            self.analyze_instructions(function.instructions());

            // The instructions that are still in the map are unused, as they would've been removed by a read.
            for value in self.writing_instructions_per_register.values() {
                self.instructions_to_remove.push(*value);
            }

            if self.instructions_to_remove.is_empty() {
                break;
            }

            self.instructions_to_remove.sort();
            for removal_index in self.instructions_to_remove.iter().rev() {
                function.instructions.remove(*removal_index);
            }
        }
    }
}

impl DeadStoreEliminator {
    fn notice_read(&mut self, register: &Register) {
        self.writing_instructions_per_register.remove(register);
    }

    fn notice_non_removable_write(&mut self, register: &Register) {
        self.writing_instructions_per_register.remove(register);
    }

    fn notice_write(&mut self, register: &Register, index: usize) {
        // All instructions that simply load values to this register, and those aren't actually being read,
        // are useless, since we override that value now anyways.

        let previous_writes_without_a_read = self.writing_instructions_per_register.insert(register.clone(), index);

        if let Some(useless_instructions) = previous_writes_without_a_read {
            self.instructions_to_remove.push(useless_instructions);
        }
    }

    fn analyze_instructions(&mut self, instructions: &[Instruction]) {
        for (index, instruction) in instructions.iter().enumerate() {
            match &instruction {
                Instruction::Compare { lhs, rhs, .. } => {
                    self.notice_read(lhs);

                    if let Operand::Register(rhs) = rhs {
                        self.notice_read(rhs);
                    }
                }

                Instruction::Call { ret_val_reg, arguments, .. } => {
                    for arg in arguments {
                        self.notice_read(arg);
                    }

                    self.notice_write(ret_val_reg, index);
                }

                Instruction::Increment { register } => {
                    self.notice_write(register, index);
                }

                Instruction::Jump { location } => {
                    _ = location;
                }

                Instruction::JumpConditional { condition, location } => {
                    _ = condition;
                    _ = location;
                }

                Instruction::Label(..) => (),

                Instruction::LoadImmediate { destination_reg, immediate } => {
                    _ = immediate;
                    self.notice_write(destination_reg, index);
                }

                Instruction::Move { source, destination } => {
                    debug_assert_ne!(source, destination);

                    // We use the value, which means that until this point, instructions for that register were actually
                    // useful.
                    self.notice_read(source);

                    // But writes to the destination register before this are useless, since we override that value now.
                    self.notice_write(destination, index);
                }

                Instruction::MathOperation { operation, destination, lhs, rhs } => {
                    _ = operation;

                    if let Operand::Register(lhs) = lhs {
                        self.notice_read(lhs);
                    }

                    if let Operand::Register(rhs) = rhs {
                        self.notice_read(rhs);
                    }

                    self.notice_write(destination, index);
                }

                Instruction::Return { value_reg } => {
                    if let Some(return_value) = value_reg {
                        self.notice_read(return_value);
                    }
                }

                Instruction::StackAlloc { dst, size } => {
                    self.notice_write(dst, index);
                    _ = size;
                }

                Instruction::LoadPtr { destination, base_ptr, offset, typ: size } => {
                    _ = size;

                    self.notice_write(destination, index);
                    self.notice_read(base_ptr);

                    if let Operand::Register(offset) = offset {
                        self.notice_read(offset);
                    }
                }

                Instruction::StorePtr { base_ptr, offset, value, typ: size } => {
                    _ = size;

                    self.notice_non_removable_write(base_ptr);

                    if let Operand::Register(offset) = offset {
                        self.notice_read(offset);
                    }

                    if let Operand::Register(value) = value {
                        self.notice_read(value);
                    }
                }
            }
        }
    }
}