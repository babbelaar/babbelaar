// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::collections::HashMap;

use crate::{Instruction, Operand, Register as IrRegister};

use super::RegisterLifetime;

#[derive(Debug, Default)]
pub struct LifeAnalysis {
    result: LifeAnalysisResult,
    function_calls: Vec<usize>,
}

impl LifeAnalysis {
    #[must_use]
    pub fn analyze(argument_registers: &[IrRegister], instructions: &[Instruction]) -> LifeAnalysisResult {
        let mut this = Self::default();

        this.add_argument_registers(argument_registers);

        for (index, instruction) in instructions.iter().enumerate() {
            this.add_instruction(index, instruction);
        }

        for call_index in this.function_calls {
            for (_, lifetime) in &mut this.result.lifetimes {
                if lifetime.first_use() < call_index && lifetime.last_use() > call_index {
                    lifetime.did_use_between_calls();
                }
            }
        }

        this.result
    }

    fn add_instruction(&mut self, index: usize, instruction: &Instruction) {
        match instruction {
            Instruction::Compare { lhs, rhs } => {
                self.add_lifetime(lhs, index);
                self.try_add_lifetime(rhs, index);
            }

            Instruction::Increment { register } => {
                self.add_lifetime(register, index);
            }

            Instruction::Move { source, destination } => {
                if let Operand::Register(source) = source {
                    self.add_lifetime(source, index);
                }
                self.add_lifetime(destination, index);
            }

            Instruction::MoveAddress { destination, offset } => {
                self.add_lifetime(destination, index);
                _ = offset;
            }

            Instruction::Call { name, arguments, ret_val_reg } => {
                _ = name;
                self.add_lifetime(ret_val_reg, index);
                for arg in arguments {
                    self.add_lifetime(arg, index);
                }

                self.add_call(index);
            }

            Instruction::Jump { location } => {
                _ = location;
            }

            Instruction::JumpConditional { condition, location } => {
                _ = condition;
                _ = location;
            }

            Instruction::Label(label) => {
                _ = label;
            }

            Instruction::Return { value_reg } => {
                if let Some(value_reg) = value_reg {
                    self.add_lifetime(value_reg, index)
                        .did_use_for_return();
                }
            }

            Instruction::MathOperation { operation, destination, lhs, rhs } => {
                _ = operation;
                self.add_lifetime(destination, index);
                self.try_add_lifetime(lhs, index);
                self.try_add_lifetime(rhs, index);
            }

            Instruction::Negate { dst, src } => {
                self.add_lifetime(dst, index);
                self.add_lifetime(src, index);
            }

            Instruction::StackAlloc { dst, size } => {
                _ = size;
                self.add_lifetime(dst, index);
            }

            Instruction::LoadPtr { destination, base_ptr, offset, typ } => {
                _ = typ;
                self.add_lifetime(destination, index);
                self.add_lifetime(base_ptr, index);
                self.try_add_lifetime(offset, index);
            }

            Instruction::StorePtr { base_ptr, offset, value, typ } => {
                _ = typ;
                self.add_lifetime(base_ptr, index);
                self.try_add_lifetime(offset, index);
                self.add_lifetime(value, index);
            }
        }
    }

    fn add_lifetime(&mut self, register: &IrRegister, index: usize) -> &mut RegisterLifetime {
        let lifetime = self.result.lifetimes.entry(*register)
            .or_insert(RegisterLifetime::new(index));

        lifetime.did_use_at(index);
        lifetime
    }

    fn try_add_lifetime(&mut self, operand: &Operand, index: usize) {
        if let Operand::Register(register) = operand {
            self.add_lifetime(register, index);
        }
    }

    fn add_argument_registers(&mut self, argument_registers: &[IrRegister]) {
        for register in argument_registers {
            self.add_lifetime(register, 0);
        }
    }

    fn add_call(&mut self, index: usize) {
        self.function_calls.push(index);
    }
}

#[derive(Debug, Default)]
pub struct LifeAnalysisResult {
    lifetimes: HashMap<IrRegister, RegisterLifetime>,
}

impl LifeAnalysisResult {
    #[must_use]
    pub fn find_only_return_register(&self) -> Option<IrRegister> {
        let mut result = None;

        for (register, lifetime) in &self.lifetimes {
            if lifetime.times_used_as_return() != 0 {
                // When there is more than 1 register used as the return register, we should not return anything.
                if result.is_some() {
                    return None;
                }

                result = Some(*register);
            }
        }

        result
    }

    #[must_use]
    pub fn lifetimes(&self) -> &HashMap<IrRegister, RegisterLifetime> {
        &self.lifetimes
    }

    #[must_use]
    pub fn into_sorted_vec(self) -> Vec<(IrRegister, RegisterLifetime)>  {
        let mut s: Vec<(IrRegister, RegisterLifetime)> = self.lifetimes.into_iter().collect();
        s.sort_by(|a, b| a.1.first_use().cmp(&b.1.first_use()));
        s
    }

    #[allow(unused)]
    pub fn dump_result(&self) {
        for (reg, lifetime) in self.lifetimes() {
            println!("Register {reg} has a lifetime of {} instructions. start={} end={}", lifetime.length(), lifetime.first_use(), lifetime.last_use());
            if lifetime.times_used_as_return() != 0 {
                println!("    and was used {}x as the return value", lifetime.times_used_as_return());
            }
        }
    }
}
