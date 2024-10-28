// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::collections::HashMap;

use crate::{CompiledFunction, Function, Instruction, Label, MathOperation, Operand, Register};

use super::{ArmBranchLocation, ArmConditionCode, ArmInstruction, ArmRegister, ArmShift2};

#[derive(Debug, Clone, Default)]
pub struct AArch64CodeGenerator {
    instructions: Vec<ArmInstruction>,
    label_offsets: HashMap<Label, usize>,
    simple_register_strategy: HashMap<Register, ArmRegister>,
}

impl AArch64CodeGenerator {
    #[must_use]
    pub fn compile(function: &Function) -> CompiledFunction {
        let mut this = Self::default();

        for instruction in function.instructions() {
            this.add_instruction(instruction);
        }

        this.dump_instructions();

        let byte_code = this.to_byte_code();
        print!("Bytecode:");
        for x in &byte_code {
            if *x < 0x10 {
                print!("0");
            }
            print!("{x:X} ");
        }
        println!();

        CompiledFunction {
            name: function.name.clone(),
            byte_code,
        }
    }

    pub fn add_instruction(&mut self, instruction: &Instruction) {
        match instruction {
            Instruction::Compare { lhs, rhs } => {
                self.add_instruction_cmp(lhs, rhs);
            }

            Instruction::Increment { register } => {
                let dst = self.allocate_register(register);
                let src = dst;
                let imm12 = 1;
                let shift = false;
                self.instructions.push(ArmInstruction::AddImmediate { dst, src, imm12, shift });
            }

            Instruction::LoadImmediate { immediate, destination_reg } => {
                let arm_register = ArmRegister { number: self.simple_register_strategy.len() as _ };
                self.simple_register_strategy.insert(*destination_reg, arm_register);
                self.instructions.push(ArmInstruction::MovZ { register: arm_register, imm16: immediate.as_i64() as _ });
            }

            Instruction::Move { source, destination } => {
                let dst = self.allocate_register(destination);
                let src = self.allocate_register(source);

                self.instructions.push(ArmInstruction::MovRegister64 { dst, src });
            }

            Instruction::Call { name, arguments, ret_val_reg } => {
                _ = name;
                _ = arguments;
                _ = ret_val_reg;
                todo!()
            }

            Instruction::Jump { location } => {
                let location = ArmBranchLocation::Label(*location);
                self.instructions.push(ArmInstruction::B { location });
            }

            Instruction::JumpConditional { condition, location } => {
                let cond = ArmConditionCode::from(*condition);
                let location = ArmBranchLocation::Label(*location);
                self.instructions.push(ArmInstruction::BCond { cond, location });
            }

            Instruction::Label(label) => {
                self.label_offsets.insert(*label, self.instructions.len());
            }

            Instruction::Return { value_reg } => {
                if let Some(value_reg) = value_reg {
                    let value_reg = self.allocate_register(value_reg);
                    if value_reg != ArmRegister::X0 {
                        self.instructions.push(ArmInstruction::MovRegister64 { dst: ArmRegister::X0, src: value_reg });
                    }
                }
                self.instructions.push(ArmInstruction::Ret);
            }

            Instruction::MathOperation { operation, destination, lhs, rhs } => {
                let dst = self.allocate_register(destination);

                match operation {
                    MathOperation::Add => self.add_instruction_add(dst, lhs, rhs),
                    MathOperation::Subtract => self.add_instruction_sub(dst, lhs, rhs),
                }
            }

            Instruction::StackAlloc { dst, size } => {
                _ = dst;
                _ = size;
                todo!()
            }

            Instruction::LoadPtr { destination, base_ptr, offset, typ } => {
                _ = destination;
                _ = base_ptr;
                _ = offset;
                _ = typ;
            }

            Instruction::StorePtr { base_ptr, offset, value, typ } => {
                _ = base_ptr;
                _ = offset;
                _ = value;
                _ = typ;
            }
        }
    }

    fn add_instruction_add(&mut self, dst: ArmRegister, lhs: &Operand, rhs: &Operand) {
        match (lhs, rhs) {
            (Operand::Immediate(lhs), Operand::Immediate(rhs)) => {
                let imm16 = lhs.as_i64() + rhs.as_i64();
                debug_assert!(imm16 < (1 << 16));
                self.instructions.push(ArmInstruction::MovZ {
                    register: dst,
                    imm16: imm16 as _,
                });
            }

            (Operand::Register(lhs), Operand::Register(rhs)) => {
                let lhs = self.allocate_register(lhs);
                let rhs = self.allocate_register(rhs);
                self.instructions.push(ArmInstruction::AddRegister {
                    dst,
                    lhs,
                    rhs,
                    imm: 0,
                    shift: ArmShift2::default(),
                });
            }

            (Operand::Immediate(imm), Operand::Register(reg)) |
                (Operand::Register(reg), Operand::Immediate(imm)) => {
                let src = self.allocate_register(reg);
                let imm12 = imm.as_i64() as _;
                debug_assert!(imm12 < (1 << 12));
                self.instructions.push(ArmInstruction::AddImmediate {
                    dst,
                    src,
                    imm12,
                    shift: false,
                });
            }
        }
    }

    fn add_instruction_cmp(&mut self, lhs: &Register, rhs: &Operand) {
        match rhs {
            Operand::Immediate(immediate) => {
                let immediate = immediate.as_i64();
                assert!(immediate < 4095, "cannot fit in an imm12");

                let register = self.allocate_register(lhs);
                self.instructions.push(ArmInstruction::CmpImmediate {
                    register,
                    value: immediate as _,
                });
            }

            Operand::Register(rhs) => {
                let lhs = self.allocate_register(lhs);
                let rhs = self.allocate_register(rhs);
                self.instructions.push(ArmInstruction::CmpRegister { lhs, rhs });
            }
        }
    }

    fn add_instruction_sub(&self, dst: ArmRegister, lhs: &Operand, rhs: &Operand) {
        _ = dst;
        _ = lhs;
        _ = rhs;
        todo!("ondersteun SUB")
    }

    /// TODO: this is just a really basic register allocator
    #[must_use]
    fn allocate_register(&mut self, register: &Register) -> ArmRegister {
        let number = self.simple_register_strategy.len() as _;
        let next_register = ArmRegister { number };

        *self.simple_register_strategy.entry(*register)
            .or_insert(next_register)
    }

    fn dump_instructions(&self) {
        println!("AArch64-instructies:");

        for (offset, instruction) in self.instructions.iter().enumerate() {
            for (label, label_offset) in &self.label_offsets {
                if *label_offset == offset {
                    println!("{label}:");
                }
            }

            println!("    {instruction}");
        }
    }

    #[must_use]
    fn to_byte_code(self) -> Vec<u8> {
        self.instructions.into_iter()
            .enumerate()
            .map(|(offset, instruction)| {
                instruction.encode(offset, &self.label_offsets)
            })
            .flat_map(u32::to_ne_bytes)
            .collect()
    }
}
