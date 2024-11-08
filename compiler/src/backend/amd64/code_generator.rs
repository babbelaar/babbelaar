// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::HashMap, mem::take};

use babbelaar::BabString;

use crate::{AllocatableRegister, CodeGenerator, CompiledFunction, Function, FunctionLink, Instruction, Label, MathOperation, Operand, Register, RegisterAllocator};

use super::{Amd64Instruction, Amd64Register};

#[derive(Debug)]
pub struct Amd64CodeGenerator {
    function_name: BabString,
    instructions: Vec<Amd64Instruction>,
    label_offsets: HashMap<Label, usize>,
    register_allocator: RegisterAllocator<Amd64Register>,
    stack_size: usize,
    space_used_on_stack: usize,
    link_locations: Vec<FunctionLink>,
}

impl Amd64CodeGenerator {
    #[must_use]
    pub fn compile(function: &Function) -> CompiledFunction {
        let mut this = Self {
            function_name: function.name().clone(),
            instructions: Vec::new(),
            label_offsets: HashMap::new(),
            register_allocator: RegisterAllocator::new(function),
            stack_size: 0,
            space_used_on_stack: 0,
            link_locations: Vec::new(),
        };

        this.add_prologue(function.instructions());

        for instruction in function.instructions() {
            this.add_instruction(instruction);
        }

        this.dump_instructions();

        let link_locations = take(&mut this.link_locations);

        let byte_code = this.to_byte_code();
        print!("Bytecode: ");
        for x in &byte_code {
            if *x < 0x10 {
                print!("0");
            }
            print!("{x:X} ");
        }
        println!("\n");

        CompiledFunction {
            name: function.name.clone(),
            byte_code,
            link_locations,
        }
    }

    fn add_instruction(&mut self, instruction: &Instruction) {
        match instruction {
            Instruction::Compare { lhs, rhs } => {
                // self.add_instruction_cmp(lhs, rhs);
            }

            Instruction::Increment { register } => {
                // let dst = self.allocate_register(register);
                // let src = dst;
                // let imm12 = 1;
                // let shift = false;
                // self.instructions.push(ArmInstruction::AddImmediate { dst, src, imm12, shift });
            }

            Instruction::LoadImmediate { immediate, destination_reg } => {
                // let arm_register = self.allocate_register(destination_reg);
                // self.instructions.push(ArmInstruction::MovZ { register: arm_register, imm16: immediate.as_i64() as _ });
            }

            Instruction::Move { source, destination } => {
                // let dst = self.allocate_register(destination);
                // let src = self.allocate_register(source);

                // if dst != src {
                //     self.instructions.push(ArmInstruction::MovRegister64 { dst, src });
                // }
            }

            Instruction::Call { name, arguments, ret_val_reg } => {
                // debug_assert!(arguments.len() < (1 << 8));

                // for (idx, arg) in arguments.iter().enumerate() {
                //     let reg = self.allocate_register(arg);

                //     if reg.number != idx as u8 {
                //         self.instructions.push(ArmInstruction::MovRegister64 {
                //             dst: ArmRegister { number: idx as _ },
                //             src: reg,
                //         });
                //     }
                // }

                // self.link_locations.push(FunctionLink {
                //     name: name.clone(),
                //     offset: self.instructions.len() * 4,
                //     method: FunctionLinkMethod::AArch64BranchLink,
                // });

                // self.instructions.push(ArmInstruction::Bl {
                //     symbol_name: name.clone(),
                //     offset: 0,
                // });

                // let ret_val_reg = self.allocate_register(ret_val_reg);
                // if ret_val_reg != ArmRegister::X0 {
                //     self.instructions.push(ArmInstruction::MovRegister64 {
                //         dst: ret_val_reg,
                //         src: ArmRegister::X0,
                //     });
                // }
            }

            Instruction::Jump { location } => {
                // let location = ArmBranchLocation::Label(*location);
                // self.instructions.push(ArmInstruction::B { location });
            }

            Instruction::JumpConditional { condition, location } => {
                // let cond = ArmConditionCode::from(*condition);
                // let location = ArmBranchLocation::Label(*location);
                // self.instructions.push(ArmInstruction::BCond { cond, location });
            }

            Instruction::Label(label) => {
                self.label_offsets.insert(*label, self.instructions.len());
            }

            Instruction::Return { value_reg } => {
                self.add_epilogue();

                if let Some(value_reg) = value_reg {
                    let value_reg = self.allocate_register(value_reg);
                    if value_reg != Amd64Register::return_register() {
                        self.instructions.push(Amd64Instruction::MovReg64Reg64 {
                            dst: Amd64Register::return_register(),
                            src: value_reg,
                        });
                    }
                }
                self.instructions.push(Amd64Instruction::ReturnNear);
            }

            Instruction::MathOperation { operation, destination, lhs, rhs } => {
                let dst = self.allocate_register(destination);

                match operation {
                    MathOperation::Add => self.add_instruction_add(dst, lhs, rhs),
                    MathOperation::Subtract => self.add_instruction_sub(dst, lhs, rhs),
                }
            }

            Instruction::StackAlloc { dst, size } => {
                let dst = self.allocate_register(dst);

                todo!("ADD imm to RSP for putting the pointer");

                self.space_used_on_stack += size;
                debug_assert!(self.space_used_on_stack <= self.stack_size);
            }

            Instruction::LoadPtr { destination, base_ptr, offset, typ } => {
                let is_64_bit = match typ.bytes() {
                    4 => false,
                    8 => true,
                    _ => todo!("We ondersteunen alleen 4 en 8 byte Laad ARM-instructies")
                };

                let Operand::Immediate(offset) = offset else {
                    todo!("ondersteun register offset {offset}")
                };

                let dst = self.allocate_register(destination);
                let base_ptr = self.allocate_register(base_ptr);

                // self.instructions.push(ArmInstruction::LdrImmediate {
                //     is_64_bit,
                //     mode: ArmUnsignedAddressingMode::UnsignedOffset,
                //     dst,
                //     base_ptr,
                //     offset: offset.as_i16(),
                // });
                todo!("Add LDR-equivalent on AMD64")
            }

            Instruction::StorePtr { base_ptr, offset, value, typ } => {
                let is_64_bit = match typ.bytes() {
                    4 => false,
                    8 => true,
                    _ => todo!("We ondersteunen alleen 4 en 8 byte Laad ARM-instructies")
                };

                let Operand::Immediate(offset) = offset else {
                    todo!("ondersteun register offset {offset}")
                };

                let base_ptr = self.allocate_register(base_ptr);

                let Operand::Register(src) = value else {
                    todo!("Ondersteun immediate SlaOp-instructie");
                };
                let src = self.allocate_register(src);

                todo!("Add STR equivalent on AMD64");
            }
        }
    }

    fn add_prologue(&mut self, instructions: &[Instruction]) {
        _ = instructions;
        // TODO
    }

    #[must_use]
    fn allocate_register(&mut self, register: &Register) -> Amd64Register {
        match self.register_allocator.get_mapping(register) {
            Some(register) => register,
            None => {
                // TODO: allocate on the stack in this case.
                panic!("Er is geen AMD64-register beschikbaar voor IR-register {register}!");
            }
        }
    }

    fn add_epilogue(&mut self) {
        // TODO
    }

    fn dump_instructions(&self) {
        println!("AMD64-instructies voor {}:", self.function_name);

        for (offset, instruction) in self.instructions.iter().enumerate() {
            for (label, label_offset) in &self.label_offsets {
                if *label_offset == offset {
                    println!("{label}:");
                }
            }

            println!("    {instruction}");
        }

        println!();
    }

    #[must_use]
    fn to_byte_code(&self) -> Vec<u8> {
        let mut output = Vec::new();
        for instruction in &self.instructions {
            instruction.encode(&mut output);
        }
        output
    }

    fn add_instruction_add(&self, dst: Amd64Register, lhs: &Operand, rhs: &Operand) {
        todo!()
    }

    fn add_instruction_sub(&self, dst: Amd64Register, lhs: &Operand, rhs: &Operand) {
        todo!()
    }
}

impl CodeGenerator for Amd64CodeGenerator {
    fn compile(function: &Function) -> CompiledFunction {
        Self::compile(function)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;
    use pretty_assertions::assert_eq;

    #[rstest]
    #[case(
        Instruction::Return { value_reg: None },
        &[0xc3]
    )]
    fn single_instruction_codegen(#[case] input: Instruction, #[case] expected_bytecode: &[u8]) {
        let function = Function {
            name: BabString::new_static("testFunctie"),
            argument_registers: Vec::new(),
            instructions: vec![input],
            label_names: HashMap::new(),
        };

        let actual_bytecode = Amd64CodeGenerator::compile(&function).byte_code;
        assert_eq!(actual_bytecode, expected_bytecode);
    }

}
