// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::HashMap, mem::take};

use babbelaar::BabString;

use crate::{AllocatableRegister, CodeGenerator, CompiledFunction, Function, FunctionLink, FunctionLinkMethod, Immediate, Instruction, JumpCondition, Label, MathOperation, Operand, Register, RegisterAllocator};

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
                let lhs = self.allocate_register(lhs);

                match rhs {
                    Operand::Immediate(immediate) => {
                        match immediate.shrink_if_possible() {
                            Immediate::Integer8(rhs) => {
                                self.instructions.push(Amd64Instruction::CmpReg32Imm8 {
                                    lhs,
                                    rhs,
                                });
                            }

                            Immediate::Integer16(..) | Immediate::Integer32(..) => {
                                self.instructions.push(Amd64Instruction::CmpReg32Imm32 {
                                    lhs,
                                    rhs: immediate.as_i32(),
                                });
                            }

                            Immediate::Integer64(..) => todo!(),
                        }
                    }

                    Operand::Register(register) => {
                        let rhs = self.allocate_register(register);

                        self.instructions.push(Amd64Instruction::CmpReg32Reg32 { lhs, rhs });
                    }
                }
            }

            Instruction::Increment { register } => {
                let reg = self.allocate_register(register);
                self.instructions.push(Amd64Instruction::Inc32 { reg });
            }

            Instruction::LoadImmediate { immediate, destination_reg } => {
                let dst = self.allocate_register(destination_reg);
                match immediate.shrink_if_possible() {
                    Immediate::Integer8(..) | Immediate::Integer16(..) | Immediate::Integer32(..) => {
                        self.instructions.push(Amd64Instruction::MovReg32Imm32 {
                            dst,
                            src: immediate.as_i32(),
                        });
                    }

                    Immediate::Integer64(qword) => {
                        todo!("Support mov64 (value is 0x{qword:x})")
                    }
                }
            }

            Instruction::Move { source, destination } => {
                let dst = self.allocate_register(destination);
                let src = self.allocate_register(source);

                if dst != src {
                    self.instructions.push(Amd64Instruction::MovReg64Reg64 { dst, src });
                }
            }

            Instruction::Call { name, arguments, ret_val_reg } => {
                debug_assert!(arguments.len() < (1 << 8));

                for (idx, arg) in arguments.iter().enumerate() {
                    let current_reg = self.allocate_register(arg);
                    let actual_reg = Amd64Register::argument_nth(idx);

                    if current_reg != actual_reg {
                        self.instructions.push(Amd64Instruction::MovReg64Reg64 {
                            dst: actual_reg,
                            src: current_reg,
                        });
                    }
                }

                self.link_locations.push(FunctionLink {
                    name: name.clone(),
                    offset: self.instructions.len(),
                    method: FunctionLinkMethod::Amd64CallNearRelative,
                });

                self.instructions.push(Amd64Instruction::CallNearRelative {
                    symbol_name: name.clone(),
                });

                let ret_val_reg = self.allocate_register(ret_val_reg);
                if ret_val_reg != Amd64Register::return_register() {
                    self.instructions.push(Amd64Instruction::MovReg64Reg64 {
                        dst: ret_val_reg,
                        src: Amd64Register::return_register(),
                    });
                }
            }

            Instruction::Jump { location } => {
                let location = *location;
                self.instructions.push(Amd64Instruction::Jmp { location });
            }

            Instruction::JumpConditional { condition, location } => {
                let location = *location;
                match condition {
                    JumpCondition::Equal => {
                        self.instructions.push(Amd64Instruction::JeShort { location });
                    }

                    JumpCondition::NotEqual => {
                        self.instructions.push(Amd64Instruction::JneShort { location });
                    }

                    _ => todo!("Ondersteun conditie {condition} voor Spring-locatie {location}"),
                }
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

    // TODO: I hate this, but we don't know the size of the future instruction since they are variable length, meaning we can't just do it all in one go
    #[must_use]
    fn to_byte_code(&self) -> Vec<u8> {
        let mut offsets = Vec::new();
        let mut relink_labels = Vec::new();

        let mut output = Vec::new();
        for instruction in &self.instructions {
            let offset = output.len();
            offsets.push(offset);

            instruction.encode(&mut output, offset, &self.label_offsets);

            if instruction.uses_label_offsets() {
                relink_labels.push((offset, instruction));
            }
        }

        let mut actual_label_offsets = HashMap::new();
        for (label, idx) in &self.label_offsets {
            actual_label_offsets.insert(*label, offsets[*idx]);
        }

        for (offset, instruction) in relink_labels {
            let mut buf = Vec::new();
            instruction.encode(&mut buf, offset, &actual_label_offsets);
            output[offset..(offset + buf.len())].copy_from_slice(&buf);
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
