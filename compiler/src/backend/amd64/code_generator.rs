// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::HashMap, mem::take};

use babbelaar::BabString;

use crate::{AllocatableRegister, CodeGenerator, CompiledFunction, Function, FunctionLink, FunctionLinkMethod, Immediate, Instruction, JumpCondition, Label, MathOperation, Operand, Register, RegisterAllocator};

use super::{Amd64FunctionCharacteristics, Amd64Instruction, Amd64Register};

#[derive(Debug)]
pub struct Amd64CodeGenerator {
    function_name: BabString,
    characteristics: Amd64FunctionCharacteristics,
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
        let characteristics = Amd64FunctionCharacteristics::analyze(function);
        let mut this = Self {
            function_name: function.name().clone(),
            characteristics,
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

        let byte_code = this.to_byte_code();
        print!("Bytecode: ");
        for x in &byte_code {
            if *x < 0x10 {
                print!("0");
            }
            print!("{x:X} ");
        }
        println!("\n");

        let link_locations = take(&mut this.link_locations);

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
                if let Some(value_reg) = value_reg {
                    let value_reg = self.allocate_register(value_reg);
                    if value_reg != Amd64Register::return_register() {
                        self.instructions.push(Amd64Instruction::MovReg64Reg64 {
                            dst: Amd64Register::return_register(),
                            src: value_reg,
                        });
                    }
                }

                self.add_epilogue();
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
                let offset = self.space_used_on_stack;
                self.space_used_on_stack += size;

                let dst = self.allocate_register(dst);

                if offset == 0 {
                    self.instructions.push(Amd64Instruction::MovReg64Reg64 { dst, src: Amd64Register::Rsp });
                } else {
                    self.instructions.push(Amd64Instruction::LeaReg64FromReg64Off8 { dst, base: Amd64Register::Rsp, offset: offset.try_into().unwrap() });
                }
            }

            Instruction::LoadPtr { destination, base_ptr, offset, typ } => {
                let dst = self.allocate_register(destination);
                let base = self.allocate_register(base_ptr);

                match (offset.shrink_if_possible(), typ.bytes()) {
                    (Operand::Immediate(Immediate::Integer8(offset)), 4) => {
                        if offset == 0 {
                            self.instructions.push(Amd64Instruction::MovReg32FromPtrReg64 { dst, base });
                        } else {
                            self.instructions.push(Amd64Instruction::MovReg32FromPtrReg64Off8 { dst, base, offset });
                        }
                    }

                    (Operand::Immediate(Immediate::Integer8(offset)), 8) => {
                        if offset == 0 {
                            self.instructions.push(Amd64Instruction::MovReg64FromPtrReg64 { dst, base });
                        } else {
                            self.instructions.push(Amd64Instruction::MovReg64FromPtrReg64Off8 { dst, base, offset });
                        }
                    }

                    _ => todo!("Ondersteun register-offset {offset} met typegrootte {}", typ.bytes()),
                }
            }

            Instruction::StorePtr { base_ptr, offset, value, typ } => {
                let base = self.allocate_register(base_ptr);

                match (value, offset.shrink_if_possible(), typ.bytes()) {
                    (Operand::Register(src), Operand::Immediate(Immediate::Integer8(offset)), 4) => {
                        let src = self.allocate_register(src);
                        if offset == 0 {
                            self.instructions.push(Amd64Instruction::MovReg32ToPtrReg64 { base, src });
                        } else {
                            self.instructions.push(Amd64Instruction::MovReg32ToPtrReg64Off8 { base, offset, src });
                        }
                    }

                    (Operand::Register(src), Operand::Immediate(Immediate::Integer8(offset)), 8) => {
                        let src = self.allocate_register(src);
                        if offset == 0 {
                            self.instructions.push(Amd64Instruction::MovReg64ToPtrReg64 { base, src });
                        } else {
                            self.instructions.push(Amd64Instruction::MovReg64ToPtrReg64Off8 { base, offset, src });
                        }
                    }

                    _ => todo!("Ondersteun register-offset {offset} met typegrootte {}", typ.bytes()),
                }
            }
        }
    }

    fn add_prologue(&mut self, instructions: &[Instruction]) {
        self.stack_size = 0;

        for instruction in instructions {
            if let Instruction::StackAlloc { size, .. } = instruction {
                self.stack_size += *size;
            }
        }

        if self.stack_size == 0 && self.characteristics.is_leaf_function() {
            // Omit frame pointer
            return;
        }

        self.instructions.push(Amd64Instruction::PushReg64 { reg: Amd64Register::Rbp });
        self.instructions.push(Amd64Instruction::MovReg64Reg64 { dst: Amd64Register::Rbp, src: Amd64Register::Rsp });

        if self.stack_size == 0 {
            return;
        }

        self.instructions.push(Amd64Instruction::SubReg64Imm8 { dst: Amd64Register::Rsp, src: self.stack_size.try_into().unwrap() });
    }

    fn add_epilogue(&mut self) {
        if self.stack_size == 0 && self.characteristics.is_leaf_function() {
            return;
        }

        self.instructions.push(Amd64Instruction::MovReg64Reg64 { dst: Amd64Register::Rsp, src: Amd64Register::Rbp });
        self.instructions.push(Amd64Instruction::PopReg64 { reg: Amd64Register::Rbp });
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
    fn to_byte_code(&mut self) -> Vec<u8> {
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

            if let Amd64Instruction::CallNearRelative { symbol_name, .. } = instruction {
                self.link_locations.push(FunctionLink {
                    name: symbol_name.clone(),
                    offset,
                    method: FunctionLinkMethod::Amd64CallNearRelative,
                });
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

    fn add_instruction_add(&mut self, dst: Amd64Register, lhs: &Operand, rhs: &Operand) {
        match (lhs, rhs) {
            (Operand::Register(lhs), Operand::Immediate(rhs)) => {
                let lhs = self.allocate_register(lhs);

                if lhs != dst {
                    self.instructions.push(Amd64Instruction::MovReg32Reg32 { dst, src: lhs });

                    if let Immediate::Integer8(i8) = rhs {
                        self.instructions.push(Amd64Instruction::LeaReg32FromReg32Off8 { dst, base: lhs, offset: *i8 });
                        return;
                    }
                }

                match rhs.shrink_if_possible() {
                    Immediate::Integer8(i8) => {
                        self.instructions.push(Amd64Instruction::AddReg32Imm8 { dst, src: i8 })
                    }

                    unsupported => todo!("Support ADD rhs {unsupported:?}"),
                }
            }

            (Operand::Register(lhs), Operand::Register(rhs)) => {
                let lhs = self.allocate_register(lhs);
                let rhs = self.allocate_register(rhs);

                if lhs != dst {
                    self.instructions.push(Amd64Instruction::MovReg32Reg32 { dst, src: lhs });
                }

                self.instructions.push(Amd64Instruction::AddReg32Reg32 { dst, src: rhs })
            }

            _ => todo!("Support add of {lhs}, {rhs}"),
        }
    }

    fn add_instruction_sub(&mut self, dst: Amd64Register, lhs: &Operand, rhs: &Operand) {
        match (lhs, rhs) {
            (Operand::Register(lhs), Operand::Immediate(rhs)) => {
                let lhs = self.allocate_register(lhs);

                if lhs != dst {
                    self.instructions.push(Amd64Instruction::MovReg32Reg32 { dst, src: lhs });
                }

                match rhs.shrink_if_possible() {
                    Immediate::Integer8(i8) => {
                        self.instructions.push(Amd64Instruction::SubReg32Imm8 { dst, src: i8 })
                    }

                    unsupported => todo!("Support SUB rhs {unsupported:?}"),
                }
            }

            (Operand::Register(lhs), Operand::Register(rhs)) => {
                let lhs = self.allocate_register(lhs);
                let rhs = self.allocate_register(rhs);

                if lhs != dst {
                    self.instructions.push(Amd64Instruction::MovReg32Reg32 { dst, src: lhs });
                }

                self.instructions.push(Amd64Instruction::SubReg32Reg32 { dst, src: rhs })
            }

            _ => todo!("Support sub of {lhs}, {rhs}"),
        }
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
