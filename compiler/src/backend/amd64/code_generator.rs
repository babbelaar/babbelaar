// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::HashMap, mem::take};

use babbelaar::BabString;
use log::{debug, warn};

use crate::{AllocatableRegister, CodeGenerator, CompiledFunction, Function, Immediate, Instruction, Label, MathOperation, Operand, Platform, Register, RegisterAllocator, Relocation, RelocationMethod, RelocationType, StackAllocator};

use super::{Amd64ConditionCode, Amd64FunctionCharacteristics, Amd64Instruction, Amd64Register};

#[derive(Debug)]
pub struct Amd64CodeGenerator {
    platform: Platform,
    function_name: BabString,
    characteristics: Amd64FunctionCharacteristics,
    instructions: Vec<Amd64Instruction>,
    label_offsets: HashMap<Label, usize>,
    register_allocator: RegisterAllocator<Amd64Register>,
    stack_allocator: StackAllocator,
    relocations: Vec<Relocation>,
}

impl Amd64CodeGenerator {
    #[must_use]
    pub fn compile(function: &Function, platform: Platform) -> CompiledFunction {
        let characteristics = Amd64FunctionCharacteristics::analyze(function);
        let mut this = Self {
            function_name: function.name().clone(),
            characteristics,
            instructions: Vec::new(),
            label_offsets: HashMap::new(),
            register_allocator: RegisterAllocator::new(platform.clone(), function),
            stack_allocator: StackAllocator::new(platform.clone()),
            relocations: Vec::new(),
            platform,
        };

        this.add_prologue(function.instructions());

        for (instruction_id, instruction) in function.instructions().iter().enumerate() {
            this.add_instruction(instruction, instruction_id);
        }

        this.dump_instructions();

        let byte_code = this.to_byte_code();
        let mut result = String::new();
        for x in &byte_code {
            if *x < 0x10 {
                result += "0";
            }
            result += &format!("{x:X} ");
        }
        debug!("Bytecode: {result}");

        let relocations = take(&mut this.relocations);

        CompiledFunction {
            name: function.name.clone(),
            byte_code,
            relocations,
        }
    }

    fn add_instruction(&mut self, instruction: &Instruction, instruction_id: usize) {
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

            Instruction::Move { source, destination } => {
                let dst = self.allocate_register(destination);

                self.add_instruction_mov(dst, source);
            }

            Instruction::MoveCondition { destination, condition } => {
                let dst = self.allocate_register(destination);
                self.instructions.push(Amd64Instruction::SetCC {
                    dst,
                    condition: Amd64ConditionCode::from(*condition),
                })
            }

            Instruction::Call { name, arguments, variable_arguments, ret_val_reg } => {
                assert_eq!(variable_arguments.len(), 0, "flexibele argumenten zijn niet ondersteund op dit platform!");
                debug_assert!(arguments.len() < (1 << 8));

                for (idx, arg) in arguments.iter().enumerate() {
                    let current_reg = self.allocate_register(&arg.register());
                    let actual_reg = Amd64Register::argument_nth(&self.platform, idx);

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

                if let Some(ret_val_reg) = ret_val_reg {
                    let ret_val_reg = self.allocate_register(ret_val_reg);
                    if ret_val_reg != Amd64Register::return_register(&self.platform) {
                        self.instructions.push(Amd64Instruction::MovReg64Reg64 {
                            dst: ret_val_reg,
                            src: Amd64Register::return_register(&self.platform),
                        });
                    }
                }
            }

            Instruction::Jump { location } => {
                let location = *location;
                self.instructions.push(Amd64Instruction::Jmp { location });
            }

            Instruction::JumpConditional { condition, location } => {
                let location = *location;

                let condition = Amd64ConditionCode::from(*condition);
                self.instructions.push(Amd64Instruction::JccShort { location, condition });
            }

            Instruction::Label(label) => {
                self.label_offsets.insert(*label, self.instructions.len());
            }

            Instruction::InitArg { destination, arg_idx } => {
                let src = Amd64Register::argument_nth(&self.platform, *arg_idx);
                let dst = self.allocate_register(destination);
                if src != dst {
                    println!("InitArg {destination}, {arg_idx} => mov {dst}, {src}");
                    self.instructions.push(Amd64Instruction::MovReg64Reg64 {
                        dst,
                        src,
                    });
                }
            }

            Instruction::Return { value_reg } => {
                if let Some(value_reg) = value_reg {
                    let value_reg = self.allocate_register(value_reg);
                    if value_reg != Amd64Register::return_register(&self.platform) {
                        self.instructions.push(Amd64Instruction::MovReg64Reg64 {
                            dst: Amd64Register::return_register(&self.platform),
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
                    MathOperation::Multiply => self.instruction_mul(dst, lhs, rhs),
                    MathOperation::Subtract => self.add_instruction_sub(dst, lhs, rhs),
                    MathOperation::Divide => todo!("Ondersteun DeelDoor op AMD64"),
                    MathOperation::Modulo => todo!("Ondersteun Modulo op AMD64"),
                    MathOperation::LeftShift => todo!("Voeg SchuifLinks toe"),
                    MathOperation::RightShift => todo!("Voeg SchuifRechts toe"),
                    MathOperation::Xor => todo!("Voeg ExclusieveOf toe"),
                }
            }

            Instruction::MoveAddress { destination, offset } => {
                todo!("Bereken datasectie adres {offset} en zet hem in {destination}")
            }

            Instruction::Negate { dst, src } => {
                let dst = self.allocate_register(dst);
                let src = self.allocate_register(src);

                if dst != src {
                    self.instructions.push(Amd64Instruction::MovReg64Reg64 { dst, src });
                }

                self.instructions.push(Amd64Instruction::NegReg64 { dst: src });
            }

            Instruction::StackAlloc { dst, size } => {
                let offset = self.stack_allocator.offset_of_reg(instruction_id);
                _ = size;

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

                // TODO: add x86-specific optimizations to create the following code below:
                let value = &Operand::Register(*value);

                match (value, offset.shrink_if_possible(), typ.bytes()) {
                    (Operand::Immediate(value), Operand::Immediate(Immediate::Integer8(offset)), 4) => {
                        if offset == 0 {
                            self.instructions.push(Amd64Instruction::MovImm32ToPtrReg64 {
                                base,
                                src: value.as_i32(),
                            });
                        } else {
                            self.instructions.push(Amd64Instruction::MovImm32ToPtrReg64Off8 {
                                base,
                                offset,
                                src: value.as_i32(),
                            });
                        }
                    }

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

                    _ => todo!("Ondersteun {instruction}"),
                }
            }
        }
    }

    fn add_prologue(&mut self, instructions: &[Instruction]) {
        for reg in self.register_allocator.callee_saved_registers_to_save().iter().cloned() {
            self.instructions.push(Amd64Instruction::PushReg64 { reg });
        }

        for (instruction_id, instruction) in instructions.iter().enumerate() {
            if let Instruction::StackAlloc { dst, size, .. } = instruction {
                self.stack_allocator.reserve_stack_allocation(instruction_id, *dst, *size);
            }
        }

        self.stack_allocator.finalize();

        if !self.characteristics.is_leaf_function() {
            self.instructions.push(Amd64Instruction::PushReg64 { reg: Amd64Register::Rbp });
            self.instructions.push(Amd64Instruction::MovReg64Reg64 { dst: Amd64Register::Rbp, src: Amd64Register::Rsp });
        }

        if self.stack_allocator.total_size() != 0 {
            self.instructions.push(Amd64Instruction::SubReg64Imm8 {
                dst: Amd64Register::Rsp,
                src: self.stack_allocator.total_size().try_into().unwrap(),
            });
        }
    }

    fn add_epilogue(&mut self) {
        if self.stack_allocator.total_size() != 0 {
            self.instructions.push(Amd64Instruction::SubReg64Imm8 {
                dst: Amd64Register::Rsp,
                src: self.stack_allocator.total_size().try_into().unwrap(),
            });
        }

        if !self.characteristics.is_leaf_function() {
            self.instructions.push(Amd64Instruction::MovReg64Reg64 { dst: Amd64Register::Rsp, src: Amd64Register::Rbp });
            self.instructions.push(Amd64Instruction::PopReg64 { reg: Amd64Register::Rbp });
        }

        for reg in self.register_allocator.callee_saved_registers_to_save().iter().rev().cloned() {
            self.instructions.push(Amd64Instruction::PopReg64 { reg });
        }
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
        debug!("AMD64-instructies voor {}:", self.function_name);

        for (offset, instruction) in self.instructions.iter().enumerate() {
            for (label, label_offset) in &self.label_offsets {
                if *label_offset == offset {
                    debug!("{label}:");
                }
            }

            debug!("    {instruction}");
        }

        debug!("");
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
                self.relocations.push(Relocation {
                    ty: RelocationType::Function {
                        name: symbol_name.clone(),
                    },
                    offset,
                    method: RelocationMethod::Amd64CallNearRelative,
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
            (Operand::Register(lhs), Operand::Immediate(rhs))
                | (Operand::Immediate(rhs), Operand::Register(lhs))=> {
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

    fn add_instruction_mov(&mut self, dst: Amd64Register, source: &Operand) {
        match source {
            Operand::Immediate(immediate) => {
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

            Operand::Register(source) => {
                let src = self.allocate_register(source);

                if dst != src {
                    self.instructions.push(Amd64Instruction::MovReg64Reg64 { dst, src });
                }
            }
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

    fn instruction_mul(&mut self, dst: Amd64Register, lhs: &Operand, rhs: &Operand) {
        match (lhs, rhs) {
            (Operand::Immediate(lhs), Operand::Immediate(rhs)) => {
                warn!("MUL met twee immediate waarden zou gedaan moeten worden door de optimalisator!");

                self.add_instruction_mov(dst, &Operand::Immediate(Immediate::Integer64(lhs.as_i64() * rhs.as_i64())));
            }

            (Operand::Immediate(imm), Operand::Register(reg)) | (Operand::Register(reg), Operand::Immediate(imm)) => {
                let reg = self.allocate_register(reg);

                match imm.shrink_if_possible() {
                    Immediate::Integer8(i8) => {
                        self.instructions.push(Amd64Instruction::IMulReg32Imm8 { dst, lhs: reg, rhs: i8 });
                    }

                    Immediate::Integer64(..) => todo!("Ondersteun MUL 64-bit op AMD64"),

                    other => {
                        self.instructions.push(Amd64Instruction::IMulReg32Imm32 { dst, lhs: reg, rhs: other.as_i32() });
                    }
                }
            }

            (Operand::Register(lhs), Operand::Register(rhs)) => {
                let lhs = self.allocate_register(lhs);
                let rhs = self.allocate_register(rhs);

                if lhs != dst {
                    self.instructions.push(Amd64Instruction::MovReg32Reg32 { dst, src: lhs });
                }

                self.instructions.push(Amd64Instruction::IMulReg32Reg32 { lhs: dst, rhs })
            }
        }
    }
}

impl CodeGenerator for Amd64CodeGenerator {
    fn compile(function: &Function) -> CompiledFunction {
        Self::compile(function, Platform::host_platform())
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

        let actual_bytecode = Amd64CodeGenerator::compile(&function, Platform::host_platform()).byte_code;
        assert_eq!(actual_bytecode, expected_bytecode);
    }

}
