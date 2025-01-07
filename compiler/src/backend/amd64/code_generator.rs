// Copyright (C) 2024 - 2025 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::HashMap, mem::take};

use babbelaar::BabString;
use log::debug;

use crate::{backend::{amd64::instruction_selector::Amd64InstructionSelector, VirtOrPhysReg}, CodeGenerator, CompiledFunction, Function, Instruction, Label, Platform, RegisterAllocator, Relocation, RelocationMethod, RelocationType, StackAllocator};

use super::{Amd64FixUp, Amd64FunctionCharacteristics, Amd64Instruction, Amd64Register};

#[derive(Debug)]
pub struct Amd64CodeGenerator {
    function_name: BabString,
    characteristics: Amd64FunctionCharacteristics,
    instructions: Vec<Amd64Instruction<Amd64Register>>,
    label_offsets: HashMap<Label, usize>,
    register_allocator: RegisterAllocator<Amd64Register>,
    stack_allocator: StackAllocator,
    relocations: Vec<Relocation>,
    current_instruction_id: usize,
}

impl Amd64CodeGenerator {
    #[must_use]
    pub fn compile(function: &Function, platform: Platform) -> CompiledFunction {
        let instruction_selector = Amd64InstructionSelector::compile(function, platform.clone());

        let characteristics = Amd64FunctionCharacteristics::analyze(function);
        let mut this = Self {
            function_name: function.name().clone(),
            characteristics,
            instructions: Vec::new(),
            label_offsets: HashMap::new(),
            register_allocator: RegisterAllocator::new(platform.clone(), function.argument_registers(), &instruction_selector.instructions),
            stack_allocator: StackAllocator::new(platform),
            relocations: Vec::new(),
            current_instruction_id: 0,
        };

        this.add_prologue(function.instructions());

        for instruction in instruction_selector.instructions {
            if let Amd64Instruction::ReturnNear = instruction {
                this.add_epilogue();
            }

            if let Amd64Instruction::Label(label) = instruction {
                this.label_offsets.insert(label, this.instructions.len());
            }

            let instruction = this.materialize_instruction(instruction);
            this.instructions.push(instruction);
            this.current_instruction_id += 1;
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

    fn add_prologue(&mut self, instructions: &[Instruction]) {
        for reg in self.register_allocator.callee_saved_registers_to_save() {
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

        for reg in self.register_allocator.callee_saved_registers_to_save().rev() {
            self.instructions.push(Amd64Instruction::PopReg64 { reg });
        }
    }

    #[must_use]
    fn materialize_instruction(&mut self, instruction: Amd64Instruction<VirtOrPhysReg<Amd64Register>>) -> Amd64Instruction<Amd64Register> {
        match instruction {
            Amd64Instruction::AddReg32Imm8 { dst, src } => Amd64Instruction::AddReg32Imm8 {
                dst: self.allocate_register(dst),
                src,
            },

            Amd64Instruction::AddReg32Reg32 { dst, src } => Amd64Instruction::AddReg32Reg32 {
                dst: self.allocate_register(dst),
                src: self.allocate_register(src),
            },

            Amd64Instruction::CallNearRelative { symbol_name } => Amd64Instruction::CallNearRelative {
                symbol_name,
            },

            Amd64Instruction::CmpReg32Imm8 { lhs, rhs } => Amd64Instruction::CmpReg32Imm8 {
                lhs: self.allocate_register(lhs),
                rhs,
            },

            Amd64Instruction::CmpReg32Imm32 { lhs, rhs } => Amd64Instruction::CmpReg32Imm32 {
                lhs: self.allocate_register(lhs),
                rhs,
            },

            Amd64Instruction::CmpReg32Reg32 { lhs, rhs } => Amd64Instruction::CmpReg32Reg32 {
                lhs: self.allocate_register(lhs),
                rhs: self.allocate_register(rhs),
            },

            Amd64Instruction::IMulReg32Imm8 { dst, lhs, rhs } => Amd64Instruction::IMulReg32Imm8 {
                dst: self.allocate_register(dst),
                lhs: self.allocate_register(lhs),
                rhs,
            },

            Amd64Instruction::IMulReg32Imm32 { dst, lhs, rhs } => Amd64Instruction::IMulReg32Imm32 {
                dst: self.allocate_register(dst),
                lhs: self.allocate_register(lhs),
                rhs,
            },

            Amd64Instruction::IMulReg32Reg32 { lhs, rhs } => Amd64Instruction::IMulReg32Reg32 {
                lhs: self.allocate_register(lhs),
                rhs: self.allocate_register(rhs),
            },

            Amd64Instruction::Inc32 { reg } => Amd64Instruction::Inc32 {
                reg: self.allocate_register(reg),
            },

            Amd64Instruction::Jmp { location } => Amd64Instruction::Jmp {
                location,
            },

            Amd64Instruction::JccShort { location, condition } => Amd64Instruction::JccShort {
                location, condition,
            },

            Amd64Instruction::Label(label) => Amd64Instruction::Label(label),

            Amd64Instruction::LeaReg32FromReg32 { dst, base } => Amd64Instruction::LeaReg32FromReg32 {
                dst: self.allocate_register(dst),
                base: self.allocate_register(base),
            },

            Amd64Instruction::LeaReg32FromReg32Off8 { dst, base, offset } => Amd64Instruction::LeaReg32FromReg32Off8 {
                dst: self.allocate_register(dst),
                base: self.allocate_register(base),
                offset,
            },

            Amd64Instruction::LeaReg64FromReg64 { dst, base } => Amd64Instruction::LeaReg64FromReg64 {
                dst: self.allocate_register(dst),
                base: self.allocate_register(base),
            },

            Amd64Instruction::LeaReg64FromReg64Off8 { dst, base, offset } => Amd64Instruction::LeaReg64FromReg64Off8 {
                dst: self.allocate_register(dst),
                base: self.allocate_register(base),
                offset,
            },

            Amd64Instruction::MovReg32FromPtrReg64 { dst, base } => Amd64Instruction::MovReg32FromPtrReg64 {
                dst: self.allocate_register(dst),
                base: self.allocate_register(base),
            },

            Amd64Instruction::MovReg32FromPtrReg64Off8 { dst, base, offset } => Amd64Instruction::MovReg32FromPtrReg64Off8 {
                dst: self.allocate_register(dst),
                base: self.allocate_register(base),
                offset,
            },

            Amd64Instruction::MovReg64FromPtrReg64 { dst, base } => Amd64Instruction::MovReg64FromPtrReg64 {
                dst: self.allocate_register(dst),
                base: self.allocate_register(base),
            },

            Amd64Instruction::MovReg64FromPtrReg64Off8 { dst, base, offset } => Amd64Instruction::MovReg64FromPtrReg64Off8 {
                dst: self.allocate_register(dst),
                base: self.allocate_register(base),
                offset,
            },

            Amd64Instruction::MovImm32ToPtrReg64 { base, src } => Amd64Instruction::MovImm32ToPtrReg64 {
                base: self.allocate_register(base),
                src,
            },

            Amd64Instruction::MovImm32ToPtrReg64Off8 { base, offset, src } => Amd64Instruction::MovImm32ToPtrReg64Off8 {
                base: self.allocate_register(base),
                offset,
                src,
            },

            Amd64Instruction::MovReg32ToPtrReg64 { base, src } => Amd64Instruction::MovReg32ToPtrReg64 {
                base: self.allocate_register(base),
                src: self.allocate_register(src),
            },

            Amd64Instruction::MovReg32ToPtrReg64Off8 { base, offset, src } => Amd64Instruction::MovReg32ToPtrReg64Off8 {
                base: self.allocate_register(base),
                offset,
                src: self.allocate_register(src),
            },

            Amd64Instruction::MovReg64ToPtrReg64 { base, src } => Amd64Instruction::MovReg64ToPtrReg64 {
                base: self.allocate_register(base),
                src: self.allocate_register(src),
            },

            Amd64Instruction::MovReg64ToPtrReg64Off8 { base, offset, src } => Amd64Instruction::MovReg64ToPtrReg64Off8 {
                base: self.allocate_register(base),
                src: self.allocate_register(src),
                offset,
            },

            Amd64Instruction::MovReg32Imm32 { dst, src } => Amd64Instruction::MovReg32Imm32 {
                dst: self.allocate_register(dst),
                src,
            },

            Amd64Instruction::MovReg32Reg32 { dst, src } => Amd64Instruction::MovReg32Reg32 {
                dst: self.allocate_register(dst),
                src: self.allocate_register(src),
            },

            Amd64Instruction::MovReg64Reg64 { dst, src } => Amd64Instruction::MovReg64Reg64 {
                dst: self.allocate_register(dst),
                src: self.allocate_register(src),
            },

            Amd64Instruction::NegReg64 { dst } => Amd64Instruction::NegReg64 {
                dst: self.allocate_register(dst),
            },

            Amd64Instruction::PopReg64 { reg } => Amd64Instruction::PopReg64 {
                reg: self.allocate_register(reg),
            },

            Amd64Instruction::PushReg64 { reg } => Amd64Instruction::PushReg64 {
                reg: self.allocate_register(reg),
            },

            Amd64Instruction::ReturnNear => Amd64Instruction::ReturnNear,

            Amd64Instruction::SetCC { dst, condition } => Amd64Instruction::SetCC {
                dst: self.allocate_register(dst),
                condition,
            },

            Amd64Instruction::SubReg32Imm8 { dst, src } => Amd64Instruction::SubReg32Imm8 {
                dst: self.allocate_register(dst),
                src,
            },

            Amd64Instruction::SubReg64Imm8 { dst, src } => Amd64Instruction::SubReg64Imm8 {
                dst: self.allocate_register(dst),
                src,
            },

            Amd64Instruction::SubReg32Reg32 { dst, src } => Amd64Instruction::SubReg32Reg32 {
                dst: self.allocate_register(dst),
                src: self.allocate_register(src),
            },

            Amd64Instruction::FixUp(Amd64FixUp::StackAlloc { dst, instruction_id }) => {
                let offset = self.stack_allocator.offset_of_reg(instruction_id);

                let dst = self.allocate_register(dst);

                if offset == 0 {
                    Amd64Instruction::MovReg64Reg64 {
                        dst,
                        src: Amd64Register::Rsp,
                    }
                } else {
                    Amd64Instruction::LeaReg64FromReg64Off8 {
                        dst,
                        base: Amd64Register::Rsp,
                        offset: offset.try_into().unwrap(),
                    }
                }
            }
        }
    }

    #[must_use]
    fn allocate_register(&mut self, register: VirtOrPhysReg<Amd64Register>) -> Amd64Register {
        let register = match register {
            VirtOrPhysReg::Physical(phys) => return phys,
            VirtOrPhysReg::Virtual(register) => register,
        };

        match self.register_allocator.get_mapping(&register, self.current_instruction_id) {
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
