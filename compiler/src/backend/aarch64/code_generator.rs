// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::HashMap, mem::take};

use babbelaar::BabString;
use log::debug;

use crate::{backend::VirtOrPhysReg, CodeGenerator, CompiledFunction, Function, Instruction, Label, Platform, RegisterAllocator, Relocation, RelocationMethod, RelocationType, StackAllocator};

use super::{fixup::AArch64FixUp, AArch64FunctionCharacteristics, AArch64InstructionSelector, AArch64Optimizer, AArch64VarArgsConvention, ArmInstruction, ArmRegister, ArmSignedAddressingMode, ArmUnsignedAddressingMode, POINTER_SIZE, SPACE_NEEDED_FOR_FP_AND_LR};

#[derive(Debug)]
pub struct AArch64CodeGenerator {
    function_name: BabString,
    characteristics: AArch64FunctionCharacteristics,
    instructions: Vec<ArmInstruction<ArmRegister>>,
    label_offsets: HashMap<Label, usize>,
    register_allocator: RegisterAllocator<ArmRegister>,
    stack_allocator: StackAllocator,
    relocations: Vec<Relocation>,

    var_args_convention: AArch64VarArgsConvention,
    current_instruction_id: usize,

    // Should use Pointer Authentication Code for RET/LR?
    is_pac_ret: bool,
}

impl AArch64CodeGenerator {
    #[must_use]
    pub fn compile(function: &Function, platform: Platform) -> CompiledFunction {
        debug!("Werkwijze `{}` aan het compileren...", function.name());

        let mut instruction_selector = AArch64InstructionSelector::compile(function, &platform);

        let characteristics = AArch64FunctionCharacteristics::analyze(function);

        let mut this = Self {
            function_name: function.name().clone(),
            characteristics,
            instructions: Vec::new(),
            label_offsets: HashMap::new(),
            register_allocator: RegisterAllocator::new(platform.clone(), function.argument_registers(), &instruction_selector.instructions),
            stack_allocator: StackAllocator::new(platform.clone()),
            relocations: Vec::new(),
            var_args_convention: instruction_selector.var_args_convention,
            current_instruction_id: 0,
            is_pac_ret: platform.options().arm64e(),
        };

        this.add_prologue(function.instructions());

        for (idx, instruction) in instruction_selector.instructions.into_iter().enumerate() {
            if instruction_selector.relocations.front().is_some_and(|x| x.offset == idx) {
                this.relocations.push(Relocation {
                    offset: this.instructions.len() * 4,
                    ..instruction_selector.relocations.pop_front().unwrap()
                });
            }

            if let ArmInstruction::Ret = instruction {
                this.add_epilogue();
            }

            this.materialize_instruction(instruction);
            this.current_instruction_id += 1;
        }

        debug_assert_eq!(instruction_selector.relocations, std::collections::VecDeque::default());

        AArch64Optimizer::optimize(&mut this.instructions, &mut this.label_offsets);

        this.dump_instructions();

        let relocations = take(&mut this.relocations);

        let byte_code = this.to_byte_code();
        let mut result = String::new();
        for x in &byte_code {
            if *x < 0x10 {
                result += "0";
            }
            result += &format!("{x:X} ");
        }
        debug!("Bytecode: {result}");

        CompiledFunction {
            name: function.name.clone(),
            byte_code,
            relocations,
        }
    }

    #[must_use]
    fn allocate_register(&mut self, register: VirtOrPhysReg<ArmRegister>) -> ArmRegister {
        let register = match register {
            VirtOrPhysReg::Physical(reg) => return reg,
            VirtOrPhysReg::Virtual(reg) => reg,
        };

        match self.register_allocator.get_mapping(&register, self.current_instruction_id) {
            Some(register) => register,
            None => {
                // TODO: allocate on the stack in this case.
                panic!("Er is geen ARM-register beschikbaar voor IR-register {register}!");
            }
        }
    }

    fn dump_instructions(&self) {
        debug!("AArch64-instructies voor {}:", self.function_name);

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

    fn add_prologue(&mut self, instructions: &[Instruction]) {
        if self.is_pac_ret {
            self.add_instruction(ArmInstruction::PacIASp);
        }

        if !self.characteristics.is_leaf_function() {
            self.stack_allocator.reserve_save_frame_pointer(SPACE_NEEDED_FOR_FP_AND_LR);
        }

        self.stack_allocator.reserve_callee_saved_registers(self.register_allocator.callee_saved_registers_to_save().count());

        // ignoring possible optimizations, count up the needed stack size when the StackAlloc instruction is used.
        for (instruction_id, instruction) in instructions.iter().enumerate() {
            if let Instruction::StackAlloc { dst, size } = instruction {
                self.stack_allocator.reserve_stack_allocation(instruction_id, *dst, *size);
            }

            if let Instruction::Call { variable_arguments, .. } = instruction {
                if self.var_args_convention == AArch64VarArgsConvention::StackOnly {
                    let size = variable_arguments.iter()
                        .map(|arg| arg.size().next_multiple_of(8))
                        .sum();
                    if size != 0 {
                        self.stack_allocator.reserve_variadic_function_call_arguments(instruction_id, size);
                    }
                }
            }
        }

        // ensure the stack is 16-byte aligned
        self.stack_allocator.finalize();

        if self.stack_allocator.has_only_frame_pointer_reservation() && !self.characteristics.is_leaf_function() {
            self.add_prologue_instructions_stack_frame_optimization();
        } else {
            self.add_prologue_instructions_general();
        }

        self.add_prologue_register_saves();
    }

    fn add_prologue_instructions_general(&mut self) {
        if self.stack_allocator.total_size() == 0 {
            return;
        }

        self.add_instruction(ArmInstruction::SubImmediate {
            is_64_bit: true,
            dst: ArmRegister::SP,
            lhs: ArmRegister::SP,
            rhs_imm12: self.stack_allocator.total_size() as _,
        });

        if !self.characteristics.is_leaf_function() {
            self.add_instruction(ArmInstruction::Stp {
                is_64_bit: true,
                mode: ArmSignedAddressingMode::SignedOffset,
                dst: ArmRegister::SP,
                offset: self.stack_allocator.offset_of_frame_pointer() as _,
                first: ArmRegister::FP,
                second: ArmRegister::LR,
            });
        }
    }

    fn add_prologue_instructions_stack_frame_optimization(&mut self) {
        self.add_instruction(ArmInstruction::Stp {
            is_64_bit: true,
            mode: ArmSignedAddressingMode::PreIndex,
            dst: ArmRegister::SP,
            offset: self.stack_allocator.offset_of_frame_pointer() as _,
            first: ArmRegister::FP,
            second: ArmRegister::LR,
        });
    }

    fn add_prologue_register_saves(&mut self) {
        if self.stack_allocator.total_size() == 0 {
            return;
        }

        let mut offset = self.stack_allocator.offset_of_callee_register_saves();

        let mut chunks = self.register_allocator.callee_saved_registers_to_save().array_chunks::<2>();
        for register_pair in chunks.by_ref() {
            self.instructions.push(ArmInstruction::Stp {
                is_64_bit: true,
                mode: ArmSignedAddressingMode::SignedOffset,
                dst: ArmRegister::SP,
                first: register_pair[0],
                second: register_pair[1],
                offset: offset as _,
            });

            offset += POINTER_SIZE * 2;
        }
        if let Some(remainder) = chunks.into_remainder() {
            for reg in remainder {
                self.add_instruction(ArmInstruction::StrImmediate {
                    is_64_bit: true,
                    mode: ArmUnsignedAddressingMode::UnsignedOffset,
                    src: reg,
                    base_ptr: ArmRegister::SP,
                    offset: offset as _,
                });

                offset += POINTER_SIZE;
            }
        }
    }

    fn add_epilogue(&mut self) {
        if self.stack_allocator.total_size() == 0 {
            return;
        }

        self.add_epilogue_register_restores();

        if self.stack_allocator.has_only_frame_pointer_reservation() && !self.characteristics.is_leaf_function() {
            self.add_epilogue_instructions_stack_frame_optimization();
        } else {
            self.add_epilogue_instructions_general();
        }

        if self.is_pac_ret {
            self.add_instruction(ArmInstruction::AutIASp);
        }
    }

    fn add_epilogue_instructions_general(&mut self) {
        if !self.characteristics.is_leaf_function() {
            self.add_instruction(ArmInstruction::Ldp {
                is_64_bit: true,
                mode: ArmSignedAddressingMode::SignedOffset,
                src: ArmRegister::SP,
                offset: self.stack_allocator.offset_of_frame_pointer() as _,
                first: ArmRegister::FP,
                second: ArmRegister::LR,
            });
        }

        self.add_instruction(ArmInstruction::AddImmediate {
            is_64_bit: true,
            dst: ArmRegister::SP,
            src: ArmRegister::SP,
            imm12: self.stack_allocator.total_size() as _,
            shift: false,
        });
    }

    fn add_epilogue_instructions_stack_frame_optimization(&mut self) {
        self.add_instruction(ArmInstruction::Ldp {
            is_64_bit: true,
            mode: ArmSignedAddressingMode::PostIndex,
            src: ArmRegister::SP,
            offset: SPACE_NEEDED_FOR_FP_AND_LR as i16,
            first: ArmRegister::FP,
            second: ArmRegister::LR,
        });
    }

    fn add_epilogue_register_restores(&mut self) {
        if self.stack_allocator.total_size() == 0 {
            return;
        }

        let mut offset = self.stack_allocator.offset_of_callee_register_saves();

        let mut chunks = self.register_allocator.callee_saved_registers_to_save().array_chunks::<2>();
        for register_pair in chunks.by_ref() {
            self.instructions.push(ArmInstruction::Ldp {
                is_64_bit: true,
                mode: ArmSignedAddressingMode::SignedOffset,
                src: ArmRegister::SP,
                first: register_pair[0],
                second: register_pair[1],
                offset: offset as _,
            });

            offset += POINTER_SIZE * 2;
        }
        if let Some(remainder) = chunks.into_remainder() {
            for reg in remainder {
                self.add_instruction(ArmInstruction::LdrImmediate {
                    is_64_bit: true,
                    mode: ArmUnsignedAddressingMode::UnsignedOffset,
                    dst: reg,
                    base_ptr: ArmRegister::SP,
                    offset: offset as _,
                });

                offset += POINTER_SIZE;
            }
        }
    }

    fn materialize_instruction(&mut self, instruction: ArmInstruction<VirtOrPhysReg<ArmRegister>>) {
        match instruction {
            ArmInstruction::AddImmediate { is_64_bit, dst, src, imm12, shift } => {
                let dst = self.allocate_register(dst);
                let src = self.allocate_register(src);
                self.add_instruction(ArmInstruction::AddImmediate { is_64_bit, dst, src, imm12, shift });
            }

            ArmInstruction::AddRegister { is_64_bit, dst, lhs, rhs, imm, shift } => {
                let dst = self.allocate_register(dst);
                let lhs = self.allocate_register(lhs);
                let rhs = self.allocate_register(rhs);
                self.add_instruction(ArmInstruction::AddRegister { is_64_bit, dst, lhs, rhs, imm, shift });
            }

            ArmInstruction::Adrp { is_64_bit, dst, imm } => {
                let dst = self.allocate_register(dst);
                self.add_instruction(ArmInstruction::Adrp { is_64_bit, dst, imm });
            }

            ArmInstruction::AsrImmediate { is_64_bit, dst, src, amount } => {
                let dst = self.allocate_register(dst);
                let src = self.allocate_register(src);
                self.add_instruction(ArmInstruction::AsrImmediate { is_64_bit, dst, src, amount });
            }

            ArmInstruction::AsrRegister { is_64_bit, dst, src, amount } => {
                let dst = self.allocate_register(dst);
                let src = self.allocate_register(src);
                let amount = self.allocate_register(amount);
                self.add_instruction(ArmInstruction::AsrRegister { is_64_bit, dst, src, amount });
            }

            ArmInstruction::AutIASp => {
                self.add_instruction(ArmInstruction::AutIASp);
            }

            ArmInstruction::B { location } => {
                self.add_instruction(ArmInstruction::B { location });
            }

            ArmInstruction::BCond { cond, location } => {
                self.add_instruction(ArmInstruction::BCond { cond, location });
            }

            ArmInstruction::Bl { offset, symbol_name } => {
                self.relocations.push(Relocation {
                    ty: RelocationType::Function {
                        name: symbol_name.clone(),
                    },
                    offset: self.instructions.len() * 4,
                    method: RelocationMethod::AArch64BranchLink,
                });
                self.add_instruction(ArmInstruction::Bl { offset, symbol_name });
            }

            ArmInstruction::CmnImmediate { is_64_bit, register, value } => {
                let register = self.allocate_register(register);
                self.add_instruction(ArmInstruction::CmnImmediate { is_64_bit, register, value });
            }

            ArmInstruction::CmnRegister { is_64_bit, lhs, rhs } => {
                let lhs = self.allocate_register(lhs);
                let rhs = self.allocate_register(rhs);
                self.add_instruction(ArmInstruction::CmnRegister { is_64_bit, lhs, rhs });
            }

            ArmInstruction::CmpImmediate { is_64_bit, register, value } => {
                let register = self.allocate_register(register);
                self.add_instruction(ArmInstruction::CmpImmediate { is_64_bit, register, value });
            }

            ArmInstruction::CmpRegister { is_64_bit, lhs, rhs } => {
                let lhs = self.allocate_register(lhs);
                let rhs = self.allocate_register(rhs);
                self.add_instruction(ArmInstruction::CmpRegister { is_64_bit, lhs, rhs });
            }

            ArmInstruction::CSet { is_64_bit, dst, condition } => {
                let dst = self.allocate_register(dst);
                self.add_instruction(ArmInstruction::CSet { is_64_bit, dst, condition });
            }

            ArmInstruction::EorImmediate { is_64_bit, dst, reg, imm } => {
                let dst = self.allocate_register(dst);
                let reg = self.allocate_register(reg);
                self.add_instruction(ArmInstruction::EorImmediate { is_64_bit, dst, reg, imm });
            }

            ArmInstruction::EorRegister { is_64_bit, dst, lhs, rhs } => {
                let dst = self.allocate_register(dst);
                let lhs = self.allocate_register(lhs);
                let rhs = self.allocate_register(rhs);
                self.add_instruction(ArmInstruction::EorRegister { is_64_bit, dst, lhs, rhs });
            }

            ArmInstruction::Ldp { is_64_bit, mode, first, second, src, offset } => {
                let src = self.allocate_register(src);
                let first = self.allocate_register(first);
                let second = self.allocate_register(second);
                self.add_instruction(ArmInstruction::Ldp { is_64_bit, mode, first, second, src, offset });
            }

            ArmInstruction::LdrByteImmediate { mode, dst, base_ptr, offset } => {
                let dst = self.allocate_register(dst);
                let base_ptr = self.allocate_register(base_ptr);
                self.add_instruction(ArmInstruction::LdrByteImmediate { mode, dst, base_ptr, offset });
            }

            ArmInstruction::LdrByteRegister { dst, base_ptr, offset } => {
                let dst = self.allocate_register(dst);
                let base_ptr = self.allocate_register(base_ptr);
                let offset = self.allocate_register(offset);
                self.add_instruction(ArmInstruction::LdrByteRegister { dst, base_ptr, offset });
            }

            ArmInstruction::LdrHalfImmediate { mode, dst, base_ptr, offset } => {
                let dst = self.allocate_register(dst);
                let base_ptr = self.allocate_register(base_ptr);
                self.add_instruction(ArmInstruction::LdrHalfImmediate { mode, dst, base_ptr, offset });
            }

            ArmInstruction::LdrHalfRegister { dst, base_ptr, offset } => {
                let dst = self.allocate_register(dst);
                let base_ptr = self.allocate_register(base_ptr);
                let offset = self.allocate_register(offset);
                self.add_instruction(ArmInstruction::LdrHalfRegister { dst, base_ptr, offset });
            }

            ArmInstruction::LdrImmediate { is_64_bit, mode, dst, base_ptr, offset } => {
                let dst = self.allocate_register(dst);
                let base_ptr = self.allocate_register(base_ptr);
                self.add_instruction(ArmInstruction::LdrImmediate { is_64_bit, mode, dst, base_ptr, offset });
            }

            ArmInstruction::LdrRegister { is_64_bit, dst, base_ptr, offset } => {
                let dst = self.allocate_register(dst);
                let base_ptr = self.allocate_register(base_ptr);
                let offset = self.allocate_register(offset);
                self.add_instruction(ArmInstruction::LdrRegister { is_64_bit, dst, base_ptr, offset });
            }

            ArmInstruction::LslImmediate { is_64_bit, dst, src, amount } => {
                let dst = self.allocate_register(dst);
                let src = self.allocate_register(src);
                self.add_instruction(ArmInstruction::LslImmediate { is_64_bit, dst, src, amount });
            }

            ArmInstruction::LslRegister { is_64_bit, dst, src, amount } => {
                let dst = self.allocate_register(dst);
                let src = self.allocate_register(src);
                let amount = self.allocate_register(amount);
                self.add_instruction(ArmInstruction::LslRegister { is_64_bit, dst, src, amount });
            }

            ArmInstruction::MAdd { is_64_bit, dst, mul_lhs, mul_rhs, addend } => {
                let dst = self.allocate_register(dst);
                let mul_lhs = self.allocate_register(mul_lhs);
                let mul_rhs = self.allocate_register(mul_rhs);
                let addend = self.allocate_register(addend);
                self.add_instruction(ArmInstruction::MAdd { is_64_bit, dst, mul_lhs, mul_rhs, addend });
            }

            ArmInstruction::MovN { is_64_bit, register, unsigned_imm16 } => {
                let register = self.allocate_register(register);
                self.add_instruction(ArmInstruction::MovN { is_64_bit, register, unsigned_imm16 });
            }

            ArmInstruction::MovRegister32 { dst, src } => {
                let dst = self.allocate_register(dst);
                let src = self.allocate_register(src);
                self.add_instruction(ArmInstruction::MovRegister32 { dst, src });
            }

            ArmInstruction::MovRegister64 { dst, src } => {
                let dst = self.allocate_register(dst);
                let src = self.allocate_register(src);
                self.add_instruction(ArmInstruction::MovRegister64 { dst, src });
            }

            ArmInstruction::MovZ { register, imm16 } => {
                let register = self.allocate_register(register);
                self.add_instruction(ArmInstruction::MovZ { register, imm16 });
            }

            ArmInstruction::MSub { is_64_bit, dst, lhs, rhs, minuend } => {
                let dst = self.allocate_register(dst);
                let lhs = self.allocate_register(lhs);
                let rhs = self.allocate_register(rhs);
                let minuend = self.allocate_register(minuend);
                self.add_instruction(ArmInstruction::MSub { is_64_bit, dst, lhs, rhs, minuend });
            }

            ArmInstruction::Mul { is_64_bit, dst, lhs, rhs } => {
                let dst = self.allocate_register(dst);
                let lhs = self.allocate_register(lhs);
                let rhs = self.allocate_register(rhs);
                self.add_instruction(ArmInstruction::Mul { is_64_bit, dst, lhs, rhs });
            }

            ArmInstruction::Neg { is_64_bit, shift, shift_amount, dst, src } => {
                let dst = self.allocate_register(dst);
                let src = self.allocate_register(src);
                self.add_instruction(ArmInstruction::Neg { is_64_bit, shift, shift_amount, dst, src });
            }

            ArmInstruction::PacIASp => {
                self.add_instruction(ArmInstruction::PacIASp);
            }

            ArmInstruction::Ret => {
                self.add_instruction(ArmInstruction::Ret);
            }

            ArmInstruction::SDiv { is_64_bit, dst, lhs, rhs } => {
                let dst = self.allocate_register(dst);
                let lhs = self.allocate_register(lhs);
                let rhs = self.allocate_register(rhs);
                self.add_instruction(ArmInstruction::SDiv { is_64_bit, dst, lhs, rhs });
            }

            ArmInstruction::Stp { is_64_bit, mode, dst, offset, first, second } => {
                let dst = self.allocate_register(dst);
                let first = self.allocate_register(first);
                let second = self.allocate_register(second);
                self.add_instruction(ArmInstruction::Stp { is_64_bit, mode, dst, offset, first, second });
            }

            ArmInstruction::StrByteImmediate { mode, src, base_ptr, offset } => {
                let src = self.allocate_register(src);
                let base_ptr = self.allocate_register(base_ptr);
                self.add_instruction(ArmInstruction::StrByteImmediate { mode, src, base_ptr, offset });
            }

            ArmInstruction::StrByteRegister { src, base_ptr, offset } => {
                let src = self.allocate_register(src);
                let base_ptr = self.allocate_register(base_ptr);
                let offset = self.allocate_register(offset);
                self.add_instruction(ArmInstruction::StrByteRegister { src, base_ptr, offset });
            }

            ArmInstruction::StrHalfImmediate { mode, src, base_ptr, offset } => {
                let src = self.allocate_register(src);
                let base_ptr = self.allocate_register(base_ptr);
                self.add_instruction(ArmInstruction::StrHalfImmediate { mode, src, base_ptr, offset });
            }

            ArmInstruction::StrHalfRegister { src, base_ptr, offset } => {
                let src = self.allocate_register(src);
                let base_ptr = self.allocate_register(base_ptr);
                let offset = self.allocate_register(offset);
                self.add_instruction(ArmInstruction::StrHalfRegister { src, base_ptr, offset });
            }

            ArmInstruction::StrImmediate { is_64_bit, mode, src, base_ptr, offset } => {
                let src = self.allocate_register(src);
                let base_ptr = self.allocate_register(base_ptr);
                self.add_instruction(ArmInstruction::StrImmediate { is_64_bit, mode, src, base_ptr, offset });
            }

            ArmInstruction::StrRegister { is_64_bit, src, base_ptr, offset } => {
                let src = self.allocate_register(src);
                let base_ptr = self.allocate_register(base_ptr);
                let offset = self.allocate_register(offset);
                self.add_instruction(ArmInstruction::StrRegister { is_64_bit, src, base_ptr, offset });
            }

            ArmInstruction::SubImmediate { is_64_bit, dst, lhs, rhs_imm12 } => {
                let dst = self.allocate_register(dst);
                let lhs = self.allocate_register(lhs);
                self.add_instruction(ArmInstruction::SubImmediate { is_64_bit, dst, lhs, rhs_imm12 });
            }

            ArmInstruction::SubRegister { is_64_bit, dst, lhs, rhs, shift, shift_mode } => {
                let dst = self.allocate_register(dst);
                let lhs = self.allocate_register(lhs);
                let rhs = self.allocate_register(rhs);
                self.add_instruction(ArmInstruction::SubRegister { is_64_bit, dst, lhs, rhs, shift, shift_mode });
            }

            ArmInstruction::Label(label) => {
                self.label_offsets.insert(label, self.instructions.len());
            }

            ArmInstruction::FixUp(AArch64FixUp::StackAlloc { dst, instruction_id }) => {
                let dst = self.allocate_register(dst);
                let offset = self.stack_allocator.offset_of_reg(instruction_id);

                self.instructions.push(ArmInstruction::AddImmediate {
                    is_64_bit: true,
                    dst,
                    src: ArmRegister::SP,
                    imm12: offset as _,
                    shift: false,
                });
            }

            ArmInstruction::FixUp(AArch64FixUp::StoreVariadic { is_64_bit, src, instruction_id, offset }) => {
                let base_offset = self.stack_allocator.offset_of_variadic_args(instruction_id);
                let src = self.allocate_register(src);

                self.instructions.push(ArmInstruction::StrImmediate {
                    is_64_bit,
                    src,
                    base_ptr: ArmRegister::SP,
                    offset: base_offset as i16 + offset,
                    mode: ArmUnsignedAddressingMode::UnsignedOffset,
                });
            }
        }
    }

    fn add_instruction(&mut self, instruction: ArmInstruction<ArmRegister>) {
        if is_instruction_redundant(&instruction) {
            return;
        }

        self.instructions.push(instruction);
    }
}

#[must_use]
fn is_instruction_redundant(instruction: &ArmInstruction<ArmRegister>) -> bool {
    match instruction {
        ArmInstruction::MovRegister32 { dst, src } | ArmInstruction::MovRegister64 { dst, src } => {
            dst == src
        }

        _ => false,
    }
}

impl CodeGenerator for AArch64CodeGenerator {
    fn compile(function: &Function) -> CompiledFunction {
        Self::compile(function, Platform::host_platform())
    }
}
