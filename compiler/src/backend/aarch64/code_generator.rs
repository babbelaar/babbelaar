// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::HashMap, mem::take};

use babbelaar::BabString;
use log::{debug, warn};

use crate::{CodeGenerator, CompiledFunction, Function, Instruction, Label, MathOperation, Operand, Register, RegisterAllocator, Relocation, RelocationMethod, RelocationType};

use super::{AArch64FunctionCharacteristics, ArmBranchLocation, ArmConditionCode, ArmInstruction, ArmRegister, ArmShift2, ArmSignedAddressingMode, ArmUnsignedAddressingMode};

const POINTER_SIZE: usize = 8;
const STACK_ALIGNMENT: usize = 16;

const SPACE_NEEDED_FOR_FP_AND_LR: usize = 2 * POINTER_SIZE;

#[derive(Debug)]
pub struct AArch64CodeGenerator {
    function_name: BabString,
    characteristics: AArch64FunctionCharacteristics,
    instructions: Vec<ArmInstruction>,
    label_offsets: HashMap<Label, usize>,
    register_allocator: RegisterAllocator<ArmRegister>,
    stack_size: usize,
    space_used_on_stack: usize,
    relocations: Vec<Relocation>,
}

impl AArch64CodeGenerator {
    #[must_use]
    pub fn compile(function: &Function) -> CompiledFunction {
        let characteristics = AArch64FunctionCharacteristics::analyze(function);
        let mut this = Self {
            function_name: function.name().clone(),
            characteristics,
            instructions: Vec::new(),
            label_offsets: HashMap::new(),
            register_allocator: RegisterAllocator::new(function),
            stack_size: 0,
            space_used_on_stack: 0,
            relocations: Vec::new(),
        };

        this.add_prologue(function.instructions());

        for instruction in function.instructions() {
            this.add_instruction(instruction);
        }

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

    fn add_instruction(&mut self, instruction: &Instruction) {
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

            Instruction::Move { source, destination } => {
                let dst = self.allocate_register(destination);

                match source {
                    Operand::Immediate(immediate) => {
                        if immediate.as_i64() < 0 {
                            self.instructions.push(ArmInstruction::MovN {
                                is_64_bit: false,
                                register: dst,
                                unsigned_imm16: (immediate.as_i64().wrapping_neg()) as _,
                            });
                        } else {
                            self.instructions.push(ArmInstruction::MovZ {
                                register: dst,
                                imm16: immediate.as_i64() as _,
                            });
                        }
                    }

                    Operand::Register(source) => {
                        let src = self.allocate_register(source);

                        if dst != src {
                            self.instructions.push(ArmInstruction::MovRegister64 { dst, src });
                        }
                    }
                }
            }

            Instruction::MoveAddress { destination, offset } => {
                let dst = self.allocate_register(destination);
                let section = offset.section_kind();

                self.relocations.push(Relocation {
                    ty: RelocationType::Data {
                        section,
                        offset: offset.offset(),
                    },
                    offset: self.instructions.len() * 4,
                    method: RelocationMethod::Aarch64Page21,
                });

                self.instructions.push(ArmInstruction::Adrp {
                    dst,
                    imm: 0,
                });

                self.relocations.push(Relocation {
                    ty: RelocationType::Data {
                        section,
                        offset: offset.offset(),
                    },
                    offset: self.instructions.len() * 4,
                    method: RelocationMethod::Aarch64PageOff12,
                });

                self.instructions.push(ArmInstruction::AddImmediate {
                    dst,
                    src: dst,
                    imm12: 0,
                    shift: false,
                });
            }

            Instruction::MoveCondition { destination, condition } => {
                let dst = self.allocate_register(destination);
                let condition = ArmConditionCode::from(*condition);
                self.instructions.push(ArmInstruction::CSet {
                    is_64_bit: true,
                    dst,
                    condition,
                })
            }

            Instruction::Call { name, arguments, ret_val_reg } => {
                debug_assert!(arguments.len() < (1 << 8));

                for (idx, arg) in arguments.iter().enumerate() {
                    let reg = self.allocate_register(arg);

                    if reg.number != idx as u8 {
                        self.instructions.push(ArmInstruction::MovRegister64 {
                            dst: ArmRegister { number: idx as _ },
                            src: reg,
                        });
                    }
                }

                self.relocations.push(Relocation {
                    ty: RelocationType::Function {
                        name: name.clone(),
                    },
                    offset: self.instructions.len() * 4,
                    method: RelocationMethod::AArch64BranchLink,
                });

                self.instructions.push(ArmInstruction::Bl {
                    symbol_name: name.clone(),
                    offset: 0,
                });

                let ret_val_reg = self.allocate_register(ret_val_reg);
                if ret_val_reg != ArmRegister::X0 {
                    self.instructions.push(ArmInstruction::MovRegister64 {
                        dst: ret_val_reg,
                        src: ArmRegister::X0,
                    });
                }
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
                self.add_epilogue();

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
                    MathOperation::Divide => self.add_instruction_sdiv(dst, lhs, rhs),
                    MathOperation::Multiply => self.add_instruction_mul(dst, lhs, rhs),
                    MathOperation::Subtract => self.add_instruction_sub(dst, lhs, rhs),
                    MathOperation::Modulo => self.add_operation_modulo(dst, lhs, rhs),
                }
            }

            Instruction::Negate { dst, src } => {
                let dst = self.allocate_register(dst);
                let src = self.allocate_register(src);

                self.instructions.push(ArmInstruction::Neg {
                    is_64_bit: true,
                    shift: ArmShift2::default(),
                    shift_amount: 0,
                    dst,
                    src,
                });
            }

            Instruction::StackAlloc { dst, size } => {
                let dst = self.allocate_register(dst);

                self.instructions.push(ArmInstruction::AddImmediate {
                    dst,
                    src: ArmRegister::SP,
                    imm12: self.space_used_on_stack as _,
                    shift: false,
                });

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

                self.instructions.push(ArmInstruction::LdrImmediate {
                    is_64_bit,
                    mode: ArmUnsignedAddressingMode::UnsignedOffset,
                    dst,
                    base_ptr,
                    offset: offset.as_i16(),
                });
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

                let src = self.allocate_register(value);

                self.instructions.push(ArmInstruction::StrImmediate {
                    is_64_bit,
                    mode: ArmUnsignedAddressingMode::UnsignedOffset,
                    src,
                    base_ptr,
                    offset: offset.as_i16(),
                });
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

    fn add_instruction_mul(&mut self, dst: ArmRegister, lhs: &Operand, rhs: &Operand) {
        match (lhs, rhs) {
            (Operand::Immediate(lhs), Operand::Immediate(rhs)) => {
                let imm16 = lhs.as_i64() * rhs.as_i64();
                debug_assert!(imm16 < (1 << 16));
                self.instructions.push(ArmInstruction::MovZ {
                    register: dst,
                    imm16: imm16 as _,
                });
            }

            (Operand::Register(lhs), Operand::Register(rhs)) => {
                let lhs = self.allocate_register(lhs);
                let rhs = self.allocate_register(rhs);
                self.instructions.push(ArmInstruction::Mul {
                    is_64_bit: true,
                    dst,
                    lhs,
                    rhs,
                });
            }

            (Operand::Register(lhs), Operand::Immediate(rhs)) => {
                todo!("Mul {lhs}, {rhs}")
            }

            (Operand::Immediate(lhs), Operand::Register(rhs)) => {
                todo!("Mul {lhs}, {rhs}")
            }
        }
    }

    fn add_instruction_sdiv(&mut self, dst: ArmRegister, lhs: &Operand, rhs: &Operand) {
        match (lhs, rhs) {
            (Operand::Immediate(lhs), Operand::Immediate(rhs)) => {
                warn!("Immediate-DeelDoor zou gedaan moeten worden door de optimalisator!");

                let imm16 = lhs.as_i64() / rhs.as_i64();
                debug_assert!(imm16 < (1 << 16));
                self.instructions.push(ArmInstruction::MovZ {
                    register: dst,
                    imm16: imm16 as _,
                });
            }

            (Operand::Register(lhs), Operand::Register(rhs)) => {
                let lhs = self.allocate_register(lhs);
                let rhs = self.allocate_register(rhs);
                self.instructions.push(ArmInstruction::SDiv {
                    is_64_bit: true,
                    dst,
                    lhs,
                    rhs,
                });
            }

            (Operand::Immediate(lhs), Operand::Register(rhs)) => {
                let rhs = self.allocate_register(rhs);

                let lhs_reg = if rhs == dst {
                    self.register_allocator.hacky_random_available_register().unwrap()
                } else {
                    dst
                };

                // TODO: add good mov subroutine for stuff like this
                self.instructions.push(ArmInstruction::MovZ {
                    register: lhs_reg,
                    imm16: lhs.as_i64() as _,
                });

                self.instructions.push(ArmInstruction::SDiv {
                    is_64_bit: true,
                    dst,
                    lhs: lhs_reg,
                    rhs,
                });
            }

            (Operand::Register(lhs), Operand::Immediate(rhs)) => {
                let lhs = self.allocate_register(lhs);

                let rhs_reg = if lhs == dst {
                    self.register_allocator.hacky_random_available_register().unwrap()
                } else {
                    dst
                };

                // TODO: add good mov subroutine for stuff like this
                self.instructions.push(ArmInstruction::MovZ {
                    register: rhs_reg,
                    imm16: rhs.as_i64() as _,
                });

                self.instructions.push(ArmInstruction::SDiv {
                    is_64_bit: true,
                    dst,
                    lhs,
                    rhs: rhs_reg,
                });
            }
        }
    }

    fn add_instruction_sub(&mut self, dst: ArmRegister, lhs: &Operand, rhs: &Operand) {
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
                self.instructions.push(ArmInstruction::SubRegister {
                    is_64_bit: true,
                    dst,
                    lhs,
                    rhs,
                    shift: 0,
                    shift_mode: ArmShift2::default(),
                });
            }

            (Operand::Immediate(lhs), Operand::Register(rhs)) => {
                let src = self.allocate_register(rhs);

                let imm12 = (-lhs.as_i64()) as _;
                debug_assert!(imm12 < (1 << 12));

                self.instructions.push(ArmInstruction::AddImmediate {
                    dst,
                    src,
                    imm12,
                    shift: false,
                });
            }

            (Operand::Register(lhs), Operand::Immediate(rhs)) => {
                let lhs = self.allocate_register(lhs);
                let rhs_imm12 = rhs.as_i64() as _;
                debug_assert!(rhs_imm12 < (1 << 12));
                self.instructions.push(ArmInstruction::SubImmediate {
                    dst,
                    lhs,
                    rhs_imm12,
                });
            }
        }
    }

    /// AArch64 doesn't have a MOD/REM instruction, so we have to use
    fn add_operation_modulo(&mut self, dst: ArmRegister, lhs: &Operand, rhs: &Operand) {
        let (lhs, rhs) = match (lhs, rhs) {
            (Operand::Immediate(lhs), Operand::Immediate(rhs)) => {
                warn!("Immediate-modulo zou gedaan moeten worden door de optimalisator!");

                let val = lhs.as_i64() % rhs.as_i64();

                self.instructions.push(ArmInstruction::MovN {
                    is_64_bit: true,
                    register: dst,
                    unsigned_imm16: val as u16,
                });
                return;
            }

            (Operand::Register(lhs), Operand::Register(rhs)) => {
                let lhs = self.allocate_register(lhs);
                let rhs = self.allocate_register(rhs);
                (lhs, rhs)
            }

            (Operand::Immediate(lhs), Operand::Register(rhs)) => {
                let rhs = self.allocate_register(rhs);

                let lhs_reg = if rhs == dst {
                    self.register_allocator.hacky_random_available_register().unwrap()
                } else {
                    dst
                };

                // TODO: add good mov subroutine for stuff like this
                self.instructions.push(ArmInstruction::MovZ {
                    register: lhs_reg,
                    imm16: lhs.as_i64() as _,
                });

                (lhs_reg, rhs)
            }

            (Operand::Register(lhs), Operand::Immediate(rhs)) => {
                let lhs = self.allocate_register(lhs);

                let rhs_reg = if lhs == dst {
                    self.register_allocator.hacky_random_available_register().unwrap()
                } else {
                    dst
                };

                // TODO: add good mov subroutine for stuff like this
                self.instructions.push(ArmInstruction::MovZ {
                    register: rhs_reg,
                    imm16: rhs.as_i64() as _,
                });

                (lhs, rhs_reg)
            }
        };

        let tmp = if dst == lhs || dst == rhs {
            self.register_allocator.hacky_random_available_register().unwrap()
        } else {
            dst
        };

        self.instructions.push(ArmInstruction::SDiv {
            is_64_bit: true,
            dst: tmp,
            lhs,
            rhs,
        });

        self.instructions.push(ArmInstruction::MSub {
            is_64_bit: true,
            dst,
            lhs: tmp,
            rhs,
            minuend: lhs,
        });
    }

    #[must_use]
    fn allocate_register(&mut self, register: &Register) -> ArmRegister {
        match self.register_allocator.get_mapping(register) {
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
        // begin with space for Frame Pointer and Link Register
        self.stack_size = 0;

        if !self.characteristics.is_leaf_function() {
            self.stack_size += SPACE_NEEDED_FOR_FP_AND_LR;
        }

        self.stack_size += (self.register_allocator.callee_saved_registers_to_save().len() * POINTER_SIZE).next_multiple_of(STACK_ALIGNMENT);

        // ignoring possible optimizations, count up the needed stack size when the StackAlloc instruction is used.
        for instruction in instructions {
            if let Instruction::StackAlloc { dst, size } = instruction {
                _ = dst; // we don't encode this here
                self.stack_size += size;
            }
        }

        // ensure the stack is 16-byte aligned
        self.stack_size = self.stack_size.next_multiple_of(16);

        assert!(self.stack_size < (1 << 12));

        if self.stack_size == SPACE_NEEDED_FOR_FP_AND_LR && !self.characteristics.is_leaf_function() {
            self.add_prologue_instructions_stack_frame_optimization();
        } else {
            self.add_prologue_instructions_general();
        }

        self.add_prologue_register_saves();
    }

    fn add_prologue_instructions_general(&mut self) {
        if self.stack_size == 0 {
            return;
        }

        self.instructions.push(ArmInstruction::SubImmediate {
            dst: ArmRegister::SP,
            lhs: ArmRegister::SP,
            rhs_imm12: self.stack_size as _,
        });

        if !self.characteristics.is_leaf_function() {
            self.instructions.push(ArmInstruction::Stp {
                is_64_bit: true,
                mode: ArmSignedAddressingMode::SignedOffset,
                dst: ArmRegister::SP,
                offset: (self.stack_size - SPACE_NEEDED_FOR_FP_AND_LR) as _,
                first: ArmRegister::FP,
                second: ArmRegister::LR,
            });
        }
    }

    fn add_prologue_instructions_stack_frame_optimization(&mut self) {
        self.instructions.push(ArmInstruction::Stp {
            is_64_bit: true,
            mode: ArmSignedAddressingMode::PreIndex,
            dst: ArmRegister::SP,
            offset: -(SPACE_NEEDED_FOR_FP_AND_LR as i16),
            first: ArmRegister::FP,
            second: ArmRegister::LR,
        });
    }

    fn add_prologue_register_saves(&mut self) {
        if self.stack_size == 0 {
            return;
        }

        let mut base_offset = self.stack_size;
        if !self.characteristics.is_leaf_function() {
            base_offset -= SPACE_NEEDED_FOR_FP_AND_LR;
        }

        // TODO: use `array_chunks()` when it is stabilized
        let pairs = self.register_allocator.callee_saved_registers_to_save().chunks(2);
        for register_pair in pairs {
            let offset = (base_offset - POINTER_SIZE * 2) as _;

            if register_pair.len() == 1 {
                self.instructions.push(ArmInstruction::StrImmediate {
                    is_64_bit: true,
                    mode: ArmUnsignedAddressingMode::UnsignedOffset,
                    src: register_pair[0],
                    base_ptr: ArmRegister::SP,
                    offset,
                });
            } else {
                self.instructions.push(ArmInstruction::Stp {
                    is_64_bit: true,
                    mode: ArmSignedAddressingMode::SignedOffset,
                    dst: ArmRegister::SP,
                    first: register_pair[0],
                    second: register_pair[1],
                    offset,
                })
            }

            base_offset -= POINTER_SIZE * 2;
        }
    }

    fn add_epilogue(&mut self) {
        if self.stack_size == 0 {
            return;
        }

        self.add_epilogue_register_restores();

        if self.stack_size == SPACE_NEEDED_FOR_FP_AND_LR && !self.characteristics.is_leaf_function() {
            self.add_epilogue_instructions_stack_frame_optimization();
        } else {
            self.add_epilogue_instructions_general();
        }
    }

    fn add_epilogue_instructions_general(&mut self) {
        if !self.characteristics.is_leaf_function() {
            self.instructions.push(ArmInstruction::Ldp {
                is_64_bit: true,
                mode: ArmSignedAddressingMode::SignedOffset,
                src: ArmRegister::SP,
                offset: (self.stack_size - SPACE_NEEDED_FOR_FP_AND_LR) as _,
                first: ArmRegister::FP,
                second: ArmRegister::LR,
            });
        }

        self.instructions.push(ArmInstruction::AddImmediate {
            dst: ArmRegister::SP,
            src: ArmRegister::SP,
            imm12: self.stack_size as _,
            shift: false,
        });
    }

    fn add_epilogue_instructions_stack_frame_optimization(&mut self) {
        self.instructions.push(ArmInstruction::Ldp {
            is_64_bit: true,
            mode: ArmSignedAddressingMode::PostIndex,
            src: ArmRegister::SP,
            offset: SPACE_NEEDED_FOR_FP_AND_LR as i16,
            first: ArmRegister::FP,
            second: ArmRegister::LR,
        });
    }

    fn add_epilogue_register_restores(&mut self) {
        if self.stack_size == 0 {
            return;
        }

        let mut base_offset = self.stack_size;
        if !self.characteristics.is_leaf_function() {
            base_offset -= SPACE_NEEDED_FOR_FP_AND_LR;
        }

        // TODO: use `array_chunks()` when it is stabilized
        let pairs = self.register_allocator.callee_saved_registers_to_save().chunks(2);
        for register_pair in pairs {
            let offset = (base_offset - POINTER_SIZE * 2) as _;

            if register_pair.len() == 1 {
                self.instructions.push(ArmInstruction::LdrImmediate {
                    is_64_bit: true,
                    mode: ArmUnsignedAddressingMode::UnsignedOffset,
                    dst: register_pair[0],
                    base_ptr: ArmRegister::SP,
                    offset,
                });
            } else {
                self.instructions.push(ArmInstruction::Ldp {
                    is_64_bit: true,
                    mode: ArmSignedAddressingMode::SignedOffset,
                    src: ArmRegister::SP,
                    first: register_pair[0],
                    second: register_pair[1],
                    offset,
                })
            }

            base_offset -= POINTER_SIZE * 2;
        }
    }
}

impl CodeGenerator for AArch64CodeGenerator {
    fn compile(function: &Function) -> CompiledFunction {
        Self::compile(function)
    }
}
