// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::collections::VecDeque;

use babbelaar::BabString;
use log::{debug, warn};

use crate::{backend::{aarch64::AArch64VarArgsConvention, VirtOrPhysReg}, ir::RegisterAllocator as IrRegisterAllocator, Environment, Function, Immediate, Instruction, MathOperation, Operand, Platform, PrimitiveType, Register, Relocation, RelocationMethod, RelocationType, TargetInstruction};

use super::{fixup::AArch64FixUp, ArmConditionCode, ArmInstruction, ArmRegister, ArmShift2, ArmUnsignedAddressingMode};

#[derive(Debug)]
pub struct AArch64InstructionSelector {
    pub(super) function_name: BabString,
    pub(super) instructions: Vec<ArmInstruction<VirtOrPhysReg<ArmRegister>>>,
    pub(super) ir_reg_allocator: IrRegisterAllocator,
    pub(super) relocations: VecDeque<Relocation>,

    pub(super) var_args_convention: AArch64VarArgsConvention,
    pub(super) current_instruction_id: usize,
}

impl AArch64InstructionSelector {
    #[must_use]
    pub fn compile(function: &Function, platform: &Platform) -> Self {
        debug!("Werkwijze `{}` aan het compileren...", function.name());

        let var_args_convention = match platform.environment() {
            Environment::Darwin => AArch64VarArgsConvention::StackOnly,
            _ => todo!("ondersteun niet-macOS AArch64")
        };

        let mut this = Self {
            function_name: function.name().clone(),
            instructions: Vec::new(),
            var_args_convention,
            current_instruction_id: 0,
            ir_reg_allocator: function.ir_register_allocator.clone(),
            relocations: Default::default(),
        };

        for (index, instruction) in function.instructions().iter().enumerate() {
            this.add_instruction(index, instruction);
        }

        this.dump_instructions();

        this
    }

    fn add_instruction(&mut self, instruction_id: usize, instruction: &Instruction) {
        self.current_instruction_id = instruction_id;

        match instruction {
            Instruction::Compare { lhs, rhs, typ } => {
                self.add_instruction_cmp(lhs, rhs, *typ);
            }

            Instruction::Increment { register, typ } => {
                let dst = self.allocate_register(register);
                let src = dst;
                let imm12 = 1;
                let shift = false;
                self.instructions.push(ArmInstruction::AddImmediate { is_64_bit: typ.is_arm_64_bit(), dst, src, imm12, shift });
            }

            Instruction::Move { source, destination } => {
                let dst = self.allocate_register(destination);

                self.add_instruction_mov(PrimitiveType::new(8, true), dst, source);
            }

            Instruction::MoveAddress { destination, offset } => {
                let dst = self.allocate_register(destination);
                let section = offset.section_kind();

                self.relocations.push_back(Relocation {
                    ty: RelocationType::Data {
                        section,
                        offset: offset.offset(),
                    },
                    offset: self.instructions.len(),
                    method: RelocationMethod::Aarch64Page21,
                });

                self.instructions.push(ArmInstruction::Adrp {
                    is_64_bit: true,
                    dst,
                    imm: 0,
                });

                self.relocations.push_back(Relocation {
                    ty: RelocationType::Data {
                        section,
                        offset: offset.offset(),
                    },
                    offset: self.instructions.len(),
                    method: RelocationMethod::Aarch64PageOff12,
                });

                self.instructions.push(ArmInstruction::AddImmediate {
                    is_64_bit: true,
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

            Instruction::Call { name, arguments, variable_arguments, ret_val_reg } => {
                debug_assert!(arguments.len() < (1 << 8));

                let register_var_args = match self.var_args_convention {
                    AArch64VarArgsConvention::RegistersAndStack => {
                        variable_arguments.as_slice()
                    }

                    AArch64VarArgsConvention::StackOnly => {
                        if !variable_arguments.is_empty() {
                            let mut offset = 0;

                            for arg in variable_arguments.clone() {
                                let src = self.allocate_register(&arg.register());

                                self.instructions.push(ArmInstruction::FixUp(AArch64FixUp::StoreVariadic {
                                    is_64_bit: arg.size() == 8,
                                    src,
                                    instruction_id,
                                    offset,
                                }));

                                offset += arg.size().next_multiple_of(8) as i16;
                            }
                        }

                        &[]
                    }
                };

                for (idx, arg) in arguments.iter().chain(register_var_args.iter()).enumerate() {
                    let reg = self.allocate_register(&arg.register());
                    let arg_reg = VirtOrPhysReg::Physical(ArmRegister { number: idx as u8 });

                    if reg != arg_reg {
                        self.instructions.push(ArmInstruction::MovRegister64 {
                            dst: arg_reg,
                            src: reg,
                        });
                    }
                }

                self.instructions.push(ArmInstruction::Bl {
                    symbol_name: name.clone(),
                    offset: 0,
                });

                if let Some(ret_val_reg) = ret_val_reg {
                    let ret_val_reg = self.allocate_register(ret_val_reg);
                    if ret_val_reg != VirtOrPhysReg::Physical(ArmRegister::X0) {
                        self.instructions.push(ArmInstruction::MovRegister64 {
                            dst: ret_val_reg,
                            src: VirtOrPhysReg::Physical(ArmRegister::X0),
                        });
                    }
                }
            }

            Instruction::Jump { location } => {
                let location = *location;
                self.instructions.push(ArmInstruction::B { location });
            }

            Instruction::JumpConditional { condition, location } => {
                let cond = ArmConditionCode::from(*condition);
                let location = *location;
                self.instructions.push(ArmInstruction::BCond { cond, location });
            }

            Instruction::Label(label) => {
                self.instructions.push(ArmInstruction::Label(*label));
            }

            Instruction::InitArg { destination, arg_idx } => {
                let reg = self.allocate_register(destination);
                let arg_reg = VirtOrPhysReg::Physical(ArmRegister { number: *arg_idx as u8 });
                if reg != arg_reg {
                    self.instructions.push(ArmInstruction::MovRegister64 {
                        dst: reg,
                        src: arg_reg,
                    });
                }
            }

            Instruction::Return { value_reg } => {
                if let Some(value_reg) = value_reg {
                    let value_reg = self.allocate_register(value_reg);
                    let ret_reg = VirtOrPhysReg::Physical(ArmRegister::X0);
                    if value_reg != ret_reg {
                        self.instructions.push(ArmInstruction::MovRegister64 { dst: ret_reg, src: value_reg });
                    }
                }

                self.instructions.push(ArmInstruction::Ret);
            }

            Instruction::MathOperation { operation, typ, destination, lhs, rhs } => {
                let typ = *typ;
                let dst = self.allocate_register(destination);

                match operation {
                    MathOperation::Add => self.add_instruction_add(typ, dst, lhs, rhs),
                    MathOperation::Divide => self.add_instruction_sdiv(typ, dst, lhs, rhs),
                    MathOperation::Multiply => self.add_instruction_mul(typ, dst, lhs, rhs),
                    MathOperation::Subtract => self.add_instruction_sub(typ, dst, lhs, rhs),
                    MathOperation::Modulo => self.add_operation_modulo(typ, dst, lhs, rhs),
                    MathOperation::LeftShift => self.add_instruction_lsl(typ, dst, lhs, rhs),
                    MathOperation::RightShift => self.add_instruction_asr(typ, dst, lhs, rhs),
                    MathOperation::Xor => self.add_instruction_eor(typ, dst, lhs, rhs),
                }
            }

            Instruction::Negate { typ, dst, src } => {
                let dst = self.allocate_register(dst);
                let src = self.allocate_register(src);

                self.instructions.push(ArmInstruction::Neg {
                    is_64_bit: typ.is_arm_64_bit(),
                    shift: ArmShift2::default(),
                    shift_amount: 0,
                    dst,
                    src,
                });
            }

            Instruction::StackAlloc { dst, size: _ } => {
                let dst = self.allocate_register(dst);

                self.instructions.push(ArmInstruction::FixUp(
                    AArch64FixUp::StackAlloc {
                        dst,
                        instruction_id,
                    }
                ));
            }

            Instruction::LoadPtr { destination, base_ptr, offset, typ } => {
                let dst = self.allocate_register(destination);
                let base_ptr = self.allocate_register(base_ptr);

                match typ.bytes() {
                    1 => match offset {
                        Operand::Immediate(offset) => {
                            self.instructions.push(ArmInstruction::LdrByteImmediate {
                                mode: ArmUnsignedAddressingMode::UnsignedOffset,
                                dst,
                                base_ptr,
                                offset: offset.as_i16(),
                            });
                        }

                        Operand::Register(offset) => {
                            let offset = self.allocate_register(offset);
                            self.instructions.push(ArmInstruction::LdrByteRegister {
                                dst,
                                base_ptr,
                                offset,
                            });
                        }
                    }

                    2 => match offset {
                        Operand::Immediate(offset) => {
                            self.instructions.push(ArmInstruction::LdrHalfImmediate {
                                mode: ArmUnsignedAddressingMode::UnsignedOffset,
                                dst,
                                base_ptr,
                                offset: offset.as_i16(),
                            });
                        }

                        Operand::Register(offset) => {
                            let offset = self.allocate_register(offset);
                            self.instructions.push(ArmInstruction::LdrHalfRegister {
                                dst,
                                base_ptr,
                                offset,
                            });
                        }
                    }

                    4 => match offset {
                        Operand::Immediate(offset) => {
                            self.instructions.push(ArmInstruction::LdrImmediate {
                                is_64_bit: false,
                                mode: ArmUnsignedAddressingMode::UnsignedOffset,
                                dst,
                                base_ptr,
                                offset: offset.as_i16(),
                            });
                        }

                        Operand::Register(offset) => {
                            let offset = self.allocate_register(offset);
                            self.instructions.push(ArmInstruction::LdrRegister {
                                is_64_bit: false,
                                dst,
                                base_ptr,
                                offset,
                            });
                        }
                    }

                    8 => match offset {
                        Operand::Immediate(offset) => {
                            self.instructions.push(ArmInstruction::LdrImmediate {
                                is_64_bit: true,
                                mode: ArmUnsignedAddressingMode::UnsignedOffset,
                                dst,
                                base_ptr,
                                offset: offset.as_i16(),
                            });
                        }

                        Operand::Register(offset) => {
                            let offset = self.allocate_register(offset);
                            self.instructions.push(ArmInstruction::LdrRegister {
                                is_64_bit: true,
                                dst,
                                base_ptr,
                                offset,
                            });
                        }
                    }

                    _ => todo!("We ondersteunen alleen 1, 2, 4 en 8 byte Laad ARM-instructies, maar kreeg type: {typ:?}"),
                }
            }

            Instruction::StorePtr { base_ptr, offset, value, typ } => {
                let base_ptr = self.allocate_register(base_ptr);

                let src = self.allocate_register(value);

                match typ.bytes() {
                    1 => match offset {
                        Operand::Immediate(offset) => {
                            self.instructions.push(ArmInstruction::StrByteImmediate {
                                mode: ArmUnsignedAddressingMode::UnsignedOffset,
                                src,
                                base_ptr,
                                offset: offset.as_i16(),
                            });
                        }

                        Operand::Register(offset) => {
                            let offset = self.allocate_register(offset);
                            self.instructions.push(ArmInstruction::StrByteRegister {
                                src,
                                base_ptr,
                                offset,
                            });
                        }
                    }

                    2 => match offset {
                        Operand::Immediate(offset) => {
                            self.instructions.push(ArmInstruction::StrHalfImmediate {
                                mode: ArmUnsignedAddressingMode::UnsignedOffset,
                                src,
                                base_ptr,
                                offset: offset.as_i16(),
                            });
                        }

                        Operand::Register(offset) => {
                            let offset = self.allocate_register(offset);
                            self.instructions.push(ArmInstruction::StrHalfRegister {
                                src,
                                base_ptr,
                                offset,
                            });
                        }
                    }

                    4 => match offset {
                        Operand::Immediate(offset) => {
                            self.instructions.push(ArmInstruction::StrImmediate {
                                is_64_bit: false,
                                mode: ArmUnsignedAddressingMode::UnsignedOffset,
                                src,
                                base_ptr,
                                offset: offset.as_i16(),
                            });
                        }

                        Operand::Register(offset) => {
                            let offset = self.allocate_register(offset);
                            self.instructions.push(ArmInstruction::StrRegister {
                                is_64_bit: false,
                                src,
                                base_ptr,
                                offset,
                            });
                        }
                    }

                    8 => match offset {
                        Operand::Immediate(offset) => {
                            self.instructions.push(ArmInstruction::StrImmediate {
                                is_64_bit: true,
                                mode: ArmUnsignedAddressingMode::UnsignedOffset,
                                src,
                                base_ptr,
                                offset: offset.as_i16(),
                            });
                        }

                        Operand::Register(offset) => {
                            let offset = self.allocate_register(offset);
                            self.instructions.push(ArmInstruction::StrRegister {
                                is_64_bit: true,
                                src,
                                base_ptr,
                                offset,
                            });
                        }
                    }

                    _ => todo!("We ondersteunen alleen 4 en 8 byte SlaOp ARM-instructies, maar kreeg type: {typ:?}"),
                }
            }
        }
    }

    fn add_instruction_add(&mut self, typ: PrimitiveType, dst: VirtOrPhysReg<ArmRegister>, lhs: &Operand, rhs: &Operand) {
        match (lhs, rhs) {
            (Operand::Immediate(lhs), Operand::Immediate(rhs)) => {
                let imm16 = lhs.as_i64() + rhs.as_i64();
                self.add_instruction_mov(typ, dst, &Operand::Immediate(Immediate::Integer64(imm16)));
            }

            (Operand::Register(lhs), Operand::Register(rhs)) => {
                let lhs = self.allocate_register(lhs);
                let rhs = self.allocate_register(rhs);
                self.instructions.push(ArmInstruction::AddRegister {
                    is_64_bit: typ.is_arm_64_bit(),
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
                    is_64_bit: typ.is_arm_64_bit(),
                    dst,
                    src,
                    imm12,
                    shift: false,
                });
            }
        }
    }

    /// Arithmetic Shift Right
    fn add_instruction_asr(&mut self, typ: PrimitiveType, dst: VirtOrPhysReg<ArmRegister>, lhs: &Operand, rhs: &Operand) {
        match (lhs, rhs) {
            (Operand::Immediate(lhs), Operand::Immediate(rhs)) => {
                warn!("Immediate-SchuifRechts zou gedaan moeten worden door de optimalisator!");

                let imm16 = lhs.as_i64() >> rhs.as_i64();
                self.add_instruction_mov(typ, dst, &Operand::Immediate(Immediate::Integer64(imm16)));
            }

            (Operand::Register(lhs), Operand::Register(rhs)) => {
                let src = self.allocate_register(lhs);
                let amount = self.allocate_register(rhs);
                self.instructions.push(ArmInstruction::AsrRegister {
                    is_64_bit: typ.is_arm_64_bit(),
                    dst,
                    src,
                    amount,
                });
            }

            (Operand::Register(lhs), Operand::Immediate(rhs)) => {
                let src = self.allocate_register(lhs);

                self.instructions.push(ArmInstruction::AsrImmediate {
                    is_64_bit: typ.is_arm_64_bit(),
                    dst,
                    src,
                    amount: rhs.as_i8() as _,
                });
            }

            (Operand::Immediate(lhs), Operand::Register(rhs)) => {
                let amount = self.allocate_register(rhs);

                let src = if amount == dst {
                    self.create_scratch_register()
                } else {
                    dst
                };

                self.add_instruction_mov(typ, src, &Operand::Immediate(*lhs));

                self.instructions.push(ArmInstruction::AsrRegister {
                    is_64_bit: typ.is_arm_64_bit(),
                    dst,
                    src,
                    amount,
                });
            }
        }
    }

    fn add_instruction_cmp(&mut self, lhs: &Register, rhs: &Operand, typ: PrimitiveType) {
        match rhs {
            Operand::Immediate(immediate) => {
                let register = self.allocate_register(lhs);

                let immediate = immediate.as_i64();
                match immediate {
                    0..4095 => {
                        self.instructions.push(ArmInstruction::CmpImmediate {
                            is_64_bit: typ.is_arm_64_bit(),
                            register,
                            value: immediate as _,
                        });
                    }

                    -4095..0 => {
                        self.instructions.push(ArmInstruction::CmnImmediate {
                            is_64_bit: typ.is_arm_64_bit(),
                            register,
                            value: -immediate as _,
                        });
                    }

                    _ => {
                        let rhs = self.create_scratch_register();
                        self.add_instruction_mov(typ, rhs, &Operand::Immediate(Immediate::Integer64(immediate)));

                        self.instructions.push(ArmInstruction::CmpRegister {
                            is_64_bit: typ.is_arm_64_bit(),
                            lhs: register,
                            rhs,
                        });
                    }
                }
            }

            Operand::Register(rhs) => {
                let lhs = self.allocate_register(lhs);
                let rhs = self.allocate_register(rhs);
                self.instructions.push(ArmInstruction::CmpRegister { is_64_bit: typ.is_arm_64_bit(), lhs, rhs });
            }
        }
    }

    fn add_instruction_eor(&mut self, typ: PrimitiveType, dst: VirtOrPhysReg<ArmRegister>, lhs: &Operand, rhs: &Operand) {
        match (lhs, rhs) {
            (Operand::Immediate(lhs), Operand::Immediate(rhs)) => {
                warn!("Immediate-ExclusieveOf zou gedaan moeten worden door de optimalisator!");

                let imm16 = lhs.as_i64() ^ rhs.as_i64();
                self.add_instruction_mov(typ, dst, &Operand::Immediate(Immediate::Integer64(imm16)));
            }

            (Operand::Register(lhs), Operand::Register(rhs)) => {
                let lhs = self.allocate_register(lhs);
                let rhs = self.allocate_register(rhs);
                self.instructions.push(ArmInstruction::EorRegister {
                    is_64_bit: typ.is_arm_64_bit(),
                    dst,
                    lhs,
                    rhs,
                });
            }

            (Operand::Register(lhs), Operand::Immediate(rhs)) => {
                let reg = self.allocate_register(lhs);

                self.instructions.push(ArmInstruction::EorImmediate {
                    is_64_bit: typ.is_arm_64_bit(),
                    dst,
                    reg,
                    imm: rhs.as_i16() as _,
                });
            }

            (Operand::Immediate(lhs_imm), Operand::Register(rhs)) => {
                let rhs = self.allocate_register(rhs);

                let lhs = if rhs == dst {
                    self.create_scratch_register()
                } else {
                    dst
                };

                self.add_instruction_mov(typ, lhs, &Operand::Immediate(*lhs_imm));

                self.instructions.push(ArmInstruction::EorRegister {
                    is_64_bit: typ.is_arm_64_bit(),
                    dst,
                    lhs,
                    rhs,
                });
            }
        }
    }

    /// Logical Shift Left
    fn add_instruction_lsl(&mut self, typ: PrimitiveType, dst: VirtOrPhysReg<ArmRegister>, lhs: &Operand, rhs: &Operand) {
        match (lhs, rhs) {
            (Operand::Immediate(lhs), Operand::Immediate(rhs)) => {
                warn!("Immediate-SchuifLinks zou gedaan moeten worden door de optimalisator!");

                let imm16 = lhs.as_i64() << rhs.as_i64();
                self.add_instruction_mov(typ, dst, &Operand::Immediate(Immediate::Integer64(imm16)));
            }

            (Operand::Register(lhs), Operand::Register(rhs)) => {
                let src = self.allocate_register(lhs);
                let amount = self.allocate_register(rhs);
                self.instructions.push(ArmInstruction::LslRegister {
                    is_64_bit: typ.is_arm_64_bit(),
                    dst,
                    src,
                    amount,
                });
            }

            (Operand::Register(lhs), Operand::Immediate(rhs)) => {
                let src = self.allocate_register(lhs);

                self.instructions.push(ArmInstruction::LslImmediate {
                    is_64_bit: typ.is_arm_64_bit(),
                    dst,
                    src,
                    amount: rhs.as_i8() as _,
                });
            }

            (Operand::Immediate(lhs), Operand::Register(rhs)) => {
                let amount = self.allocate_register(rhs);

                let src = if amount == dst {
                    self.create_scratch_register()
                } else {
                    dst
                };

                self.add_instruction_mov(typ, src, &Operand::Immediate(*lhs));

                self.instructions.push(ArmInstruction::LslRegister {
                    is_64_bit: typ.is_arm_64_bit(),
                    dst,
                    src,
                    amount,
                });
            }
        }
    }

    fn add_instruction_mov(&mut self, typ: PrimitiveType, dst: VirtOrPhysReg<ArmRegister>, source: &Operand) {
        match source {
            Operand::Immediate(immediate) => {
                if immediate.as_i64() < 0 {
                    self.instructions.push(ArmInstruction::MovN {
                        is_64_bit: typ.is_arm_64_bit(),
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

    fn add_instruction_mul(&mut self, typ: PrimitiveType, dst: VirtOrPhysReg<ArmRegister>, lhs: &Operand, rhs: &Operand) {
        match (lhs, rhs) {
            (Operand::Immediate(lhs), Operand::Immediate(rhs)) => {
                let imm16 = lhs.as_i64() * rhs.as_i64();
                self.add_instruction_mov(typ, dst, &Operand::Immediate(Immediate::Integer64(imm16)));
            }

            (Operand::Register(lhs), Operand::Register(rhs)) => {
                let lhs = self.allocate_register(lhs);
                let rhs = self.allocate_register(rhs);
                self.instructions.push(ArmInstruction::Mul {
                    is_64_bit: typ.is_arm_64_bit(),
                    dst,
                    lhs,
                    rhs,
                });
            }

            (Operand::Register(lhs), Operand::Immediate(rhs_val)) => {
                let lhs = self.allocate_register(lhs);

                let rhs = if lhs == dst {
                    self.create_scratch_register()
                } else {
                    dst
                };

                self.add_instruction_mov(typ, rhs, &Operand::Immediate(*rhs_val));

                self.instructions.push(ArmInstruction::Mul {
                    is_64_bit: typ.is_arm_64_bit(),
                    dst,
                    lhs,
                    rhs,
                });
            }

            (Operand::Immediate(lhs_val), Operand::Register(rhs)) => {
                let rhs = self.allocate_register(rhs);

                let lhs = if rhs == dst {
                    self.create_scratch_register()
                } else {
                    dst
                };

                self.add_instruction_mov(typ, lhs, &Operand::Immediate(*lhs_val));

                self.instructions.push(ArmInstruction::Mul {
                    is_64_bit: typ.is_arm_64_bit(),
                    dst,
                    lhs,
                    rhs,
                });
            }
        }
    }

    fn add_instruction_sdiv(&mut self, typ: PrimitiveType, dst: VirtOrPhysReg<ArmRegister>, lhs: &Operand, rhs: &Operand) {
        match (lhs, rhs) {
            (Operand::Immediate(lhs), Operand::Immediate(rhs)) => {
                warn!("Immediate-DeelDoor zou gedaan moeten worden door de optimalisator!");

                let imm16 = lhs.as_i64() / rhs.as_i64();
                self.add_instruction_mov(typ, dst, &Operand::Immediate(Immediate::Integer64(imm16)));
            }

            (Operand::Register(lhs), Operand::Register(rhs)) => {
                let lhs = self.allocate_register(lhs);
                let rhs = self.allocate_register(rhs);
                self.instructions.push(ArmInstruction::SDiv {
                    is_64_bit: typ.is_arm_64_bit(),
                    dst,
                    lhs,
                    rhs,
                });
            }

            (Operand::Immediate(lhs), Operand::Register(rhs)) => {
                let rhs = self.allocate_register(rhs);

                let lhs_reg = if rhs == dst {
                    self.create_scratch_register()
                } else {
                    dst
                };

                self.add_instruction_mov(typ, lhs_reg, &Operand::Immediate(*lhs));

                self.instructions.push(ArmInstruction::SDiv {
                    is_64_bit: typ.is_arm_64_bit(),
                    dst,
                    lhs: lhs_reg,
                    rhs,
                });
            }

            (Operand::Register(lhs), Operand::Immediate(rhs)) => {
                let lhs = self.allocate_register(lhs);

                let rhs_reg = if lhs == dst {
                    self.create_scratch_register()
                } else {
                    dst
                };

                self.add_instruction_mov(typ, rhs_reg, &Operand::Immediate(*rhs));

                self.instructions.push(ArmInstruction::SDiv {
                    is_64_bit: typ.is_arm_64_bit(),
                    dst,
                    lhs,
                    rhs: rhs_reg,
                });
            }
        }
    }

    fn add_instruction_sub(&mut self, typ: PrimitiveType, dst: VirtOrPhysReg<ArmRegister>, lhs: &Operand, rhs: &Operand) {
        match (lhs, rhs) {
            (Operand::Immediate(lhs), Operand::Immediate(rhs)) => {
                let imm16 = lhs.as_i64() + rhs.as_i64();
                self.add_instruction_mov(typ, dst, &Operand::Immediate(Immediate::Integer64(imm16)));
            }

            (Operand::Register(lhs), Operand::Register(rhs)) => {
                let lhs = self.allocate_register(lhs);
                let rhs = self.allocate_register(rhs);
                self.instructions.push(ArmInstruction::SubRegister {
                    is_64_bit: typ.is_arm_64_bit(),
                    dst,
                    lhs,
                    rhs,
                    shift: 0,
                    shift_mode: ArmShift2::default(),
                });
            }

            (Operand::Immediate(lhs_imm), Operand::Register(rhs)) => {
                let rhs = self.allocate_register(rhs);

                let lhs = if rhs != dst {
                    rhs
                } else {
                    self.create_scratch_register()
                };

                self.add_instruction_mov(typ, lhs, &Operand::Immediate(*lhs_imm));

                self.instructions.push(ArmInstruction::SubRegister {
                    is_64_bit: typ.is_arm_64_bit(),
                    dst,
                    lhs,
                    rhs,
                    shift: 0,
                    shift_mode: ArmShift2::default(),
                });
            }

            (Operand::Register(lhs), Operand::Immediate(rhs)) => {
                let lhs = self.allocate_register(lhs);
                let rhs_imm12 = rhs.as_i64() as _;
                debug_assert!(rhs_imm12 < (1 << 12));
                self.instructions.push(ArmInstruction::SubImmediate {
                    is_64_bit: typ.is_arm_64_bit(),
                    dst,
                    lhs,
                    rhs_imm12,
                });
            }
        }
    }

    /// AArch64 doesn't have a MOD/REM instruction, so we have to use
    fn add_operation_modulo(&mut self, typ: PrimitiveType, dst: VirtOrPhysReg<ArmRegister>, lhs: &Operand, rhs: &Operand) {
        let (lhs, rhs) = match (lhs, rhs) {
            (Operand::Immediate(lhs), Operand::Immediate(rhs)) => {
                warn!("Immediate-modulo zou gedaan moeten worden door de optimalisator!");

                let val = lhs.as_i64() % rhs.as_i64();

                self.add_instruction_mov(typ, dst, &Operand::Immediate(Immediate::Integer64(val)));
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
                    self.create_scratch_register()
                } else {
                    dst
                };

                self.add_instruction_mov(typ, lhs_reg, &Operand::Immediate(*lhs));

                (lhs_reg, rhs)
            }

            (Operand::Register(lhs), Operand::Immediate(rhs)) => {
                let lhs = self.allocate_register(lhs);

                let rhs_reg = if lhs == dst {
                    self.create_scratch_register()
                } else {
                    dst
                };

                self.add_instruction_mov(typ, rhs_reg, &Operand::Immediate(*rhs));

                (lhs, rhs_reg)
            }
        };

        let tmp = if dst == lhs || dst == rhs {
            self.create_scratch_register()
        } else {
            dst
        };

        self.instructions.push(ArmInstruction::SDiv {
            is_64_bit: typ.is_arm_64_bit(),
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
    fn allocate_register(&mut self, register: &Register) -> VirtOrPhysReg<ArmRegister> {
        // match self.register_allocator.get_mapping(register, self.current_instruction_id) {
        //     Some(register) => register,
        //     None => {
        //         // TODO: allocate on the stack in this case.
        //         panic!("Er is geen ARM-register beschikbaar voor IR-register {register}!");
        //     }
        // }
        VirtOrPhysReg::Virtual(*register)
    }

    #[must_use]
    fn create_scratch_register(&mut self) -> VirtOrPhysReg<ArmRegister> {
        VirtOrPhysReg::Virtual(self.ir_reg_allocator.next())
    }

    fn dump_instructions(&self) {
        debug!("AArch64-instructies voor {}:", self.function_name);

        for instruction in &self.instructions {
            let spaces = if instruction.as_label().is_some() { "" } else { "    " };
            debug!("{spaces}{instruction}");
        }

        debug!("");
    }
}
