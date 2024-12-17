// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::HashMap, mem::take};

use babbelaar::BabString;
use log::{debug, warn};

use crate::{backend::aarch64::AArch64VarArgsConvention, CodeGenerator, CompiledFunction, Environment, Function, Immediate, Instruction, Label, MathOperation, Operand, Platform, Register, RegisterAllocator, Relocation, RelocationMethod, RelocationType};

use super::{AArch64FunctionCharacteristics, AArch64StackAllocator, ArmBranchLocation, ArmConditionCode, ArmInstruction, ArmRegister, ArmShift2, ArmSignedAddressingMode, ArmUnsignedAddressingMode, POINTER_SIZE, SPACE_NEEDED_FOR_FP_AND_LR};

#[derive(Debug)]
pub struct AArch64CodeGenerator {
    function_name: BabString,
    characteristics: AArch64FunctionCharacteristics,
    instructions: Vec<ArmInstruction>,
    label_offsets: HashMap<Label, usize>,
    register_allocator: RegisterAllocator<ArmRegister>,
    stack_allocator: AArch64StackAllocator,
    relocations: Vec<Relocation>,

    var_args_convention: AArch64VarArgsConvention,
}

impl AArch64CodeGenerator {
    #[must_use]
    pub fn compile(function: &Function, platform: Platform) -> CompiledFunction {
        let characteristics = AArch64FunctionCharacteristics::analyze(function);

        let var_args_convention = match platform.environment() {
            Environment::Darwin => AArch64VarArgsConvention::StackOnly,
            _ => todo!("ondersteun niet-macOS AArch64")
        };

        let mut this = Self {
            function_name: function.name().clone(),
            characteristics,
            instructions: Vec::new(),
            label_offsets: HashMap::new(),
            register_allocator: RegisterAllocator::new(function),
            stack_allocator: AArch64StackAllocator::new(),
            relocations: Vec::new(),
            var_args_convention,
        };

        this.add_prologue(function.instructions());

        for (index, instruction) in function.instructions().iter().enumerate() {
            this.add_instruction(index, instruction);
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

    fn add_instruction(&mut self, instruction_id: usize, instruction: &Instruction) {
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

                self.add_instruction_mov(dst, source);
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

            Instruction::Call { name, arguments, variable_arguments, ret_val_reg } => {
                debug_assert!(arguments.len() < (1 << 8));

                let register_var_args = match self.var_args_convention {
                    AArch64VarArgsConvention::RegistersAndStack => {
                        variable_arguments.as_slice()
                    }

                    AArch64VarArgsConvention::StackOnly => {
                        if !variable_arguments.is_empty() {
                            let mut offset = self.stack_allocator.offset_of_variadic_args(instruction_id);

                            for arg in variable_arguments {
                                let src = self.allocate_register(&arg.register());
                                self.instructions.push(ArmInstruction::StrImmediate {
                                    is_64_bit: arg.size() == 8,
                                    src,
                                    base_ptr: ArmRegister::SP,
                                    offset: offset as _,
                                    mode: ArmUnsignedAddressingMode::UnsignedOffset,
                                });

                                offset += arg.size().next_multiple_of(8);
                            }
                        }

                        &[]
                    }
                };

                for (idx, arg) in arguments.iter().chain(register_var_args.iter()).enumerate() {
                    let reg = self.allocate_register(&arg.register());

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

                if let Some(ret_val_reg) = ret_val_reg {
                    let ret_val_reg = self.allocate_register(ret_val_reg);
                    if ret_val_reg != ArmRegister::X0 {
                        self.instructions.push(ArmInstruction::MovRegister64 {
                            dst: ret_val_reg,
                            src: ArmRegister::X0,
                        });
                    }
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

            Instruction::InitArg { destination, arg_idx } => {
                let reg = self.allocate_register(destination);
                if *arg_idx != reg.number as usize {
                    self.instructions.push(ArmInstruction::MovRegister64 {
                        dst: reg,
                        src: ArmRegister {
                            number: *arg_idx as _,
                        },
                    });
                }
            }

            Instruction::Return { value_reg } => {
                if let Some(value_reg) = value_reg {
                    let value_reg = self.allocate_register(value_reg);
                    if value_reg != ArmRegister::X0 {
                        self.instructions.push(ArmInstruction::MovRegister64 { dst: ArmRegister::X0, src: value_reg });
                    }
                }

                self.add_epilogue();
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
                    MathOperation::LeftShift => self.add_instruction_lsl(dst, lhs, rhs),
                    MathOperation::RightShift => self.add_instruction_asr(dst, lhs, rhs),
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

            Instruction::StackAlloc { dst, size: _ } => {
                let dst = self.allocate_register(dst);
                let offset = self.stack_allocator.offset_of_reg(instruction_id);

                self.instructions.push(ArmInstruction::AddImmediate {
                    dst,
                    src: ArmRegister::SP,
                    imm12: offset as _,
                    shift: false,
                });
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

    fn add_instruction_add(&mut self, dst: ArmRegister, lhs: &Operand, rhs: &Operand) {
        match (lhs, rhs) {
            (Operand::Immediate(lhs), Operand::Immediate(rhs)) => {
                let imm16 = lhs.as_i64() + rhs.as_i64();
                self.add_instruction_mov(dst, &Operand::Immediate(Immediate::Integer64(imm16)));
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

    /// Arithmetic Shift Right
    fn add_instruction_asr(&mut self, dst: ArmRegister, lhs: &Operand, rhs: &Operand) {
        match (lhs, rhs) {
            (Operand::Immediate(lhs), Operand::Immediate(rhs)) => {
                warn!("Immediate-SchuifRechts zou gedaan moeten worden door de optimalisator!");

                let imm16 = lhs.as_i64() >> rhs.as_i64();
                self.add_instruction_mov(dst, &Operand::Immediate(Immediate::Integer64(imm16)));
            }

            (Operand::Register(lhs), Operand::Register(rhs)) => {
                let src = self.allocate_register(lhs);
                let amount = self.allocate_register(rhs);
                self.instructions.push(ArmInstruction::AsrRegister {
                    is_64_bit: true,
                    dst,
                    src,
                    amount,
                });
            }

            (Operand::Register(lhs), Operand::Immediate(rhs)) => {
                let src = self.allocate_register(lhs);

                self.instructions.push(ArmInstruction::AsrImmediate {
                    is_64_bit: true,
                    dst,
                    src,
                    amount: rhs.as_i8() as _,
                });
            }

            (Operand::Immediate(lhs), Operand::Register(rhs)) => {
                let amount = self.allocate_register(rhs);

                let src = if amount == dst {
                    self.register_allocator.hacky_random_available_register().unwrap()
                } else {
                    dst
                };

                self.add_instruction_mov(src, &Operand::Immediate(*lhs));

                self.instructions.push(ArmInstruction::AsrRegister {
                    is_64_bit: true,
                    dst,
                    src,
                    amount,
                });
            }
        }
    }

    fn add_instruction_cmp(&mut self, lhs: &Register, rhs: &Operand) {
        match rhs {
            Operand::Immediate(immediate) => {
                let register = self.allocate_register(lhs);

                let immediate = immediate.as_i64();
                match immediate {
                    0..4095 => {
                        self.instructions.push(ArmInstruction::CmpImmediate {
                            register,
                            value: immediate as _,
                        });
                    }

                    -4095..0 => {
                        self.instructions.push(ArmInstruction::CmnImmediate {
                            register,
                            value: -immediate as _,
                        });
                    }

                    _ => {
                        let rhs = self.register_allocator.hacky_random_available_register().unwrap();
                        self.add_instruction_mov(rhs, &Operand::Immediate(Immediate::Integer64(immediate)));

                        self.instructions.push(ArmInstruction::CmpRegister {
                            lhs: register,
                            rhs,
                        });
                    }
                }
            }

            Operand::Register(rhs) => {
                let lhs = self.allocate_register(lhs);
                let rhs = self.allocate_register(rhs);
                self.instructions.push(ArmInstruction::CmpRegister { lhs, rhs });
            }
        }
    }

    /// Logical Shift Left
    fn add_instruction_lsl(&mut self, dst: ArmRegister, lhs: &Operand, rhs: &Operand) {
        match (lhs, rhs) {
            (Operand::Immediate(lhs), Operand::Immediate(rhs)) => {
                warn!("Immediate-SchuifLinks zou gedaan moeten worden door de optimalisator!");

                let imm16 = lhs.as_i64() << rhs.as_i64();
                self.add_instruction_mov(dst, &Operand::Immediate(Immediate::Integer64(imm16)));
            }

            (Operand::Register(lhs), Operand::Register(rhs)) => {
                let src = self.allocate_register(lhs);
                let amount = self.allocate_register(rhs);
                self.instructions.push(ArmInstruction::LslRegister {
                    is_64_bit: true,
                    dst,
                    src,
                    amount,
                });
            }

            (Operand::Register(lhs), Operand::Immediate(rhs)) => {
                let src = self.allocate_register(lhs);

                self.instructions.push(ArmInstruction::LslImmediate {
                    is_64_bit: true,
                    dst,
                    src,
                    amount: rhs.as_i8() as _,
                });
            }

            (Operand::Immediate(lhs), Operand::Register(rhs)) => {
                let amount = self.allocate_register(rhs);

                let src = if amount == dst {
                    self.register_allocator.hacky_random_available_register().unwrap()
                } else {
                    dst
                };

                self.add_instruction_mov(src, &Operand::Immediate(*lhs));

                self.instructions.push(ArmInstruction::LslRegister {
                    is_64_bit: true,
                    dst,
                    src,
                    amount,
                });
            }
        }
    }

    fn add_instruction_mov(&mut self, dst: ArmRegister, source: &Operand) {
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

    fn add_instruction_mul(&mut self, dst: ArmRegister, lhs: &Operand, rhs: &Operand) {
        match (lhs, rhs) {
            (Operand::Immediate(lhs), Operand::Immediate(rhs)) => {
                let imm16 = lhs.as_i64() * rhs.as_i64();
                self.add_instruction_mov(dst, &Operand::Immediate(Immediate::Integer64(imm16)));
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

            (Operand::Register(lhs), Operand::Immediate(rhs_val)) => {
                let lhs = self.allocate_register(lhs);

                let rhs = if lhs == dst {
                    self.register_allocator.hacky_random_available_register().unwrap()
                } else {
                    dst
                };

                self.add_instruction_mov(rhs, &Operand::Immediate(*rhs_val));

                self.instructions.push(ArmInstruction::Mul {
                    is_64_bit: true,
                    dst,
                    lhs,
                    rhs,
                });
            }

            (Operand::Immediate(lhs_val), Operand::Register(rhs)) => {
                let rhs = self.allocate_register(rhs);

                let lhs = if rhs == dst {
                    self.register_allocator.hacky_random_available_register().unwrap()
                } else {
                    dst
                };

                self.add_instruction_mov(lhs, &Operand::Immediate(*lhs_val));

                self.instructions.push(ArmInstruction::Mul {
                    is_64_bit: true,
                    dst,
                    lhs,
                    rhs,
                });
            }
        }
    }

    fn add_instruction_sdiv(&mut self, dst: ArmRegister, lhs: &Operand, rhs: &Operand) {
        match (lhs, rhs) {
            (Operand::Immediate(lhs), Operand::Immediate(rhs)) => {
                warn!("Immediate-DeelDoor zou gedaan moeten worden door de optimalisator!");

                let imm16 = lhs.as_i64() / rhs.as_i64();
                self.add_instruction_mov(dst, &Operand::Immediate(Immediate::Integer64(imm16)));
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

                self.add_instruction_mov(lhs_reg, &Operand::Immediate(*lhs));

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

                self.add_instruction_mov(rhs_reg, &Operand::Immediate(*rhs));

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
                self.add_instruction_mov(dst, &Operand::Immediate(Immediate::Integer64(imm16)));
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

                self.add_instruction_mov(dst, &Operand::Immediate(Immediate::Integer64(val)));
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

                self.add_instruction_mov(lhs_reg, &Operand::Immediate(*lhs));

                (lhs_reg, rhs)
            }

            (Operand::Register(lhs), Operand::Immediate(rhs)) => {
                let lhs = self.allocate_register(lhs);

                let rhs_reg = if lhs == dst {
                    self.register_allocator.hacky_random_available_register().unwrap()
                } else {
                    dst
                };

                self.add_instruction_mov(rhs_reg, &Operand::Immediate(*rhs));

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
        if !self.characteristics.is_leaf_function() {
            self.stack_allocator.reserve_save_frame_pointer(SPACE_NEEDED_FOR_FP_AND_LR);
        }

        self.stack_allocator.reserve_callee_saved_registers(self.register_allocator.callee_saved_registers_to_save().len());

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

        self.instructions.push(ArmInstruction::SubImmediate {
            dst: ArmRegister::SP,
            lhs: ArmRegister::SP,
            rhs_imm12: self.stack_allocator.total_size() as _,
        });

        if !self.characteristics.is_leaf_function() {
            self.instructions.push(ArmInstruction::Stp {
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
        self.instructions.push(ArmInstruction::Stp {
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

        // TODO: use `array_chunks()` when it is stabilized
        let pairs = self.register_allocator.callee_saved_registers_to_save().chunks(2);
        for register_pair in pairs {
            if register_pair.len() == 1 {
                self.instructions.push(ArmInstruction::StrImmediate {
                    is_64_bit: true,
                    mode: ArmUnsignedAddressingMode::UnsignedOffset,
                    src: register_pair[0],
                    base_ptr: ArmRegister::SP,
                    offset: offset as _,
                });
            } else {
                self.instructions.push(ArmInstruction::Stp {
                    is_64_bit: true,
                    mode: ArmSignedAddressingMode::SignedOffset,
                    dst: ArmRegister::SP,
                    first: register_pair[0],
                    second: register_pair[1],
                    offset: offset as _,
                })
            }

            offset += POINTER_SIZE * 2;
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
    }

    fn add_epilogue_instructions_general(&mut self) {
        if !self.characteristics.is_leaf_function() {
            self.instructions.push(ArmInstruction::Ldp {
                is_64_bit: true,
                mode: ArmSignedAddressingMode::SignedOffset,
                src: ArmRegister::SP,
                offset: self.stack_allocator.offset_of_frame_pointer() as _,
                first: ArmRegister::FP,
                second: ArmRegister::LR,
            });
        }

        self.instructions.push(ArmInstruction::AddImmediate {
            dst: ArmRegister::SP,
            src: ArmRegister::SP,
            imm12: self.stack_allocator.total_size() as _,
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
        if self.stack_allocator.total_size() == 0 {
            return;
        }

        let mut offset = self.stack_allocator.offset_of_callee_register_saves();

        // TODO: use `array_chunks()` when it is stabilized
        let pairs = self.register_allocator.callee_saved_registers_to_save().chunks(2);
        for register_pair in pairs {
            if register_pair.len() == 1 {
                self.instructions.push(ArmInstruction::LdrImmediate {
                    is_64_bit: true,
                    mode: ArmUnsignedAddressingMode::UnsignedOffset,
                    dst: register_pair[0],
                    base_ptr: ArmRegister::SP,
                    offset: offset as _,
                });
            } else {
                self.instructions.push(ArmInstruction::Ldp {
                    is_64_bit: true,
                    mode: ArmSignedAddressingMode::SignedOffset,
                    src: ArmRegister::SP,
                    first: register_pair[0],
                    second: register_pair[1],
                    offset: offset as _,
                })
            }

            offset += POINTER_SIZE * 2;
        }
    }
}

impl CodeGenerator for AArch64CodeGenerator {
    fn compile(function: &Function) -> CompiledFunction {
        Self::compile(function, Platform::host_platform())
    }
}
