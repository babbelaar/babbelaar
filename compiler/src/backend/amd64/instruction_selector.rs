// Copyright (C) 2024 - 2025 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use babbelaar::BabString;
use log::{debug, warn};

use crate::{backend::VirtOrPhysReg, ir::RegisterAllocator as IrRegisterAllocator, AllocatableRegister, Function, Immediate, Instruction, MathOperation, Operand, Platform, Register, TargetInstruction};

use super::{Amd64ConditionCode, Amd64FixUp, Amd64Instruction, Amd64Register};

#[derive(Debug)]
pub struct Amd64InstructionSelector {
    pub(super) platform: Platform,
    pub(super) function_name: BabString,
    pub(super) instructions: Vec<Amd64Instruction<VirtOrPhysReg<Amd64Register>>>,
    pub(super) current_instruction_id: usize,
    pub(super) ir_reg_allocator: IrRegisterAllocator,
}

impl Amd64InstructionSelector {
    #[must_use]
    pub fn compile(function: &Function, platform: Platform) -> Self {
        let mut this = Self {
            function_name: function.name().clone(),
            instructions: Vec::new(),
            platform,
            current_instruction_id: 0,
            ir_reg_allocator: function.ir_register_allocator.clone(),
        };

        for (instruction_id, instruction) in function.instructions().iter().enumerate() {
            this.add_instruction(instruction, instruction_id);
        }

        this.dump_instructions();

        this
    }

    fn add_instruction(&mut self, instruction: &Instruction, instruction_id: usize) {
        self.current_instruction_id = instruction_id;

        match instruction {
            Instruction::Compare { lhs, rhs, typ } => {
                let lhs = self.allocate_register(lhs);

                assert!(typ.bytes() <= 4);

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

            Instruction::Increment { register, typ } => {
                assert!(typ.bytes() <= 4);

                let reg = self.allocate_register(register);
                self.instructions.push(Amd64Instruction::Inc32 { reg });
            }

            Instruction::Move { source, destination } => {
                let dst = self.allocate_register(destination);

                self.add_instruction_mov(dst, source);
            }

            Instruction::MoveCondition { destination, condition } => {
                let dst = self.allocate_register(destination);
                self.instructions.push(Amd64Instruction::MovReg32Imm32 { dst, src: 0 });
                self.instructions.push(Amd64Instruction::SetCC {
                    dst,
                    condition: Amd64ConditionCode::from(*condition),
                })
            }

            Instruction::Call { name, arguments, variable_arguments, ret_val_reg } => {
                assert!(arguments.len() < 5, "We ondersteunen een maximum van 4 argumenten op AMD64...");

                for (idx, arg) in arguments.iter().chain(variable_arguments.iter()).enumerate() {
                    let current_reg = self.allocate_register(&arg.register());
                    let actual_reg = VirtOrPhysReg::Physical(Amd64Register::argument_nth(&self.platform, idx));

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
                    let actual_ret_reg = VirtOrPhysReg::Physical(Amd64Register::return_register(&self.platform));
                    if ret_val_reg != actual_ret_reg {
                        self.instructions.push(Amd64Instruction::MovReg64Reg64 {
                            dst: ret_val_reg,
                            src: actual_ret_reg,
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
                self.instructions.push(Amd64Instruction::Label(*label));
            }

            Instruction::InitArg { destination, arg_idx } => {
                let src = VirtOrPhysReg::Physical(Amd64Register::argument_nth(&self.platform, *arg_idx));
                let dst = self.allocate_register(destination);
                if src != dst {
                    self.instructions.push(Amd64Instruction::MovReg64Reg64 {
                        dst,
                        src,
                    });
                }
            }

            Instruction::Return { value_reg } => {
                if let Some(value_reg) = value_reg {
                    let value_reg = self.allocate_register(value_reg);
                    let actual_ret_reg = VirtOrPhysReg::Physical(Amd64Register::return_register(&self.platform));
                    if value_reg != actual_ret_reg {
                        self.instructions.push(Amd64Instruction::MovReg64Reg64 {
                            dst: actual_ret_reg,
                            src: value_reg,
                        });
                    }
                }

                self.instructions.push(Amd64Instruction::ReturnNear);
            }

            Instruction::MathOperation { operation, typ: _, destination, lhs, rhs } => {
                let dst = self.allocate_register(destination);

                match operation {
                    MathOperation::Add => self.add_instruction_add(dst, lhs, rhs),
                    MathOperation::Multiply => self.instruction_mul(dst, lhs, rhs),
                    MathOperation::Subtract => self.add_instruction_sub(dst, lhs, rhs),
                    MathOperation::Divide => self.instruction_idiv(dst, lhs, rhs),
                    MathOperation::Modulo => self.add_instruction_mod(dst, lhs, rhs),
                    MathOperation::LeftShift => todo!("Voeg SchuifLinks toe"),
                    MathOperation::RightShift => todo!("Voeg SchuifRechts toe"),
                    MathOperation::Xor => self.add_instruction_xor(dst, lhs, rhs),
                }
            }

            Instruction::MoveAddress { destination, offset } => {
                let destination = self.allocate_register(destination);
                self.instructions.push(Amd64Instruction::FixUp(Amd64FixUp::MoveAddress {
                    destination,
                    offset: *offset,
                }));
            }

            Instruction::Negate { typ: _, dst, src } => {
                let dst = self.allocate_register(dst);
                let src = self.allocate_register(src);

                if dst != src {
                    self.instructions.push(Amd64Instruction::MovReg64Reg64 { dst, src });
                }

                self.instructions.push(Amd64Instruction::NegReg64 { dst });
            }

            Instruction::StackAlloc { dst, size } => {
                _ = size;

                let dst = self.allocate_register(dst);

                self.instructions.push(Amd64Instruction::FixUp(Amd64FixUp::StackAlloc {
                    dst,
                    instruction_id,
                }));
            }

            Instruction::LoadPtr { destination, base_ptr, offset, typ } => {
                let dst = self.allocate_register(destination);
                let base = self.allocate_register(base_ptr);

                match (offset.shrink_if_possible(), typ.bytes()) {
                    (Operand::Immediate(Immediate::Integer8(offset)), 1) => {
                        if offset == 0 {
                            self.instructions.push(Amd64Instruction::MovzxReg8FromPtrReg64 { dst, base });
                        } else {
                            self.instructions.push(Amd64Instruction::MovzxReg8FromPtrReg64Off8 { dst, base, offset });
                        }
                    }

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
                    (Operand::Immediate(value), Operand::Immediate(Immediate::Integer8(offset)), 1) => {
                        if offset == 0 {
                            self.instructions.push(Amd64Instruction::MovImm8ToPtrReg64 {
                                base,
                                src: value.as_i8(),
                            });
                        } else {
                            self.instructions.push(Amd64Instruction::MovImm8ToPtrReg64Off8 {
                                base,
                                offset,
                                src: value.as_i8(),
                            });
                        }
                    }

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

                    (Operand::Register(src), Operand::Immediate(Immediate::Integer8(offset)), 1) => {
                        let src = self.allocate_register(src);
                        if offset == 0 {
                            self.instructions.push(Amd64Instruction::MovReg8ToPtrReg64 { base, src });
                        } else {
                            self.instructions.push(Amd64Instruction::MovReg8ToPtrReg64Off8 { base, offset, src });
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

    // fn add_prologue(&mut self, instructions: &[Instruction]) {
    //     for reg in self.register_allocator.callee_saved_registers_to_save() {
    //         self.instructions.push(Amd64Instruction::PushReg64 { reg });
    //     }

    //     for (instruction_id, instruction) in instructions.iter().enumerate() {
    //         if let Instruction::StackAlloc { dst, size, .. } = instruction {
    //             self.stack_allocator.reserve_stack_allocation(instruction_id, *dst, *size);
    //         }
    //     }

    //     self.stack_allocator.finalize();

    //     if !self.characteristics.is_leaf_function() {
    //         self.instructions.push(Amd64Instruction::PushReg64 { reg: Amd64Register::Rbp });
    //         self.instructions.push(Amd64Instruction::MovReg64Reg64 { dst: VirtOrPhysReg<Amd64Register>::Rbp, src: VirtOrPhysReg::Physical(Amd64Register::Rsp) });
    //     }

    //     if self.stack_allocator.total_size() != 0 {
    //         self.instructions.push(Amd64Instruction::SubReg64Imm8 {
    //             dst: VirtOrPhysReg::Physical(Amd64Register::Rsp),
    //             src: self.stack_allocator.total_size().try_into().unwrap(),
    //         });
    //     }
    // }

    // fn add_epilogue(&mut self) {
    //     if self.stack_allocator.total_size() != 0 {
    //         self.instructions.push(Amd64Instruction::SubReg64Imm8 {
    //             dst: VirtOrPhysReg::Physical(Amd64Register::Rsp),
    //             src: self.stack_allocator.total_size().try_into().unwrap(),
    //         });
    //     }

    //     if !self.characteristics.is_leaf_function() {
    //         self.instructions.push(Amd64Instruction::MovReg64Reg64 { dst: VirtOrPhysReg::Physical(Amd64Register::Rsp), src: Amd64Register::Rbp });
    //         self.instructions.push(Amd64Instruction::PopReg64 { reg: Amd64Register::Rbp });
    //     }

    //     for reg in self.register_allocator.callee_saved_registers_to_save().rev() {
    //         self.instructions.push(Amd64Instruction::PopReg64 { reg });
    //     }
    // }

    #[must_use]
    fn allocate_register(&mut self, register: &Register) -> VirtOrPhysReg<Amd64Register> {
        // match self.register_allocator.get_mapping(register, self.current_instruction_id) {
        //     Some(register) => register,
        //     None => {
        //         // TODO: allocate on the stack in this case.
        //         panic!("Er is geen AMD64-register beschikbaar voor IR-register {register}!");
        //     }
        // }
        VirtOrPhysReg::Virtual(*register)
    }

    fn dump_instructions(&self) {
        debug!("AMD64-instructies voor {}:", self.function_name);

        for instruction in &self.instructions {
            let spaces = if instruction.as_label().is_some() { "" } else { "    " };
            debug!("{spaces}{instruction}");
        }

        debug!("");
    }

    fn add_instruction_add(&mut self, dst: VirtOrPhysReg<Amd64Register>, lhs: &Operand, rhs: &Operand) {
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

                if rhs == dst {
                    self.instructions.push(Amd64Instruction::AddReg32Reg32 { dst, src: lhs })
                } else if lhs == dst {
                    self.instructions.push(Amd64Instruction::AddReg32Reg32 { dst, src: rhs })
                } else {
                    self.instructions.push(Amd64Instruction::MovReg32Reg32 { dst, src: lhs });
                    self.instructions.push(Amd64Instruction::AddReg32Reg32 { dst, src: rhs })
                }
            }

            _ => todo!("Support add of {lhs}, {rhs}"),
        }
    }

    fn instruction_idiv(&mut self, dst: VirtOrPhysReg<Amd64Register>, lhs: &Operand, rhs: &Operand) {
        let rax = VirtOrPhysReg::Physical(Amd64Register::Rax);
        self.add_instruction_mov(rax, lhs);

        self.instructions.push(Amd64Instruction::Cqo);

        let reg = match rhs {
            Operand::Immediate(..) => {
                let reg = self.ir_reg_allocator.next();
                self.add_instruction_mov(VirtOrPhysReg::Virtual(reg), rhs);
                reg
            }

            Operand::Register(reg) => *reg,
        };

        self.instructions.push(Amd64Instruction::IDiv64 { rhs: VirtOrPhysReg::Virtual(reg) });

        if dst != rax {
            self.instructions.push(Amd64Instruction::MovReg64Reg64 { dst, src: rax });
        }
    }

    /// Uses the IDIV instruction, but moves RDX instead of RAX.
    fn add_instruction_mod(&mut self, dst: VirtOrPhysReg<Amd64Register>, lhs: &Operand, rhs: &Operand) {
        let rax = VirtOrPhysReg::Physical(Amd64Register::Rax);
        let rdx = VirtOrPhysReg::Physical(Amd64Register::Rdx);
        self.add_instruction_mov(rax, lhs);

        self.instructions.push(Amd64Instruction::Cqo);

        let reg = match rhs {
            Operand::Immediate(..) => {
                let reg = self.ir_reg_allocator.next();
                self.add_instruction_mov(VirtOrPhysReg::Virtual(reg), rhs);
                reg
            }

            Operand::Register(reg) => *reg,
        };

        self.instructions.push(Amd64Instruction::IDiv64 { rhs: VirtOrPhysReg::Virtual(reg) });

        if dst != rdx {
            self.instructions.push(Amd64Instruction::MovReg64Reg64 { dst, src: rdx });
        }
    }

    fn add_instruction_mov(&mut self, dst: VirtOrPhysReg<Amd64Register>, source: &Operand) {
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

    fn add_instruction_sub(&mut self, dst: VirtOrPhysReg<Amd64Register>, lhs: &Operand, rhs: &Operand) {
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

    fn instruction_mul(&mut self, dst: VirtOrPhysReg<Amd64Register>, lhs: &Operand, rhs: &Operand) {
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

    fn add_instruction_xor(&mut self, dst: VirtOrPhysReg<Amd64Register>, lhs: &Operand, rhs: &Operand) {
        match (lhs, rhs) {
            (Operand::Register(lhs), Operand::Immediate(rhs))
                | (Operand::Immediate(rhs), Operand::Register(lhs))=> {
                let lhs = self.allocate_register(lhs);

                if lhs != dst {
                    self.instructions.push(Amd64Instruction::MovReg32Reg32 { dst, src: lhs });
                }

                let rhs_reg = VirtOrPhysReg::Virtual(self.ir_reg_allocator.next());
                self.instructions.push(Amd64Instruction::MovReg32Imm32 {
                    dst: rhs_reg,
                    src: rhs.as_i32(),
                });

                self.instructions.push(Amd64Instruction::XorReg32Reg32 { dst, src: rhs_reg });
            }

            (Operand::Register(lhs), Operand::Register(rhs)) => {
                let lhs = self.allocate_register(lhs);
                let rhs = self.allocate_register(rhs);

                if rhs == dst {
                    self.instructions.push(Amd64Instruction::XorReg32Reg32 { dst, src: lhs })
                } else if lhs == dst {
                    self.instructions.push(Amd64Instruction::XorReg32Reg32 { dst, src: rhs })
                } else {
                    self.instructions.push(Amd64Instruction::XorReg32Reg32 { dst, src: lhs });
                    self.instructions.push(Amd64Instruction::XorReg32Reg32 { dst, src: rhs })
                }
            }

            _ => todo!("Support add of {lhs}, {rhs}"),
        }
    }
}
