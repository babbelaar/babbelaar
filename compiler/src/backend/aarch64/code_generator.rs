// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::collections::HashMap;

use crate::{backend::RegisterAllocator, CompiledFunction, Function, Instruction, Label, MathOperation, Operand, Register};

use super::{ArmBranchLocation, ArmConditionCode, ArmInstruction, ArmRegister, ArmShift2, ArmSignedAddressingMode, ArmUnsignedAddressingMode};

const SPACE_NEEDED_FOR_FP_AND_LR: usize = 2 * 8;

#[derive(Debug)]
pub struct AArch64CodeGenerator {
    instructions: Vec<ArmInstruction>,
    label_offsets: HashMap<Label, usize>,
    register_allocator: RegisterAllocator<ArmRegister>,
    stack_size: usize,
    space_used_on_stack: usize,
}

impl AArch64CodeGenerator {
    #[must_use]
    pub fn compile(function: &Function) -> CompiledFunction {
        let mut this = Self {
            instructions: Vec::new(),
            label_offsets: HashMap::new(),
            register_allocator: RegisterAllocator::new(function),
            stack_size: 0,
            space_used_on_stack: 0,
        };

        this.add_prologue(function.instructions());

        for instruction in function.instructions() {
            this.add_instruction(instruction);
        }

        this.dump_instructions();

        let byte_code = this.to_byte_code();
        print!("Bytecode:");
        for x in &byte_code {
            if *x < 0x10 {
                print!("0");
            }
            print!("{x:X} ");
        }
        println!();

        CompiledFunction {
            name: function.name.clone(),
            byte_code,
        }
    }

    pub fn add_instruction(&mut self, instruction: &Instruction) {
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

            Instruction::LoadImmediate { immediate, destination_reg } => {
                let arm_register = self.allocate_register(destination_reg);
                self.instructions.push(ArmInstruction::MovZ { register: arm_register, imm16: immediate.as_i64() as _ });
            }

            Instruction::Move { source, destination } => {
                let dst = self.allocate_register(destination);
                let src = self.allocate_register(source);

                self.instructions.push(ArmInstruction::MovRegister64 { dst, src });
            }

            Instruction::Call { name, arguments, ret_val_reg } => {
                _ = name;
                _ = arguments;
                _ = ret_val_reg;
                todo!()
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
                    MathOperation::Subtract => self.add_instruction_sub(dst, lhs, rhs),
                }
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

                let Operand::Register(src) = value else {
                    todo!("Ondersteun immediate SlaOp-instructie");
                };
                let src = self.allocate_register(src);

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
        println!("AArch64-instructies:");

        for (offset, instruction) in self.instructions.iter().enumerate() {
            for (label, label_offset) in &self.label_offsets {
                if *label_offset == offset {
                    println!("{label}:");
                }
            }

            println!("    {instruction}");
        }
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
        self.stack_size = SPACE_NEEDED_FOR_FP_AND_LR;

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

        self.instructions.push(ArmInstruction::SubImmediate {
            dst: ArmRegister::SP,
            lhs: ArmRegister::SP,
            rhs_imm12: self.stack_size as _,
        });

        self.instructions.push(ArmInstruction::Stp {
            is_64_bit: true,
            mode: ArmSignedAddressingMode::SignedOffset,
            dst: ArmRegister::SP,
            offset: (self.stack_size - SPACE_NEEDED_FOR_FP_AND_LR) as _,
            first: ArmRegister::FP,
            second: ArmRegister::LR,
        });
    }

    fn add_epilogue(&mut self) {
        self.instructions.push(ArmInstruction::Ldp {
            is_64_bit: true,
            mode: ArmSignedAddressingMode::SignedOffset,
            src: ArmRegister::SP,
            offset: (self.stack_size - SPACE_NEEDED_FOR_FP_AND_LR) as _,
            first: ArmRegister::FP,
            second: ArmRegister::LR,
        });

        self.instructions.push(ArmInstruction::AddImmediate {
            dst: ArmRegister::SP,
            src: ArmRegister::SP,
            imm12: self.stack_size as _,
            shift: false,
        });
    }
}
