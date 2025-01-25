// Copyright (C) 2024 - 2025 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::HashMap, fmt::Display, i32};

use babbelaar::BabString;

use crate::{backend::{abstract_register::AbstractRegister, VirtOrPhysReg}, DataSectionOffset, Label, TargetBranchInfo, TargetInstruction, TargetInstructionInfo};

use super::{address::Amd64Displacement, Amd64Address, Amd64ConditionCode, Amd64FixUp, Amd64Register};


/// Intel glossary:
///
/// | Symbol        | Description
/// |---------------|-------------------------
/// | r8            | 8-bit register
/// | r16           | 16-bit register
/// | r32           | 32-bit register
/// | r64           | 64-bit register
/// | imm8..imm64   | Immediate value of specific width
/// | REX.W         | Indicates the use of a REX prefix that affects operand size or instruction semantics. The ordering of the REX prefix and other optional/mandatory instruction prefixes are discussed Chapter 2. Note that REX prefixes that promote legacy instructions to 64-bit behavior are not listed explicitly in the opcode column.
/// | /r            | Indicates that the ModR/M byte of the instruction contains a register operand and an r/m operand.
#[derive(Debug, Clone, PartialEq)]
#[allow(unused)]
pub enum Amd64Instruction<Reg> {
    AddReg32Imm8 { dst: Reg, src: i8 },
    AddReg32Reg32 { dst: Reg, src: Reg },
    AddReg64Imm8 { dst: Reg, src: i8 },
    AddReg64Reg64 { dst: Reg, src: Reg },

    CallNearRelative { symbol_name: BabString },

    CmpReg32Imm8 { lhs: Reg, rhs: i8 },
    CmpReg32Imm32 { lhs: Reg, rhs: i32 },
    CmpReg32Reg32 { lhs: Reg, rhs: Reg },

    /// <https://www.felixcloutier.com/x86/cwd:cdq:cqo>
    Cqd,
    /// <https://www.felixcloutier.com/x86/cwd:cdq:cqo>
    Cqo,

    IDiv32 { rhs: Reg },
    IDiv64 { rhs: Reg },

    IMulReg32Imm8 { dst: Reg, lhs: Reg, rhs: i8 },
    IMulReg32Imm32 { dst: Reg, lhs: Reg, rhs: i32 },
    IMulReg32Reg32 { lhs: Reg, rhs: Reg },

    Inc32 { reg: Reg },
    Inc64 { reg: Reg },

    Jmp { location: Label },

    /// Jump when condition is met (short = 8-bit offset)
    JccShort { location: Label, condition: Amd64ConditionCode },

    LeaReg32FromReg32 {
        dst: Reg,
        base: Reg,
    },

    LeaReg32FromReg32Off8 {
        dst: Reg,
        base: Reg,
        offset: i8,
    },

    LeaReg64FromReg64 {
        dst: Reg,
        base: Reg,
    },

    LeaReg64FromReg64Off8 {
        dst: Reg,
        base: Reg,
        offset: i8,
    },

    LeaReg64RipDisp32 {
        dst: Reg,
        offset: i32,
        data_section_offset: DataSectionOffset,
    },

    MovImm8ToPtrReg64 { base: Reg, src: i8 },
    MovImm8ToPtrReg64Off8 { base: Reg, offset: i8, src: i8 },

    MovReg32FromPtr { dst: Reg, address: Amd64Address<Reg> },
    MovReg64FromPtr { dst: Reg, address: Amd64Address<Reg> },

    MovImm32ToPtrReg64 { base: Reg, src: i32 },
    MovImm32ToPtrReg64Off8 { base: Reg, offset: i8, src: i32 },

    MovReg8ToPtrReg64 { base: Reg, src: Reg },
    MovReg8ToPtrReg64Off8 { base: Reg, offset: i8, src: Reg },
    MovReg32ToPtrReg64 { base: Reg, src: Reg },
    MovReg32ToPtrReg64Off8 { base: Reg, offset: i8, src: Reg },
    MovReg64ToPtrReg64 { base: Reg, src: Reg },
    MovReg64ToPtrReg64Off8 { base: Reg, offset: i8, src: Reg },

    MovReg32Imm32 { dst: Reg, src: i32 },
    MovReg32Reg32 { dst: Reg, src: Reg },
    MovReg64Imm64 { dst: Reg, src: i64 },
    MovReg64Reg64 { dst: Reg, src: Reg },

    MovzxReg8FromPtr { dst: Reg, address: Amd64Address<Reg> },

    NegReg64 { dst: Reg },

    PopReg64 { reg: Reg },
    PushReg64 { reg: Reg },

    ReturnNear,

    SetCC { dst: Reg, condition: Amd64ConditionCode },

    SarReg32Cl { reg: Reg },
    SarReg64Cl { reg: Reg },
    ShlReg32Cl { reg: Reg },
    ShlReg64Cl { reg: Reg },
    ShrReg32Cl { reg: Reg },
    ShrReg64Cl { reg: Reg },

    SubReg32Imm8 { dst: Reg, src: i8 },
    SubReg64Imm8 { dst: Reg, src: i8 },
    SubReg32Reg32 { dst: Reg, src: Reg },

    XorReg32Reg32 { dst: Reg, src: Reg },
    XorReg64Reg64 { dst: Reg, src: Reg },

    //
    // Virtual Instructions
    //

    Label(Label),
    FixUp(Amd64FixUp),
}

impl<Reg> Amd64Instruction<Reg> {
    #[must_use]
    pub fn uses_label_offsets(&self) -> bool {
        match self {
            Self::Jmp { .. } => true,
            Self::JccShort { .. } => true,
            _ => false,
        }
    }
}

impl Amd64Instruction<Amd64Register> {
    pub fn encode(&self, output: &mut Vec<u8>, offset: usize, label_offsets: &HashMap<Label, usize>) {
        match self {
            Self::AddReg32Imm8 { dst, src } => {
                if dst.is_64_extended_register() {
                    output.push(register_extension(false, false, false, true));
                }
                output.push(0x83);
                output.push(mod_rm_byte_extra_op(0, *dst));
                output.push(*src as u8);
            }

            Self::AddReg32Reg32 { dst, src } => {
                if dst.is_64_extended_register() || src.is_64_extended_register() {
                    output.push(register_extension(false, src.is_64_extended_register(), false, dst.is_64_extended_register()));
                }
                output.push(0x01);
                output.push(mod_rm_byte_reg_reg(*dst, *src))
            }

            Self::AddReg64Imm8 { dst, src } => {
                output.push(register_extension(true, false, false, dst.is_64_extended_register()));
                output.push(0x83);
                output.push(mod_rm_byte_extra_op(0, *dst));
                output.push(*src as u8);
            }

            Self::AddReg64Reg64 { dst, src } => {
                output.push(register_extension(true, src.is_64_extended_register(), false, dst.is_64_extended_register()));
                output.push(0x01);
                output.push(mod_rm_byte_reg_reg(*dst, *src))
            }

            Self::CallNearRelative { symbol_name } => {
                _ = symbol_name;

                output.push(0xe8);
                output.extend_from_slice(&0u32.to_le_bytes());
            }

            Self::CmpReg32Imm8 { lhs, rhs } => {
                if lhs.is_64_extended_register() {
                    output.push(register_extension(false, false, false, true));
                }
                output.push(0x83);
                output.push(mod_rm_byte_extra_op(7, *lhs));
                output.push(*rhs as u8);
            }

            Self::CmpReg32Imm32 { lhs, rhs } => {
                if lhs.is_64_extended_register() {
                    output.push(register_extension(false, false, false, true));
                }
                output.push(0x81);
                output.push(mod_rm_byte_extra_op(7, *lhs));
                output.extend_from_slice(&rhs.to_le_bytes());
            }

            Self::CmpReg32Reg32 { lhs, rhs } => {
                if lhs.is_64_extended_register() || rhs.is_64_extended_register() {
                    output.push(register_extension(false, rhs.is_64_extended_register(), false, lhs.is_64_extended_register()));
                }
                output.push(0x39);
                output.push(mod_rm_byte_reg_reg(*lhs, *rhs));
            }

            Self::Cqd => {
                output.push(0x99);
            }

            Self::Cqo => {
                output.push(0x48);
                output.push(0x99);
            }

            Self::IDiv32 { rhs } => {
                if rhs.is_64_extended_register() {
                    output.push(register_extension(false, false, false, true));
                }
                output.push(0xF7);
                output.push(mod_rm_byte_extra_op(7, *rhs));
            }

            Self::IDiv64 { rhs } => {
                output.push(register_extension(true, false, false, rhs.is_64_extended_register()));
                output.push(0xF7);
                output.push(mod_rm_byte_extra_op(7, *rhs));
            }

            Self::IMulReg32Imm8 { dst, lhs, rhs } => {
                if dst.is_64_extended_register() || lhs.is_64_extended_register() {
                    output.push(register_extension(false, lhs.is_64_extended_register(), false, dst.is_64_extended_register()));
                }
                output.push(0x6b);
                output.push(mod_rm_byte_reg_reg(*lhs, *dst));
                output.extend_from_slice(&rhs.to_le_bytes());
            }

            Self::IMulReg32Imm32 { dst, lhs, rhs } => {
                if dst.is_64_extended_register() || lhs.is_64_extended_register() {
                    output.push(register_extension(false, lhs.is_64_extended_register(), false, dst.is_64_extended_register()));
                }
                output.push(0x69);
                output.push(mod_rm_byte_reg_reg(*lhs, *dst));
                output.extend_from_slice(&rhs.to_le_bytes());
            }

            Self::IMulReg32Reg32 { lhs, rhs } => {
                if lhs.is_64_extended_register() || rhs.is_64_extended_register() {
                    output.push(register_extension(false, lhs.is_64_extended_register(), false, rhs.is_64_extended_register()));
                }
                output.push(0x0F);
                output.push(0xAF);
                output.push(mod_rm_byte_reg_reg(*rhs, *lhs));
            }

            Self::Inc32 { reg } => {
                if reg.is_64_extended_register() {
                    output.push(register_extension(false, false, false, true));
                }
                output.push(0xff);
                output.push(mod_rm_byte_reg(*reg));
            }

            Self::Inc64 { reg } => {
                output.push(register_extension(true, false, false, reg.is_64_extended_register()));
                output.push(0xff);
                output.push(mod_rm_byte_reg(*reg));
            }

            Self::Jmp { location } => {
                let offset = {
                    let destination = *label_offsets.get(&location).unwrap() as isize;
                    let offset = offset as isize;
                    (destination - offset - 2) as i64
                };

                if let Ok(offset) = i8::try_from(offset) {
                    output.push(0xEB);
                    output.push(offset as u8);
                    return;
                }

                let offset = offset - 3;

                // Note: 16-bit jump is not supported on 64-bit mode

                if let Ok(offset) = i32::try_from(offset) {
                    output.push(0xE9);
                    output.extend(&offset.to_le_bytes());
                }

                // I don't expect functions to be larger than 2^31 bytes large.
                todo!("Very far jump, to be implemented, but is this realistically ever needed in a function?")
            }

            Self::JccShort { location, condition } => {
                let offset = {
                    let destination = *label_offsets.get(&location).unwrap() as isize;
                    let offset = offset as isize;
                    (destination - offset - 2) as i64
                };

                let Ok(offset) = i8::try_from(offset) else {
                    panic!("JccShort past niet, we willen 0x{offset:x}");
                };

                output.push(condition.jcc_short_code());
                output.push(offset as u8);
            }

            Self::Label(..) => (),

            Self::LeaReg32FromReg32 { dst, base } => {
                output.push(0x8d);
                output.push(mod_rm_no_displacement(*dst, *base));
            }

            Self::LeaReg32FromReg32Off8 { dst, base, offset } => {
                output.push(0x8d);
                output.push(mod_rm_8_bit_displacement(*dst, *base));
                output.push(*offset as u8);
            }

            Self::LeaReg64FromReg64 { dst, base } => {
                output.push(register_extension(true, false, false, false));
                output.push(0x8d);
                output.push(mod_rm_no_displacement(*dst, *base));
            }

            Self::LeaReg64FromReg64Off8 { dst, base, offset } => {
                output.push(register_extension(true, base.is_64_extended_register(), false, dst.is_64_extended_register()));
                output.push(0x8d);
                output.push(mod_rm_8_bit_displacement(*dst, *base));
                if *base == Amd64Register::Rsp {
                    output.push(sib_byte(SibScale::Scale1, None, Amd64Register::Rsp));
                }
                output.push(*offset as u8);
            }

            Self::LeaReg64RipDisp32 { dst, offset, .. } => {
                output.push(register_extension(true, dst.is_64_extended_register(), false, false));
                output.push(0x8d);
                output.push(mod_rip_relative_disp32(*dst));
                output.extend(&offset.to_le_bytes());
            }

            Self::MovReg32FromPtr { dst, address } => {
                output.push(register_extension(false, dst.is_64_extended_register(), address.index_is_64_extended_register(), address.base().is_64_extended_register()));
                output.push(0x8b);
                encode_addressing_suffix(output, dst, address);
            }

            Self::MovReg64FromPtr { dst, address } => {
                output.push(register_extension(true, dst.is_64_extended_register(), address.index_is_64_extended_register(), address.base().is_64_extended_register()));
                output.push(0x8b);
                encode_addressing_suffix(output, dst, address);
            }

            Self::MovImm8ToPtrReg64 { base, src } => {
                output.push(0xc7);
                output.push(base.mod_rm_bits());
                output.push(*src as u8);
            }

            Self::MovImm8ToPtrReg64Off8 { base, offset, src } => {
                output.push(0xc7);
                output.push(mod_rm_8_bit_displacement_single(*base));
                output.push(*offset as u8);
                output.push(*src as u8);
            }

            Self::MovImm32ToPtrReg64 { base, src } => {
                output.push(0xc7);
                output.push(base.mod_rm_bits());
                output.extend_from_slice(&src.to_le_bytes());
            }

            Self::MovImm32ToPtrReg64Off8 { base, offset, src } => {
                output.push(0xc7);
                output.push(mod_rm_8_bit_displacement_single(*base));
                output.push(*offset as u8);
                output.extend_from_slice(&src.to_le_bytes());
            }

            Self::MovReg8ToPtrReg64 { base, src } => {
                // always encoded..?
                output.push(register_extension(false, src.is_64_extended_register(), false, base.is_64_extended_register()));
                output.push(0x88);
                output.push(mod_rm_no_displacement(*src, *base));
            }

            Self::MovReg8ToPtrReg64Off8 { base, offset, src } => {
                output.push(register_extension(false, src.is_64_extended_register(), false, base.is_64_extended_register()));
                output.push(0x88);
                output.push(mod_rm_8_bit_displacement(*src, *base));
                output.push(*offset as u8);
            }

            Self::MovReg32ToPtrReg64 { base, src } => {
                output.push(0x89);
                output.push(mod_rm_no_displacement(*src, *base));
            }

            Self::MovReg32ToPtrReg64Off8 { base, offset, src } => {
                output.push(0x89);
                output.push(mod_rm_8_bit_displacement(*src, *base));
                output.push(*offset as u8);
            }

            Self::MovReg64ToPtrReg64 { base, src } => {
                output.push(register_extension(true, false, false, false));
                output.push(0x89);
                output.push(mod_rm_no_displacement(*src, *base));
            }

            Self::MovReg64ToPtrReg64Off8 { base, offset, src } => {
                output.push(register_extension(true, false, false, false));
                output.push(0x89);
                output.push(mod_rm_8_bit_displacement(*src, *base));
                output.push(*offset as u8);
            }

            Self::MovReg32Imm32 { dst, src } => {
                if dst.is_64_extended_register() {
                    output.push(register_extension(false, false, false, true));
                }
                output.push(0xb8 + dst.mod_rm_bits());
                output.extend_from_slice(&src.to_le_bytes());
            }

            Self::MovReg32Reg32 { dst, src } => {
                if dst.is_64_extended_register() || src.is_64_extended_register() {
                    output.push(register_extension(false, src.is_64_extended_register(), false, dst.is_64_extended_register()));
                }
                output.push(0x89);
                output.push(mod_rm_byte_reg_reg(*dst, *src));
            }

            Self::MovReg64Imm64 { dst, src } => {
                output.push(register_extension(true, false, false, dst.is_64_extended_register()));
                output.push(0xb8 + dst.mod_rm_bits());
                output.extend_from_slice(&src.to_le_bytes());
            }

            Self::MovReg64Reg64 { dst, src } => {
                output.push(register_extension(true, src.is_64_extended_register(), false, dst.is_64_extended_register()));
                output.push(0x89);
                output.push(mod_rm_byte_reg_reg(*dst, *src));
            }

            Self::MovzxReg8FromPtr { dst, address } => {
                output.push(register_extension(true, dst.is_64_extended_register(), address.index_is_64_extended_register(), address.base().is_64_extended_register()));
                output.push(0x0F);
                output.push(0xB6);
                encode_addressing_suffix(output, dst, address);
            }

            Self::NegReg64 { dst } => {
                output.push(register_extension(true, false, false, false));
                output.push(0xf7);
                output.push(mod_rm_byte_extra_op(3, *dst));
            }

            Self::PopReg64 { reg } => {
                if reg.is_64_extended_register() {
                    output.push(register_extension(false, false, false, true));
                }
                output.push(0x58 + reg.mod_rm_bits());
            }

            Self::PushReg64 { reg } => {
                if reg.is_64_extended_register() {
                    output.push(register_extension(false, false, false, true));
                }
                output.push(0x50 + reg.mod_rm_bits());
            }

            Self::ReturnNear => output.push(0xc3),

            Self::SetCC { dst, condition } => {
                if dst.is_64_extended_register() {
                    output.push(register_extension(false, false, false, true));
                } else if dst.has_8bit_lo_and_hi() {
                    output.push(register_extension(false, false, false, false));
                }

                output.push(0x0f);
                output.push(condition.setcc_op_code_part());
                output.push(mod_rm_byte_reg(*dst));
            }

            Self::SarReg32Cl { reg } => {
                if reg.is_64_extended_register() {
                    output.push(register_extension(false, false, false, true));
                }
                output.push(0xD3);
                output.push(mod_rm_byte_extra_op(7, *reg));
            }

            Self::SarReg64Cl { reg } => {
                output.push(register_extension(true, false, false, reg.is_64_extended_register()));
                output.push(0xD3);
                output.push(mod_rm_byte_extra_op(7, *reg));
            }

            Self::ShlReg32Cl { reg } => {
                if reg.is_64_extended_register() {
                    output.push(register_extension(false, false, false, true));
                }
                output.push(0xD3);
                output.push(mod_rm_byte_extra_op(4, *reg));
            }

            Self::ShlReg64Cl { reg } => {
                output.push(register_extension(true, false, false, reg.is_64_extended_register()));
                output.push(0xD3);
                output.push(mod_rm_byte_extra_op(4, *reg));
            }

            Self::ShrReg32Cl { reg } => {
                if reg.is_64_extended_register() {
                    output.push(register_extension(false, false, false, true));
                }
                output.push(0xD3);
                output.push(mod_rm_byte_extra_op(5, *reg));
            }

            Self::ShrReg64Cl { reg } => {
                output.push(register_extension(true, false, false, reg.is_64_extended_register()));
                output.push(0xD3);
                output.push(mod_rm_byte_extra_op(5, *reg));
            }

            Self::SubReg32Imm8 { dst, src } => {
                output.push(0x83);
                output.push(mod_rm_byte_extra_op(5, *dst));
                output.push(*src as u8);
            }

            Self::SubReg64Imm8 { dst, src } => {
                output.push(register_extension(true, false, false, false));
                output.push(0x83);
                output.push(mod_rm_byte_extra_op(5, *dst));
                output.push(*src as u8);
            }

            Self::SubReg32Reg32 { dst, src } => {
                output.push(0x29);
                output.push(mod_rm_byte_reg_reg(*dst, *src))
            }

            Self::XorReg32Reg32 { dst, src: rhs } => {
                output.push(0x31);
                output.push(mod_rm_byte_reg_reg(*dst, *rhs));
            }

            Self::XorReg64Reg64 { dst, src: rhs } => {
                output.push(register_extension(true, rhs.is_64_extended_register(), false, dst.is_64_extended_register()));
                output.push(0x31);
                output.push(mod_rm_byte_reg_reg(*dst, *rhs));
            }

            Self::FixUp(..) => panic!("FixUps zouden geresolveerd moeten zijn!"),
        }
    }
}

fn encode_addressing_suffix(output: &mut Vec<u8>, dst: &Amd64Register, address: &Amd64Address<Amd64Register>) {
    if let Some(index) = address.index() {
        output.push(mod_rm_byte(address.displacement().mod_rm_mode(), *dst, None));
        output.push(sib_byte(address.scale(), Some(index), address.base()));
    } else {
        output.push(mod_rm_byte(address.displacement().mod_rm_mode(), *dst, Some(address.base())));
    }

    match address.displacement() {
        Amd64Displacement::Disp8(displacement) => output.push(displacement as u8),
        Amd64Displacement::Disp32(displacement) => output.extend_from_slice(&displacement.to_le_bytes()),
        _ => (),
    }
}

impl<Reg: AbstractRegister> Display for Amd64Instruction<Reg> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::AddReg32Imm8 { dst, src } => {
                f.write_fmt(format_args!("add {}, 0x{src:x}", dst.name32()))
            }

            Self::AddReg32Reg32 { dst, src } => {
                f.write_fmt(format_args!("add {}, {}", dst.name32(), src.name32()))
            }

            Self::AddReg64Imm8 { dst, src } => {
                f.write_fmt(format_args!("add {}, 0x{src:x}", dst.name64()))
            }

            Self::AddReg64Reg64 { dst, src } => {
                f.write_fmt(format_args!("add {}, {}", dst.name64(), src.name64()))
            }

            Self::CallNearRelative { symbol_name } => {
                f.write_fmt(format_args!("call {symbol_name}"))
            }

            Self::CmpReg32Imm8 { lhs, rhs } => {
                f.write_fmt(format_args!("cmp {}, 0x{:x}", lhs.name32(), rhs))
            }

            Self::CmpReg32Imm32 { lhs, rhs } => {
                f.write_fmt(format_args!("cmp {}, 0x{:x}", lhs.name32(), rhs))
            }

            Self::CmpReg32Reg32 { lhs, rhs } => {
                f.write_fmt(format_args!("cmp {}, {}", lhs.name32(), rhs.name32()))
            }

            Self::Cqd => f.write_str("cqd"),
            Self::Cqo => f.write_str("cqo"),

            Self::IDiv32 { rhs } => {
                f.write_fmt(format_args!("idiv {}", rhs.name32()))
            }

            Self::IDiv64 { rhs } => {
                f.write_fmt(format_args!("idiv {}", rhs.name64()))
            }

            Self::IMulReg32Imm8 { dst, lhs, rhs } => {
                f.write_fmt(format_args!("imul {}, {}, 0x{rhs:x}", dst.name32(), lhs.name32()))
            }

            Self::IMulReg32Imm32 { dst, lhs, rhs } => {
                f.write_fmt(format_args!("imul {}, {}, 0x{rhs:x}", dst.name32(), lhs.name32()))
            }

            Self::IMulReg32Reg32 { lhs, rhs } => {
                f.write_fmt(format_args!("imul {}, {}", lhs.name32(), rhs.name32()))
            }

            Self::Inc32 { reg } => {
                f.write_fmt(format_args!("inc {}", reg.name32()))
            }

            Self::Inc64 { reg } => {
                f.write_fmt(format_args!("inc {}", reg.name64()))
            }

            Self::Jmp { location } => {
                f.write_fmt(format_args!("jmp {location}"))
            }

            Self::JccShort { location, condition } => {
                f.write_fmt(format_args!("j{condition} {location}"))
            }

            Self::Label(label) => {
                f.write_fmt(format_args!("{label}:"))
            }

            Self::LeaReg32FromReg32 { dst, base } => {
                f.write_fmt(format_args!("lea {}, [{}]", dst.name32(), base.name32()))
            }

            Self::LeaReg32FromReg32Off8 { dst, base, offset } => {
                f.write_fmt(format_args!("lea {}, [{} + 0x{offset:x}]", dst.name32(), base.name32()))
            }

            Self::LeaReg64FromReg64 { dst, base } => {
                f.write_fmt(format_args!("lea {}, [{}]", dst.name64(), base.name64()))
            }

            Self::LeaReg64FromReg64Off8 { dst, base, offset } => {
                f.write_fmt(format_args!("lea {}, [{} + 0x{offset:x}]", dst.name64(), base.name64()))
            }

            Self::LeaReg64RipDisp32 { dst, offset, data_section_offset } => {
                f.write_fmt(format_args!("lea {}, [rip + 0x{offset:x}]    # {data_section_offset}", dst.name64()))
            }

            Self::MovReg32FromPtr { dst, address } => {
                f.write_fmt(format_args!("mov {}, {address}", dst.name32()))
            }

            Self::MovReg64FromPtr { dst, address } => {
                f.write_fmt(format_args!("mov {}, {address}", dst.name64()))
            }

            Self::MovImm8ToPtrReg64 { base, src } => {
                f.write_fmt(format_args!("mov BYTE PTR [{}], 0x{src:x}", base.name64()))
            }

            Self::MovImm8ToPtrReg64Off8 { base, offset, src } => {
                f.write_fmt(format_args!("mov BYTE PTR [{} + 0x{offset:x}], 0x{src:x}", base.name64()))
            }

            Self::MovImm32ToPtrReg64 { base, src } => {
                f.write_fmt(format_args!("mov [{}], 0x{src:x}", base.name64()))
            }

            Self::MovImm32ToPtrReg64Off8 { base, offset, src } => {
                f.write_fmt(format_args!("mov [{} + 0x{offset:x}], 0x{src:x}", base.name64()))
            }

            Self::MovReg8ToPtrReg64 { base, src } => {
                f.write_fmt(format_args!("mov BYTE PTR [{}], {}", base.name64(), src.name8()))
            }

            Self::MovReg8ToPtrReg64Off8 { base, offset, src } => {
                f.write_fmt(format_args!("mov BYTE PTR [{} + 0x{offset:x}], {}", base.name64(), src.name8()))
            }

            Self::MovReg32ToPtrReg64 { base, src } => {
                f.write_fmt(format_args!("mov [{}], {}", base.name64(), src.name32()))
            }

            Self::MovReg32ToPtrReg64Off8 { base, offset, src } => {
                f.write_fmt(format_args!("mov [{} + 0x{offset:x}], {}", base.name64(), src.name32()))
            }

            Self::MovReg64ToPtrReg64 { base, src } => {
                f.write_fmt(format_args!("mov [{}], {}", base.name64(), src.name64()))
            }

            Self::MovReg64ToPtrReg64Off8 { base, offset, src } => {
                f.write_fmt(format_args!("mov [{} + 0x{offset:x}], {}", base.name64(), src.name64()))
            }

            Self::MovReg32Imm32 { dst, src } => {
                f.write_fmt(format_args!("mov {}, 0x{src:x}", dst.name32()))
            }

            Self::MovReg32Reg32 { dst, src } => {
                f.write_fmt(format_args!("mov {}, {}", dst.name32(), src.name32()))
            }

            Self::MovReg64Imm64 { dst, src } => {
                f.write_fmt(format_args!("mov {}, 0x{src:x}", dst.name64()))
            }

            Self::MovReg64Reg64 { dst, src } => {
                f.write_fmt(format_args!("mov {}, {}", dst.name64(), src.name64()))
            }

            Self::MovzxReg8FromPtr { dst, address } => {
                f.write_fmt(format_args!("movzx {}, BYTE PTR {address}", dst.name64()))
            }

            Self::NegReg64 { dst } => {
                f.write_fmt(format_args!("neg {}", dst.name64()))
            }

            Self::PopReg64 { reg } => {
                f.write_fmt(format_args!("pop {}", reg.name64()))
            }

            Self::PushReg64 { reg } => {
                f.write_fmt(format_args!("push {}", reg.name64()))
            }

            Self::ReturnNear => f.write_str("ret"),

            Self::SetCC { dst, condition } => {
                f.write_fmt(format_args!("set{condition} {}", dst.name8()))
            }

            Self::SarReg32Cl { reg } => {
                f.write_fmt(format_args!("sar {}, cl", reg.name32()))
            }

            Self::SarReg64Cl { reg } => {
                f.write_fmt(format_args!("sar {}, cl", reg.name64()))
            }

            Self::ShlReg32Cl { reg } => {
                f.write_fmt(format_args!("shl {}, cl", reg.name32()))
            }

            Self::ShlReg64Cl { reg } => {
                f.write_fmt(format_args!("shl {}, cl", reg.name64()))
            }

            Self::ShrReg32Cl { reg } => {
                f.write_fmt(format_args!("shr {}, cl", reg.name32()))
            }

            Self::ShrReg64Cl { reg } => {
                f.write_fmt(format_args!("shr {}, cl", reg.name64()))
            }

            Self::SubReg32Imm8 { dst, src } => {
                f.write_fmt(format_args!("sub {}, 0x{src:x}", dst.name32()))
            }

            Self::SubReg64Imm8 { dst, src } => {
                f.write_fmt(format_args!("sub {}, 0x{src:x}", dst.name64()))
            }

            Self::SubReg32Reg32 { dst, src } => {
                f.write_fmt(format_args!("sub {}, {}", dst.name32(), src.name32()))
            }

            Self::XorReg32Reg32 { dst, src: rhs } => {
                f.write_fmt(format_args!("xor {}, {}", dst.name32(), rhs.name32()))
            }

            Self::XorReg64Reg64 { dst, src: rhs } => {
                f.write_fmt(format_args!("xor {}, {}", dst.name64(), rhs.name64()))
            }

            Self::FixUp(fixup) => std::fmt::Debug::fmt(&fixup, f),
        }
    }
}

impl TargetInstruction for Amd64Instruction<VirtOrPhysReg<Amd64Register>> {
    type PhysReg = Amd64Register;

    fn info(&self) -> TargetInstructionInfo<Self::PhysReg> {
        let mut info = TargetInstructionInfo::new();

        // TODO: this should really be a macro...
        match self {
            Amd64Instruction::AddReg32Imm8 { dst, src } => {
                info.add_dst(dst);
                _ = src;
            }

            Amd64Instruction::AddReg32Reg32 { dst, src } => {
                info.add_dst(dst);
                info.add_src(src);
            }

            Amd64Instruction::AddReg64Imm8 { dst, src } => {
                info.add_dst(dst);
                _ = src;
            }

            Amd64Instruction::AddReg64Reg64 { dst, src } => {
                info.add_dst(dst);
                info.add_src(src);
            }

            Amd64Instruction::CallNearRelative { symbol_name } => {
                _ = symbol_name
            }

            Amd64Instruction::CmpReg32Imm8 { lhs, rhs } => {
                info.add_src(lhs);
                _ = rhs;
            }

            Amd64Instruction::CmpReg32Imm32 { lhs, rhs } => {
                info.add_src(lhs);
                _ = rhs;
            }

            Amd64Instruction::CmpReg32Reg32 { lhs, rhs } => {
                info.add_src(lhs);
                info.add_src(rhs);
            }

            Amd64Instruction::Cqd => {
                info.add_dst(Amd64Register::Rax);
                info.add_dst(Amd64Register::Rdx);
            }

            Amd64Instruction::Cqo => {
                info.add_dst(Amd64Register::Rax);
                info.add_dst(Amd64Register::Rdx);
            }

            Amd64Instruction::IDiv32 { rhs } => {
                info.add_dst(Amd64Register::Rax);
                info.add_dst(Amd64Register::Rdx);
                info.add_dst(rhs);
            }

            Amd64Instruction::IDiv64 { rhs } => {
                info.add_dst(Amd64Register::Rax);
                info.add_dst(Amd64Register::Rdx);
                info.add_dst(rhs);
            }

            Amd64Instruction::IMulReg32Imm8 { dst, lhs, rhs } => {
                info.add_dst(dst);
                info.add_src(lhs);
                _ = rhs;
            }

            Amd64Instruction::IMulReg32Imm32 { dst, lhs, rhs } => {
                info.add_dst(dst);
                info.add_src(lhs);
                _ = rhs;
            }

            Amd64Instruction::IMulReg32Reg32 { lhs, rhs } => {
                info.add_src(lhs);
                info.add_src(rhs);
            }

            Amd64Instruction::Inc32 { reg } => {
                info.add_dst(reg);
            }

            Amd64Instruction::Inc64 { reg } => {
                info.add_dst(reg);
            }

            Amd64Instruction::Jmp { location } => {
                _ = location;
            }

            Amd64Instruction::JccShort { location, condition } => {
                _ = location;
                _ = condition;
            }

            Amd64Instruction::Label(..) => (),

            Amd64Instruction::LeaReg32FromReg32 { dst, base } => {
                info.add_dst(dst);
                info.add_src(base);
            }

            Amd64Instruction::LeaReg32FromReg32Off8 { dst, base, offset } => {
                info.add_dst(dst);
                info.add_src(base);
                _ = offset;
            }

            Amd64Instruction::LeaReg64FromReg64 { dst, base } => {
                info.add_dst(dst);
                info.add_src(base);
            }

            Amd64Instruction::LeaReg64FromReg64Off8 { dst, base, offset } => {
                info.add_dst(dst);
                info.add_src(base);
                _ = offset;
            }

            Amd64Instruction::LeaReg64RipDisp32 { dst, offset, data_section_offset } => {
                info.add_dst(dst);
                // rip..?
                _ = offset;
                _ = data_section_offset;
            }

            Amd64Instruction::MovReg32FromPtr { dst, address } => {
                info.add_dst(dst);
                address.add_as_source_info(&mut info);
            }

            Amd64Instruction::MovReg64FromPtr { dst, address } => {
                info.add_dst(dst);
                address.add_as_source_info(&mut info);
            }

            Amd64Instruction::MovImm8ToPtrReg64 { base, src } => {
                info.add_src(base);
                _ = src;
            }

            Amd64Instruction::MovImm8ToPtrReg64Off8 { base, offset, src } => {
                info.add_src(base);
                _ = offset;
                _ = src;
            }

            Amd64Instruction::MovImm32ToPtrReg64 { base, src } => {
                info.add_src(base);
                _ = src;
            }

            Amd64Instruction::MovImm32ToPtrReg64Off8 { base, offset, src } => {
                info.add_src(base);
                _ = offset;
                _ = src;
            }

            Amd64Instruction::MovReg8ToPtrReg64 { base, src } => {
                info.add_src(base);
                info.add_src(src);
            }

            Amd64Instruction::MovReg8ToPtrReg64Off8 { base, offset, src } => {
                info.add_src(base);
                info.add_src(src);
                _ = offset;
            }

            Amd64Instruction::MovReg32ToPtrReg64 { base, src } => {
                info.add_src(base);
                info.add_src(src);
            }

            Amd64Instruction::MovReg32ToPtrReg64Off8 { base, offset, src } => {
                info.add_src(base);
                info.add_src(src);
                _ = offset;
            }

            Amd64Instruction::MovReg64ToPtrReg64 { base, src } => {
                info.add_src(base);
                info.add_src(src);
            }

            Amd64Instruction::MovReg64ToPtrReg64Off8 { base, offset, src } => {
                info.add_src(base);
                info.add_src(src);
                _ = offset;
            }

            Amd64Instruction::MovReg32Imm32 { dst, src } => {
                info.add_dst(dst);
                _ = src;
            }

            Amd64Instruction::MovReg32Reg32 { dst, src } => {
                info.add_dst(dst);
                info.add_src(src);
            }

            Amd64Instruction::MovReg64Imm64 { dst, src } => {
                info.add_dst(dst);
                _ = src;
            }

            Amd64Instruction::MovReg64Reg64 { dst, src } => {
                info.add_dst(dst);
                info.add_src(src);
            }

            Amd64Instruction::MovzxReg8FromPtr { dst, address } => {
                info.add_dst(dst);
                address.add_as_source_info(&mut info);
            }

            Amd64Instruction::NegReg64 { dst } => {
                info.add_dst(dst);
            }

            Amd64Instruction::PopReg64 { reg } => {
                info.add_dst(reg);
            }

            Amd64Instruction::PushReg64 { reg } => {
                info.add_src(reg);
            }

            Amd64Instruction::ReturnNear => {

            }

            Amd64Instruction::SetCC { dst, condition } => {
                info.add_dst(dst);
                _ = condition;
            }

            Amd64Instruction::SarReg32Cl { reg } => {
                info.add_dst(reg);
                info.add_src(Amd64Register::Rcx);
            }

            Amd64Instruction::SarReg64Cl { reg } => {
                info.add_dst(reg);
                info.add_src(Amd64Register::Rcx);
            }

            Amd64Instruction::ShlReg32Cl { reg } => {
                info.add_dst(reg);
                info.add_src(Amd64Register::Rcx);
            }

            Amd64Instruction::ShlReg64Cl { reg } => {
                info.add_dst(reg);
                info.add_src(Amd64Register::Rcx);
            }

            Amd64Instruction::ShrReg32Cl { reg } => {
                info.add_dst(reg);
                info.add_src(Amd64Register::Rcx);
            }

            Amd64Instruction::ShrReg64Cl { reg } => {
                info.add_dst(reg);
                info.add_src(Amd64Register::Rcx);
            }

            Amd64Instruction::SubReg32Imm8 { dst, src } => {
                info.add_dst(dst);
                _ = src;
            }

            Amd64Instruction::SubReg64Imm8 { dst, src } => {
                info.add_dst(dst);
                _ = src;
            }

            Amd64Instruction::SubReg32Reg32 { dst, src } => {
                info.add_dst(dst);
                info.add_src(src);
            }

            Amd64Instruction::XorReg32Reg32 { dst, src: rhs } => {
                info.add_dst(dst);
                info.add_src(rhs);
            }

            Amd64Instruction::XorReg64Reg64 { dst, src: rhs } => {
                info.add_dst(dst);
                info.add_src(rhs);
            }

            Amd64Instruction::FixUp(fixup) => {
                fixup.add_info(&mut info);
            }
        }

        info
    }

    fn branch_info(&self) -> Option<TargetBranchInfo> {
        match self {
            Self::Jmp { location } => Some(TargetBranchInfo::new_jump(*location, None)),
            Self::JccShort { location, condition } => Some(TargetBranchInfo::new_jump(*location, Some(condition.clone().into()))),
            Self::ReturnNear => Some(TargetBranchInfo::new_return(None)),
            _ => None,
        }
    }

    fn as_label(&self) -> Option<Label> {
        if let Self::Label(label) = self {
            Some(*label)
        } else {
            None
        }
    }

    fn as_rr_move(&self) -> Option<(VirtOrPhysReg<Self::PhysReg>, VirtOrPhysReg<Self::PhysReg>)> {
        match self {
            Self::MovReg32Reg32 { dst, src }
                | Self::MovReg64Reg64 { dst, src } => {
                Some((*dst, *src))
            }

            _ => None,
        }
    }

    fn is_call(&self) -> bool {
        matches!(self, Self::CallNearRelative { .. })
    }
}

/// Creates the REX prefix (Volume 2A chapter 2.2.1).
#[must_use]
const fn register_extension(w: bool, r: bool, x: bool, b: bool) -> u8 {
    // x is if we should use 64-bit extended registers
    0b0100_0000
        | ((w as u8) << 3)
        | ((r as u8) << 2)
        | ((x as u8) << 1)
        | ((b as u8) << 0)
}

#[must_use]
fn mod_rm_byte_reg(dst: Amd64Register) -> u8 {
    let mut byte = 0b11_000_000;
    byte |= dst.mod_rm_bits();

    byte
}

#[must_use]
fn mod_rm_byte_reg_reg(dst: Amd64Register, src: Amd64Register) -> u8 {
    let mut byte = 0b11_000_000;
    byte |= src.mod_rm_bits() << 3;
    byte |= dst.mod_rm_bits();

    byte
}

/// Extra op meaning the number after the slash (e.g. /7 is 7)
#[must_use]
fn mod_rm_byte_extra_op(rm: u8, reg: Amd64Register) -> u8 {
    let mut byte = 0b11_000_000;
    byte |= rm << 3;
    byte |= reg.mod_rm_bits();

    byte
}

#[must_use]
fn mod_rm_8_bit_displacement(dst: Amd64Register, src: Amd64Register) -> u8 {
    let mut byte = 0b01_000_000;

    byte |= dst.mod_rm_bits() << 3;
    byte |= src.mod_rm_bits();

    byte
}

#[must_use]
fn mod_rm_8_bit_displacement_single(reg: Amd64Register) -> u8 {
    let mut byte = 0b01_000_000;

    byte |= reg.mod_rm_bits();

    byte
}

#[must_use]
fn mod_rm_no_displacement(dst: Amd64Register, src: Amd64Register) -> u8 {
    let mut byte = 0b00_000_000;

    byte |= dst.mod_rm_bits() << 3;
    byte |= src.mod_rm_bits();

    byte
}

#[must_use]
fn mod_rip_relative_disp32(dst: Amd64Register) -> u8 {
    let mut byte = 0b00_000_101;

    byte |= dst.mod_rm_bits() << 3;

    byte
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[allow(unused)]
pub enum SibScale {
    #[default]
    Scale1 = 0b00,
    Scale2 = 0b01,
    Scale4 = 0b10,
    Scale8 = 0b11,
}

#[must_use]
fn mod_rm_byte(mode: Amd64ModRMMode, dst: Amd64Register, src: Option<Amd64Register>) -> u8 {
    let mut byte = (mode as u8) << 6;

    byte |= dst.mod_rm_bits() << 3;

    if let Some(src) = src {
        byte |= src.mod_rm_bits();
    } else {
        byte |= 0b100;
    }

    byte
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(unused)]
pub enum Amd64ModRMMode {
    NoDisp = 0b00,
    Disp8 = 0b01,
    Disp32 = 0b10,
    ExtraOp = 0b11,
}

impl Display for SibScale {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Scale1 => f.write_str("1"),
            Self::Scale2 => f.write_str("2"),
            Self::Scale4 => f.write_str("4"),
            Self::Scale8 => f.write_str("8"),
        }
    }
}

/// Certain encodings of the ModR/M byte require a second addressing byte (the SIB byte). The base-plus-index and
/// scale-plus-index forms of 32-bit addressing require the SIB byte. The SIB byte includes the following fields:
/// - The _scale_ field specifies the scale factor.
/// - The _index_ field specifies the register number of the index register.
/// - The _base_ field specifies the register number of the base register.
/// See Section 2.1.5 for the encodings of the ModR/M and SIB bytes.
#[must_use]
fn sib_byte(scale: SibScale, index: Option<Amd64Register>, base: Amd64Register) -> u8 {
    assert_ne!(index, Some(Amd64Register::Rsp));

    let scale = (scale as u8) << 6;
    let index = index.map(|x| x.sib_bits()).unwrap_or(0b100) << 3;
    let base = base.sib_bits();

    scale | index | base
}

#[cfg(test)]
mod tests {
    use crate::DataSectionKind;

    use super::*;
    use rstest::rstest;
    use pretty_assertions::assert_eq;

    #[rstest]
    #[case(
        Amd64Instruction::Inc32 { reg: Amd64Register::Rax },
        [
            0xff,
            0xc0,
        ].to_vec(),
    )]
    #[case(
        Amd64Instruction::Inc32 { reg: Amd64Register::Rdx },
        [
            0xff,
            0xc2,
        ].to_vec(),
    )]
    #[case(
        Amd64Instruction::CmpReg32Imm32 { lhs: Amd64Register::Rdx, rhs: 300 },
        [ 0x81, 0xfa, 0x2c, 0x01, 0x00, 0x00 ].to_vec(),
    )]
    #[case(
        Amd64Instruction::CmpReg32Imm8 { lhs: Amd64Register::Rax, rhs: 1 },
        [ 0x83, 0xf8, 0x01 ].to_vec(),
    )]
    #[case(
        Amd64Instruction::AddReg32Reg32 { dst: Amd64Register::Rax, src: Amd64Register::Rdx },
        [ 0x01, 0xd0 ].to_vec(),
    )]
    #[case(
        Amd64Instruction::AddReg32Imm8 { dst: Amd64Register::Rax, src: 10 },
        [ 0x83, 0xc0, 0x0a ].to_vec(),
    )]
    #[case(
        Amd64Instruction::SubReg32Imm8 { dst: Amd64Register::Rax, src: 10 },
        [ 0x83, 0xe8, 0x0a ].to_vec(),
    )]
    #[case(
        Amd64Instruction::SubReg32Reg32 { dst: Amd64Register::Rax, src: Amd64Register::Rsi },
        [ 0x29, 0xf0 ].to_vec(),
    )]
    #[case(
        Amd64Instruction::NegReg64 { dst: Amd64Register::Rsi },
        [ 0x48, 0xf7, 0xde ].to_vec(),
    )]
    #[case(
        Amd64Instruction::IMulReg32Reg32 {
            lhs: Amd64Register::Rax,
            rhs: Amd64Register::Rsi,
        },
        [ 0x0f, 0xaf, 0xc6 ].to_vec(),
    )]
    #[case(
        Amd64Instruction::IMulReg32Reg32 {
            lhs: Amd64Register::Rax,
            rhs: Amd64Register::Rdi,
        },
        [ 0x0f, 0xaf, 0xc7 ].to_vec(),
    )]
    #[case(
        Amd64Instruction::IMulReg32Imm8 {
            dst: Amd64Register::Rax,
            lhs: Amd64Register::Rdi,
            rhs: 0x34
        },
        [ 0x6b, 0xc7, 0x34 ].to_vec(),
    )]
    #[case(
        Amd64Instruction::IMulReg32Imm32 {
            dst: Amd64Register::Rax,
            lhs: Amd64Register::Rdi,
            rhs: 0x12345678
        },
        [ 0x69, 0xc7, 0x78, 0x56, 0x34, 0x12 ].to_vec(),
    )]
    #[case(
        Amd64Instruction::PushReg64 {
            reg: Amd64Register::Rsp,
        },
        [ 0x54 ].to_vec(),
    )]
    #[case(
        Amd64Instruction::PushReg64 {
            reg: Amd64Register::R12,
        },
        [ 0x41, 0x54 ].to_vec(),
    )]
    #[case(
        Amd64Instruction::MovReg32Reg32 {
            dst: Amd64Register::R12,
            src: Amd64Register::R13,
        },
        [ 0x45, 0x89, 0xEC ].to_vec(),
    )]
    #[case(
        Amd64Instruction::MovReg32Reg32 {
            dst: Amd64Register::R12,
            src: Amd64Register::Rax,
        },
        [ 0x41, 0x89, 0xC4  ].to_vec(),
    )]
    #[case(
        Amd64Instruction::MovReg64Reg64 {
            dst: Amd64Register::R12,
            src: Amd64Register::Rax,
        },
        [ 0x49, 0x89, 0xC4 ].to_vec(),
    )]
    #[case(
        Amd64Instruction::MovReg64Reg64 {
            dst: Amd64Register::Rax,
            src: Amd64Register::R12,
        },
        [ 0x4C, 0x89, 0xE0 ].to_vec(),
    )]
    #[case(
        Amd64Instruction::MovReg64Reg64 {
            dst: Amd64Register::R12,
            src: Amd64Register::R13,
        },
        [ 0x4D, 0x89, 0xEC ].to_vec(),
    )]
    #[case(
        Amd64Instruction::SetCC {
            dst: Amd64Register::Rcx,
            condition: Amd64ConditionCode::Equal,
        },
        [ 0x0F, 0x94, 0xC1 ].to_vec(),
    )]
    #[case(
        Amd64Instruction::XorReg64Reg64 {
            dst: Amd64Register::R12,
            src: Amd64Register::R13,
        },
        [ 0x4D, 0x31, 0xEC ].to_vec(),
    )]
    #[case(
        Amd64Instruction::MovReg32Imm32 {
            dst: Amd64Register::Rcx,
            src: 123,
        },
        [ 0xB9, 0x7b, 0x00, 0x00, 0x00 ].to_vec(),
    )]
    #[case(
        Amd64Instruction::MovReg32Imm32 {
            dst: Amd64Register::R15,
            src: 123,
        },
        [ 0x41, 0xbf, 0x7b, 0x00, 0x00, 0x00 ].to_vec(),
    )]
    #[case(
        Amd64Instruction::SarReg32Cl { reg: Amd64Register::Rax },
        vec![ 0xD3, 0xF8 ]
    )]
    #[case(
        Amd64Instruction::SarReg64Cl { reg: Amd64Register::Rax },
        vec![ 0x48, 0xD3, 0xF8 ]
    )]
    #[case(
        Amd64Instruction::SarReg64Cl { reg: Amd64Register::R12 },
        vec![ 0x49, 0xD3, 0xFC ]
    )]
    #[case(
        Amd64Instruction::ShlReg64Cl { reg: Amd64Register::Rcx },
        vec![ 0x48, 0xD3, 0xE1 ]
    )]
    #[case(
        Amd64Instruction::ShlReg64Cl { reg: Amd64Register::R15 },
        vec![ 0x49, 0xD3, 0xE7 ]
    )]
    #[case(
        Amd64Instruction::ShrReg64Cl { reg: Amd64Register::R9 },
        vec![ 0x49, 0xD3, 0xE9 ]
    )]
    #[case(
        Amd64Instruction::ShrReg32Cl { reg: Amd64Register::Rsp },
        vec![ 0xD3, 0xEC ]
    )]
    #[case(
        Amd64Instruction::ShrReg64Cl { reg: Amd64Register::Rbx },
        vec![ 0x48, 0xD3, 0xEB ]
    )]
    fn check_encoding(#[case] input: Amd64Instruction<Amd64Register>, #[case] expected: Vec<u8>) {
        let mut actual = Vec::new();
        input.encode(&mut actual, 0, &HashMap::new());

        assert_eq!(actual, expected, "{input} werd ge-encodeerd naar: {actual:#x?}, maar we verwachten: {expected:#x?}");
    }

    #[rstest]
    #[case(
        Amd64Instruction::LeaReg32FromReg32 { dst: Amd64Register::Rax, base: Amd64Register::Rbx },
        [ 0x8d, 0x03 ].to_vec(),
    )]
    #[case(
        Amd64Instruction::LeaReg32FromReg32Off8 { dst: Amd64Register::Rax, base: Amd64Register::Rbx, offset: 0x10 },
        [ 0x8d, 0x43, 0x10 ].to_vec(),
    )]
    #[case(
        Amd64Instruction::LeaReg64FromReg64 { dst: Amd64Register::Rax, base: Amd64Register::Rbx },
        [ 0x48, 0x8d, 0x03 ].to_vec(),
    )]
    #[case(
        Amd64Instruction::LeaReg64FromReg64Off8 { dst: Amd64Register::Rax, base: Amd64Register::Rbp, offset: -4 },
        [ 0x48, 0x8d, 0x45, 0xfc ].to_vec(),
    )]
    #[case(
        Amd64Instruction::LeaReg64FromReg64Off8 { dst: Amd64Register::Rsi, base: Amd64Register::Rsp, offset: 0x4 },
        [ 0x48, 0x8d, 0x74, 0x24, 0x04 ].to_vec(),
    )]
    #[case(
        Amd64Instruction::LeaReg64RipDisp32 { dst: Amd64Register::Rax, data_section_offset: DataSectionOffset::new(DataSectionKind::ReadOnly, 0), offset: 0 },
        [ 0x48, 0x8d, 0x05, 0x00, 0x00, 0x00, 0x00, ].to_vec(),
    )]
    #[case(
        Amd64Instruction::LeaReg64RipDisp32 { dst: Amd64Register::R12, data_section_offset: DataSectionOffset::new(DataSectionKind::ReadOnly, 0), offset: 0 },
        [ 0x4c, 0x8d, 0x25, 0x00, 0x00, 0x00, 0x00, ].to_vec(),
    )]
    fn check_encoding_lea(#[case] input: Amd64Instruction<Amd64Register>, #[case] expected: Vec<u8>) {
        let mut actual = Vec::new();
        input.encode(&mut actual, 0, &HashMap::new());

        assert_eq!(actual, expected, "actual wasn't the expected");
    }

    #[rstest]
    #[case(
        Amd64Instruction::MovReg32ToPtrReg64 { base: Amd64Register::Rcx, src: Amd64Register::Rax },
        [ 0x89, 0x01 ].to_vec(),
    )]
    #[case(
        Amd64Instruction::MovImm32ToPtrReg64 { base: Amd64Register::Rdi, src: 8 },
        [ 0xc7, 0x07, 0x08, 0x00, 0x00, 0x00 ].to_vec(),
    )]
    #[case(
        Amd64Instruction::MovImm32ToPtrReg64Off8 { base: Amd64Register::Rdi, offset: 4, src: 62 },
        [ 0xc7, 0x47, 0x04, 62, 0x00, 0x00, 0x00 ].to_vec(),
    )]
    #[case(
        Amd64Instruction::MovReg64ToPtrReg64 {
            base: Amd64Register::Rdi,
            src: Amd64Register::Rsi,
        },
        [ 0x48, 0x89, 0x37 ].to_vec(),
    )]
    #[case(
        Amd64Instruction::MovReg64ToPtrReg64Off8 {
            base: Amd64Register::Rdi,
            offset: 0x08,
            src: Amd64Register::Rsi,
        },
        [ 0x48, 0x89, 0x77, 0x08 ].to_vec(),
    )]
    fn check_encoding_mov_deref(#[case] input: Amd64Instruction<Amd64Register>, #[case] expected: Vec<u8>) {
        let mut actual = Vec::new();
        input.encode(&mut actual, 0, &HashMap::new());

        assert_eq!(actual, expected, "actual wasn't the expected! Instruction was: {input}");
    }

    #[rstest]
    #[case(
        SibScale::Scale1,
        None,
        Amd64Register::Rsp,
        0x24
    )]
    fn sib_encoding(#[case] scale: SibScale, #[case] index: Option<Amd64Register>, #[case] base: Amd64Register, #[case] expected: u8) {
        let actual = sib_byte(scale, index, base);
        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case(
        Amd64Instruction::MovReg32FromPtr {
            dst: Amd64Register::Rax,
            address: Amd64Address::new(Amd64Register::Rdx)
                .with_index(Amd64Register::R12, SibScale::Scale1),
        },
        vec![0x42, 0x8B, 0x04, 0x22]
    )]
    #[case(
        Amd64Instruction::MovReg64FromPtr {
            dst: Amd64Register::Rax,
            address: Amd64Address::new(Amd64Register::Rdx)
                .with_index(Amd64Register::R12, SibScale::Scale1),
        },
        vec![0x4A, 0x8B, 0x04, 0x22]
    )]
    #[case(
        Amd64Instruction::MovReg64FromPtr {
            dst: Amd64Register::Rdi,
            address: Amd64Address::new(Amd64Register::Rbx)
                .with_index(Amd64Register::Rcx, SibScale::Scale2),
        },
        vec![0x48, 0x8B, 0x3C, 0x4B]
    )]
    #[case(
        Amd64Instruction::MovReg64FromPtr {
            dst: Amd64Register::R9,
            address: Amd64Address::new(Amd64Register::R10)
                .with_index(Amd64Register::R11, SibScale::Scale4)
                .with_displacement(Amd64Displacement::Disp8(18)),
        },
        vec![0x4f, 0x8b, 0x4c, 0x9a, 0x12]
    )]
    #[case(
        Amd64Instruction::MovReg64FromPtr {
            dst: Amd64Register::R10,
            address: Amd64Address::new(Amd64Register::R11)
                .with_index(Amd64Register::R12, SibScale::Scale8)
                .with_displacement(Amd64Displacement::Disp32(0xCAFEBABEu32 as _)),
        },
        vec![0x4f, 0x8b, 0x94, 0xe3, 0xbe, 0xba, 0xfe, 0xca]
    )]
    #[case(
        Amd64Instruction::MovzxReg8FromPtr {
            dst: Amd64Register::R15,
            address: Amd64Address::new(Amd64Register::Rbx)
                .with_index(Amd64Register::R14, SibScale::Scale1)
        },
        vec![0x4e, 0x0f, 0xb6, 0x3c, 0x33]
    )]
    fn check_encoding_mov_scaled_index(#[case] input: Amd64Instruction<Amd64Register>, #[case] expected: Vec<u8>) {
        let mut actual = Vec::new();
        input.encode(&mut actual, 0, &HashMap::new());

        assert_eq!(actual, expected, "actual wasn't the expected! Instruction was: {input} and we encoded it as: {actual:x?}");
    }
}
