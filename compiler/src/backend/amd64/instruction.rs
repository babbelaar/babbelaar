// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::HashMap, fmt::Display, i32};

use babbelaar::BabString;

use crate::Label;

use super::register::Amd64Register;


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
pub enum Amd64Instruction {
    AddReg32Imm8 { dst: Amd64Register, src: i8 },
    AddReg32Reg32 { dst: Amd64Register, src: Amd64Register },

    CallNearRelative { symbol_name: BabString },

    CmpReg32Imm8 { lhs: Amd64Register, rhs: i8 },
    CmpReg32Imm32 { lhs: Amd64Register, rhs: i32 },
    CmpReg32Reg32 { lhs: Amd64Register, rhs: Amd64Register },

    Inc32 { reg: Amd64Register },

    Jmp { location: Label },
    /// Jump if not equal (short = 8-bit offset)
    JneShort { location: Label },
    /// Jump if equal (short = 8-bit offset)
    JeShort { location: Label },

    MovReg32Imm32 { dst: Amd64Register, src: i32 },
    MovReg32Reg32 { dst: Amd64Register, src: Amd64Register },
    MovReg64Reg64 { dst: Amd64Register, src: Amd64Register },

    ReturnNear,
}

impl Amd64Instruction {
    #[must_use]
    pub fn uses_label_offsets(&self) -> bool {
        match self {
            Self::Jmp { .. } => true,
            Self::JeShort { .. } => true,
            Self::JneShort { .. } => true,
            _ => false,
        }
    }

    pub fn encode(&self, output: &mut Vec<u8>, offset: usize, label_offsets: &HashMap<Label, usize>) {
        match self {
            Self::AddReg32Imm8 { dst, src } => {
                output.push(0x83);
                output.push(mod_rm_byte_extra_op(0, *dst));
                output.push(*src as u8);
            }

            Self::AddReg32Reg32 { dst, src } => {
                output.push(0x01);
                output.push(mod_rm_byte_reg_reg(*dst, *src))
            }

            Self::CallNearRelative { symbol_name } => {
                _ = symbol_name;

                output.push(0xe8);
                output.extend_from_slice(&0u32.to_le_bytes());
            }

            Self::CmpReg32Imm8 { lhs, rhs } => {
                output.push(0x83);
                output.push(mod_rm_byte_extra_op(7, *lhs));
                output.push(*rhs as u8);
            }

            Self::CmpReg32Imm32 { lhs, rhs } => {
                output.push(0x81);
                output.push(mod_rm_byte_extra_op(7, *lhs));
                output.extend_from_slice(&rhs.to_le_bytes());
            }

            Self::CmpReg32Reg32 { lhs, rhs } => {
                output.push(0x39);
                output.push(mod_rm_byte_reg_reg(*lhs, *rhs));
            }

            Self::Inc32 { reg } => {
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

            Self::JeShort { location } => {
                let offset = {
                    let destination = *label_offsets.get(&location).unwrap() as isize;
                    let offset = offset as isize;
                    (destination - offset - 2) as i64
                };

                let Ok(offset) = i8::try_from(offset) else {
                    panic!("JeShort past niet, we willen 0x{offset:x}");
                };

                output.push(0x74);
                output.push(offset as u8);
            }

            Self::JneShort { location } => {
                let offset = {
                    let destination = *label_offsets.get(&location).unwrap() as isize;
                    let offset = offset as isize;
                    (destination - offset - 2) as i64
                };

                let Ok(offset) = i8::try_from(offset) else {
                    panic!("JneShort past niet, we willen 0x{offset:x}");
                };

                output.push(0x75);
                output.push(offset as u8);
            }

            Self::MovReg32Imm32 { dst, src } => {
                output.push(0xb8 + dst.mod_rm_bits());
                output.extend_from_slice(&src.to_le_bytes());
            }

            Self::MovReg32Reg32 { dst, src } => {
                output.push(0x89);
                output.push(mod_rm_byte_reg_reg(*dst, *src));
            }

            Self::MovReg64Reg64 { dst, src } => {
                output.push(register_extension(true, false, false, false));
                output.push(0x89);
                output.push(mod_rm_byte_reg_reg(*dst, *src));
            }

            Self::ReturnNear => output.push(0xc3),
        }
    }
}

impl Display for Amd64Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::AddReg32Imm8 { dst, src } => {
                f.write_fmt(format_args!("add {}, 0x{src:x}", dst.name32()))
            }

            Self::AddReg32Reg32 { dst, src } => {
                f.write_fmt(format_args!("add {}, {}", dst.name32(), src.name32()))
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
                f.write_str("cmp ")?;
                f.write_str(lhs.name32())?;
                f.write_str(", ")?;
                f.write_str(rhs.name32())
            }

            Self::Inc32 { reg } => {
                f.write_str("inc ")?;
                f.write_str(reg.name32())
            }

            Self::Jmp { location } => {
                f.write_fmt(format_args!("jmp {location}"))
            }

            Self::JeShort { location } => {
                f.write_fmt(format_args!("je {location}"))
            }

            Self::JneShort { location } => {
                f.write_fmt(format_args!("jne {location}"))
            }

            Self::MovReg32Imm32 { dst, src } => {
                f.write_fmt(format_args!("mov {dst}, 0x{src:x}"))
            }

            Self::MovReg32Reg32 { dst, src } => {
                f.write_fmt(format_args!("mov {}, {}", dst.name32(), src.name32()))
            }

            Self::MovReg64Reg64 { dst, src } => {
                f.write_fmt(format_args!("mov {}, {}", dst.name64(), src.name64()))
            }

            Self::ReturnNear => f.write_str("ret"),
        }
    }
}

/// Creates the REX prefix (Volume 2A chapter 2.2.1).
#[must_use]
const fn register_extension(reg_64: bool, r: bool, x: bool, b: bool) -> u8 {
    0b0100_0000
        | ((reg_64 as u8) << 3)
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

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

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
    fn check_encoding(#[case] input: Amd64Instruction, #[case] expected: Vec<u8>) {
        let mut actual = Vec::new();
        input.encode(&mut actual, 0, &HashMap::new());

        assert_eq!(actual, expected);
    }
}
