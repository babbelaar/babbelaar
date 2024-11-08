// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::HashMap, fmt::Display, i32};

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
    Jmp { location: Label },

    MovReg32Imm32 { dst: Amd64Register, src: i32 },
    MovReg32Reg32 { dst: Amd64Register, src: Amd64Register },
    MovReg64Reg64 { dst: Amd64Register, src: Amd64Register },

    ReturnNear,
}

impl Amd64Instruction {
    pub fn encode(&self, output: &mut Vec<u8>, offset: usize, label_offsets: &HashMap<Label, usize>) {
        match self {
            Self::Jmp { location } => {
                let offset = {
                    let destination = *label_offsets.get(&location).unwrap() as isize;
                    let offset = offset as isize;
                    (destination - offset) as i64
                };

                if let Ok(offset) = i8::try_from(offset) {
                    output.push(0xEB);
                    output.push(offset as u8);
                    return;
                }

                // Note: 16-bit jump is not supported on 64-bit mode

                if let Ok(offset) = i32::try_from(offset) {
                    output.push(0xEb);
                    output.extend(&offset.to_le_bytes());
                }

                // I don't expect functions to be larger than 2^31 bytes large.
                todo!("Very far jump, to be implemented, but is this realistically ever needed in a function?")
            }

            Self::MovReg32Imm32 { dst, src } => {
                assert_eq!(dst.mod_rm_bits(), 0); // TODO: encode this into the opcode
                output.push(0xb8);
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
            Self::Jmp { location } => {
                f.write_fmt(format_args!("jmp {location}"))
            }

            Self::MovReg32Imm32 { dst, src } => {
                f.write_fmt(format_args!("mov {dst}, 0x{src:x}"))
            }

            Self::MovReg32Reg32 { dst, src } => {
                f.write_fmt(format_args!("mov {}, {}", dst.name64(), src.name64()))
            }

            Self::MovReg64Reg64 { dst, src } => {
                f.write_fmt(format_args!("mov {}, {}", dst.name32(), src.name32()))
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
fn mod_rm_byte_reg_reg(dst: Amd64Register, src: Amd64Register) -> u8 {
    let mut byte = 0b11_000_000;
    byte |= src.mod_rm_bits() << 3;
    byte |= dst.mod_rm_bits();

    byte
}