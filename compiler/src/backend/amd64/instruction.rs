// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::Display;

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
    MovReg32Reg32 { dst: Amd64Register, src: Amd64Register },
    MovReg64Reg64 { dst: Amd64Register, src: Amd64Register },

    ReturnNear,
}

impl Amd64Instruction {
    pub fn encode(&mut self, output: &mut Vec<u8>) {
        match self {
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