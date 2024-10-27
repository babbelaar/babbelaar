// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::HashMap, fmt::Display};

use crate::Label;

use super::{
    ArmBranchLocation,
    ArmConditionCode,
    ArmRegister,
};

#[derive(Debug, Clone, Copy)]
pub enum ArmInstruction {
    /// It can be in place if src == dst, but it doesn't have to be
    AddImmediate {
        dst: ArmRegister,
        src: ArmRegister,
        imm12: u16,
        shift: bool,
    },

    AddRegister {
        dst: ArmRegister,
        lhs: ArmRegister,
        rhs: ArmRegister,
        imm: u8, // 6 bits
        shift: ArmShift2,
    },

    B {
        location: ArmBranchLocation,
    },

    BCond {
        cond: ArmConditionCode,
        location: ArmBranchLocation,
    },

    CmpImmediate {
        register: ArmRegister,
        value: u16,
    },

    CmpRegister {
        lhs: ArmRegister,
        rhs: ArmRegister,
    },

    #[allow(unused)]
    MovRegister32 { dst: ArmRegister, src: ArmRegister },

    MovRegister64 { dst: ArmRegister, src: ArmRegister },

    MovZ { register: ArmRegister, imm16: u16 },

    Ret,
}

impl ArmInstruction {
    /// This functions encodes the tagged union values into a plain 32-bits integer.
    /// All instructions in ARM-land are 32-bits.
    #[must_use]
    pub fn encode(self, offset: usize, label_offsets: &HashMap<Label, usize>) -> u32 {
        match self {
            Self::AddImmediate { dst, src, imm12, shift } => {
                debug_assert!(imm12 < (1 << 12));

                let mut instruction = 0x91000000;
                instruction |= (shift as u32) << 22;
                instruction |= (imm12 as u32) << 10;
                instruction |= (src.number as u32) << 5;
                instruction |= dst.number as u32;

                instruction
            }

            Self::AddRegister { dst, lhs, rhs, shift, imm } => {
                debug_assert!(imm <= 0b11);

                let mut instruction = 0x8B000000;

                instruction |= dst.number as u32;
                instruction |= (lhs.number as u32) << 5;
                instruction |= (imm as u32) << 10;
                instruction |= (rhs.number as u32) << 16;
                instruction |= (shift as u32) << 22;


                instruction
            }

            Self::B { location } => {
                let pc_relative_offset = match location {
                    ArmBranchLocation::Label(label) => {
                        let destination = *label_offsets.get(&label).unwrap() as isize;
                        let offset = offset as isize;
                        (destination - offset) as i32
                    }

                    ArmBranchLocation::PcRelativeOffset(offset) => offset,
                };

                let mut instruction = 0x14000000;
                instruction |= pc_relative_offset as u32;
                instruction
            }

            Self::BCond { cond, location } => {
                let pc_relative_offset = match location {
                    ArmBranchLocation::Label(label) => {
                        let destination = *label_offsets.get(&label).unwrap() as isize;
                        let offset = offset as isize;
                        (destination - offset) as i32
                    }

                    ArmBranchLocation::PcRelativeOffset(offset) => offset,
                };

                let mut instruction = 0x54000000;

                instruction |= cond as u8 as u32;
                instruction |= take_bits(pc_relative_offset as u32, 19) << 5;

                instruction
            }

            Self::CmpImmediate { register, value } => {
                let mut instruction = 0xF100001F;
                // NOTE: sf is 1
                instruction |= (register.number as u32) << 5;
                instruction |= (value as u32) << 10;
                // NOTE: sh is 0
                instruction
            }

            Self::CmpRegister { lhs, rhs } => {
                let mut instruction = 0xEB00001F;
                instruction |= (lhs.number as u32) << 5;
                instruction |= (rhs.number as u32) << 16;
                // NOTE: sf is 1
                // NOTE: imm6 is 0
                // NOTE: shift is 0
                instruction
            }

            Self::MovRegister32 { dst, src } => {
                let mut instruction = 0x2A0003E0;
                instruction |= (src.number as u32) << 16;
                instruction |= (dst.number as u32) << 0;
                instruction
            }

            Self::MovRegister64 { dst, src } => {
                let mut instruction = 0xAA0003E0;
                instruction |= (src.number as u32) << 16;
                instruction |= (dst.number as u32) << 0;
                instruction
            }

            Self::MovZ { register, imm16 } => {
                let mut instruction = 0xD2800000;
                instruction |= register.number as u32;
                instruction |= (imm16 as u32) << 5;
                instruction
            }

            Self::Ret => {
                let rn = ArmRegister::X30;
                let mut instruction = 0xD65F0000;
                instruction |= (rn.number as u32) << 5;
                instruction
            }
        }
    }
}

#[must_use]
fn take_bits(i: u32, n: u32) -> u32  {
    i & ((1 << n) - 1)
}

impl Display for ArmInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::AddImmediate { dst, src, imm12, shift } => {
                f.write_fmt(format_args!("add {dst}, {src} + #{imm12}"))?;

                if *shift {
                    f.write_str(", shift 12")?;
                }

                Ok(())
            }

            Self::AddRegister { dst, lhs, rhs, imm, shift } => {
                f.write_fmt(format_args!("add {dst}, {lhs} + {rhs} + #{imm} ({shift:?})"))
            }

            Self::B { location } => {
                f.write_fmt(format_args!("b {location}"))
            }

            Self::BCond { cond, location } => {
                f.write_fmt(format_args!("b.{cond} {location}"))
            }

            Self::CmpImmediate { register, value } => {
                f.write_fmt(format_args!("cmp {register}, #{value}"))
            }

            Self::CmpRegister { lhs, rhs } => {
                f.write_fmt(format_args!("cmp {lhs}, {rhs}"))
            }

            Self::MovRegister32 { dst, src } => {
                let dst = dst.number;
                let src = src.number;
                f.write_fmt(format_args!("mov w{dst}, w{src}"))
            }

            Self::MovRegister64 { dst, src } => {
                f.write_fmt(format_args!("mov {dst}, {src}"))
            }

            Self::MovZ { register, imm16 } => {
                f.write_fmt(format_args!("movz {register}, #{imm16}"))
            }

            Self::Ret => {
                f.write_str("ret")
            }
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
#[allow(unused)]
pub enum ArmShift2 {
    #[default]
    LSL = 0b00,

    LSR = 0b01,

    ASR = 0b10,
}
