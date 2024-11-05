// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::HashMap, fmt::Display};

use crate::Label;

use super::{
    ArmBranchLocation,
    ArmConditionCode,
    ArmRegister, ArmSignedAddressingMode, ArmUnsignedAddressingMode,
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

    /// Branch with link
    Bl {
        offset: i32,
    },

    CmpImmediate {
        register: ArmRegister,
        value: u16,
    },

    CmpRegister {
        lhs: ArmRegister,
        rhs: ArmRegister,
    },

    /// Load pair
    Ldp {
        is_64_bit: bool,
        mode: ArmSignedAddressingMode,
        first: ArmRegister,
        second: ArmRegister,
        src: ArmRegister,
        offset: i16,
    },

    /// Load register
    LdrImmediate {
        is_64_bit: bool,
        mode: ArmUnsignedAddressingMode,
        dst: ArmRegister,
        base_ptr: ArmRegister,
        offset: i16,
    },

    #[allow(unused)]
    MovRegister32 { dst: ArmRegister, src: ArmRegister },

    MovRegister64 { dst: ArmRegister, src: ArmRegister },

    MovZ { register: ArmRegister, imm16: u16 },

    Ret,

    /// Store Pair of Registers calculates an address from a base register
    /// value and an immediate offset, and stores two 32-bit words or two
    /// 64-bit double words to the calculated address, from two registers.
    Stp {
        is_64_bit: bool,
        mode: ArmSignedAddressingMode,
        dst: ArmRegister,
        offset: i16,
        first: ArmRegister,
        second: ArmRegister,
    },

    StrImmediate {
        is_64_bit: bool,
        mode: ArmUnsignedAddressingMode,
        src: ArmRegister,
        base_ptr: ArmRegister,
        offset: i16,
    },

    SubImmediate {
        dst: ArmRegister,
        lhs: ArmRegister,
        rhs_imm12: u16,
    },

    SubRegister {
        is_64_bit: bool,
        dst: ArmRegister,
        lhs: ArmRegister,
        rhs: ArmRegister,
        shift: u8,
        shift_mode: ArmShift2,
    },
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

            Self::Bl { offset } => {
                debug_assert!(offset < (1 << 26));
                debug_assert!(offset > -(1 << 26));

                let mut instruction = 0x94000000;
                instruction |= (offset as u32) & 0x3FFFFFF;
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

            Self::Ldp { mode, is_64_bit, src, mut offset, first, second } => {
                let mut instruction = match mode {
                    ArmSignedAddressingMode::PostIndex    => 0x28C00000,
                    ArmSignedAddressingMode::PreIndex     => 0x29C00000,
                    ArmSignedAddressingMode::SignedOffset => 0x29400000,
                };

                if is_64_bit {
                    instruction |= 1 << 31;
                }

                instruction |= first.number as u32;
                instruction |= (src.number as u32) << 5;
                instruction |= (second.number as u32) << 10;

                if is_64_bit {
                    // For the 64-bit post-index and 64-bit pre-index variant: is the signed immediate byte offset, a
                    // multiple of 8 in the range -512 to 504, encoded in the "imm7" field as <imm>/8.

                    // For the 64-bit signed offset variant: is the optional signed immediate byte offset, a multiple of
                    // 8 in the range -512 to 504, defaulting to 0 and encoded in the "imm7" field as <imm>/8.
                    debug_assert!(offset >= -512);
                    debug_assert!(offset <= 504);
                    debug_assert!(offset % 8 == 0);

                    offset /= 8;
                } else {
                    // For the 32-bit post-index and 32-bit pre-index variant: is the signed immediate byte offset, a
                    // multiple of 4 in the range -256 to 252, encoded in the "imm7" field as <imm>/4.

                    // For the 32-bit signed offset variant: is the optional signed immediate byte offset, a multiple of
                    // 4 in the range -256 to 252, defaulting to 0 and encoded in the "imm7" field as <imm>/4.
                    debug_assert!(offset >= -256);
                    debug_assert!(offset <= 252);
                    debug_assert!(offset % 4 == 0);

                    offset /= 4;
                }

                instruction |= take_bits(offset as u32, 7) << 15;

                instruction
            }

            Self::LdrImmediate { is_64_bit, mode, dst, base_ptr, offset } => {
                let mut instruction = match mode {
                    ArmUnsignedAddressingMode::PostIndex      => 0xB8400400,
                    ArmUnsignedAddressingMode::PreIndex       => 0xB8400C00,
                    ArmUnsignedAddressingMode::UnsignedOffset => 0xB9400000,
                };

                if is_64_bit {
                    instruction |= 1 << 30;
                }

                instruction |= dst.number as u32;
                instruction |= (base_ptr.number as u32) << 5;

                match mode {
                    ArmUnsignedAddressingMode::PostIndex | ArmUnsignedAddressingMode::PreIndex => {
                        // Is the signed immediate byte offset, in the range -256 to 255, encoded in the "imm9" field.

                        debug_assert!(offset >= -256);
                        debug_assert!(offset <= 255);

                        instruction |= take_bits(offset as u32, 9) << 12;
                    }

                    ArmUnsignedAddressingMode::UnsignedOffset => {
                        // For the 32-bit variant: is the optional positive immediate byte offset, a multiple of 4 in
                        // the range 0 to 16380, defaulting to 0 and encoded in the "imm12" field as <pimm>/4.

                        // For the 64-bit variant: is the optional positive immediate byte offset, a multiple of 8 in
                        // the range 0 to 32760, defaulting to 0 and encoded in the "imm12" field as <pimm>/8.

                        debug_assert!(offset >= 0);

                        if is_64_bit {
                            debug_assert!(offset <= 32760);
                            instruction |= take_bits((offset / 8) as u32, 12) << 10;
                        } else {
                            debug_assert!(offset <= 16380);
                            instruction |= take_bits((offset / 4) as u32, 12) << 10;
                        }
                    }
                }

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

            Self::Stp { mode, is_64_bit, dst, mut offset, first, second } => {
                let mut instruction = match mode {
                    ArmSignedAddressingMode::PostIndex    => 0x28800000,
                    ArmSignedAddressingMode::PreIndex     => 0x29800000,
                    ArmSignedAddressingMode::SignedOffset => 0x29000000,
                };

                if is_64_bit {
                    instruction |= 1 << 31;
                }

                instruction |= first.number as u32;
                instruction |= (dst.number as u32) << 5;
                instruction |= (second.number as u32) << 10;

                if is_64_bit {
                    // For the 64-bit post-index and 64-bit pre-index variant: is the signed immediate byte offset, a
                    // multiple of 8 in the range -512 to 504, encoded in the "imm7" field as <imm>/8.

                    // For the 64-bit signed offset variant: is the optional signed immediate byte offset, a multiple of
                    // 8 in the range -512 to 504, defaulting to 0 and encoded in the "imm7" field as <imm>/8.
                    debug_assert!(offset >= -512);
                    debug_assert!(offset <= 504);
                    debug_assert!(offset % 8 == 0);

                    offset /= 8;
                } else {
                    // For the 32-bit post-index and 32-bit pre-index variant: is the signed immediate byte offset, a
                    // multiple of 4 in the range -256 to 252, encoded in the "imm7" field as <imm>/4.

                    // For the 32-bit signed offset variant: is the optional signed immediate byte offset, a multiple of
                    // 4 in the range -256 to 252, defaulting to 0 and encoded in the "imm7" field as <imm>/4.
                    debug_assert!(offset >= -256);
                    debug_assert!(offset <= 252);
                    debug_assert!(offset % 4 == 0);

                    offset /= 4;
                }

                instruction |= take_bits(offset as u32, 7) << 15;

                instruction
            }

            Self::StrImmediate { is_64_bit, mode, src, base_ptr, offset } => {
                let mut instruction = match mode {
                    ArmUnsignedAddressingMode::PostIndex      => 0xB8000400,
                    ArmUnsignedAddressingMode::PreIndex       => 0xB8000C00,
                    ArmUnsignedAddressingMode::UnsignedOffset => 0xB9000000,
                };

                if is_64_bit {
                    instruction |= 1 << 30;
                }

                instruction |= src.number as u32;
                instruction |= (base_ptr.number as u32) << 5;

                match mode {
                    ArmUnsignedAddressingMode::PostIndex | ArmUnsignedAddressingMode::PreIndex => {
                        // Is the signed immediate byte offset, in the range -256 to 255, encoded in the "imm9" field.

                        debug_assert!(offset >= -256);
                        debug_assert!(offset <= 255);

                        instruction |= take_bits(offset as u32, 9) << 12;
                    }

                    ArmUnsignedAddressingMode::UnsignedOffset => {
                        // For the 32-bit variant: is the optional positive immediate byte offset, a multiple of 4 in
                        // the range 0 to 16380, defaulting to 0 and encoded in the "imm12" field as <pimm>/4.

                        // For the 64-bit variant: is the optional positive immediate byte offset, a multiple of 8 in
                        // the range 0 to 32760, defaulting to 0 and encoded in the "imm12" field as <pimm>/8.

                        debug_assert!(offset >= 0);

                        if is_64_bit {
                            debug_assert!(offset <= 32760);
                            instruction |= take_bits((offset / 8) as u32, 12) << 10;
                        } else {
                            debug_assert!(offset <= 16380);
                            instruction |= take_bits((offset / 4) as u32, 12) << 10;
                        }
                    }
                }

                instruction
            }

            Self::SubImmediate { dst, lhs, rhs_imm12 } => {
                debug_assert!(rhs_imm12 < (1 << 12));

                let mut instruction = 0xD1000000;

                instruction |= dst.number as u32;
                instruction |= (lhs.number as u32) << 5;
                instruction |= (rhs_imm12 as u32) << 10;

                instruction
            }

            Self::SubRegister { is_64_bit, dst, lhs, rhs, shift, shift_mode } => {
                debug_assert!(shift < (1 << 6));

                let mut instruction = 0x4B000000;

                if is_64_bit {
                    instruction |= 1 << 30;
                }

                instruction |= dst.number as u32;
                instruction |= (lhs.number as u32) << 5;
                instruction |= (shift as u32) << 10;
                instruction |= (rhs.number as u32) << 16;

                instruction |= (shift_mode as u32) << 22;

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

            Self::Bl { offset } => {
                f.write_fmt(format_args!("b {offset}"))
            }

            Self::CmpImmediate { register, value } => {
                f.write_fmt(format_args!("cmp {register}, #{value}"))
            }

            Self::CmpRegister { lhs, rhs } => {
                f.write_fmt(format_args!("cmp {lhs}, {rhs}"))
            }

            Self::Ldp { is_64_bit, mode, first, second, src, offset } => {
                _ = is_64_bit;
                match mode {
                    ArmSignedAddressingMode::PostIndex => {
                        f.write_fmt(format_args!("ldp {first}, {second}, {src}, #0x{offset:x}"))
                    }

                    ArmSignedAddressingMode::PreIndex => {
                        f.write_fmt(format_args!("ldp {first}, {second}, [{src}, #0x{offset:x}]!"))
                    }

                    ArmSignedAddressingMode::SignedOffset => {
                        f.write_fmt(format_args!("ldp {first}, {second}, [{src}, #0x{offset:x}]"))
                    }
                }
            }

            Self::LdrImmediate { is_64_bit, mode, dst, base_ptr, offset } => {
                _ = is_64_bit;
                match mode {
                    ArmUnsignedAddressingMode::PostIndex => {
                        f.write_fmt(format_args!("ldr {dst}, {base_ptr}, #0x{offset:x}"))
                    }

                    ArmUnsignedAddressingMode::PreIndex => {
                        f.write_fmt(format_args!("ldr {dst}, [{base_ptr}, #0x{offset:x}]!"))
                    }

                    ArmUnsignedAddressingMode::UnsignedOffset => {
                        f.write_fmt(format_args!("ldr {dst}, [{base_ptr}, #0x{offset:x}]"))
                    }
                }
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
                f.write_fmt(format_args!("mov {register}, #{imm16}"))
            }

            Self::Ret => {
                f.write_str("ret")
            }

            Self::Stp { is_64_bit, dst, offset, first, second, mode } => {
                _ = is_64_bit;
                match mode {
                    ArmSignedAddressingMode::PostIndex => {
                        f.write_fmt(format_args!("stp {first}, {second}, {dst}, #0x{offset:x}"))
                    }

                    ArmSignedAddressingMode::PreIndex => {
                        f.write_fmt(format_args!("stp {first}, {second}, [{dst}, #0x{offset:x}]!"))
                    }

                    ArmSignedAddressingMode::SignedOffset => {
                        f.write_fmt(format_args!("stp {first}, {second}, [{dst}, #0x{offset:x}]"))
                    }
                }
            }

            Self::StrImmediate { is_64_bit, mode, src, base_ptr, offset } => {
                _ = is_64_bit;
                match mode {
                    ArmUnsignedAddressingMode::PostIndex => {
                        f.write_fmt(format_args!("str {src}, {base_ptr}, #0x{offset:x}"))
                    }

                    ArmUnsignedAddressingMode::PreIndex => {
                        f.write_fmt(format_args!("str {src}, [{base_ptr}, #0x{offset:x}]!"))
                    }

                    ArmUnsignedAddressingMode::UnsignedOffset => {
                        f.write_fmt(format_args!("str {src}, [{base_ptr}, #0x{offset:x}]"))
                    }
                }
            }

            Self::SubImmediate { dst, lhs, rhs_imm12 } => {
                f.write_fmt(format_args!("sub {dst}, {lhs}, #{rhs_imm12}"))
            }

            Self::SubRegister { is_64_bit, dst, lhs, rhs, shift, shift_mode } => {
                _ = is_64_bit;

                f.write_fmt(format_args!("sub {dst}, {lhs}, {rhs}"))?;

                if *shift != 0 {
                    f.write_fmt(format_args!(", {shift_mode:?}, #{shift}"))?;
                }

                Ok(())
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

#[cfg(test)]
mod tests {
    use rstest::rstest;
    use super::*;

    #[rstest]
    #[case(
        ArmInstruction::Ret,
        0xd65f03c0,
    )]
    #[case(
        ArmInstruction::SubImmediate {
            dst: ArmRegister::SP,
            lhs: ArmRegister::SP,
            rhs_imm12: 0x20,
        },
        0xd10083ff,
    )]
    #[case(
        ArmInstruction::Ldp {
            is_64_bit: true,
            mode: ArmSignedAddressingMode::SignedOffset,
            first: ArmRegister::FP,
            second: ArmRegister::LR,
            src: ArmRegister::SP,
            offset: 16,
        },
        0xa9417bfd,
    )]
    #[case(
        ArmInstruction::Stp {
            is_64_bit: true,
            mode: ArmSignedAddressingMode::SignedOffset,
            first: ArmRegister::FP,
            second: ArmRegister::LR,
            dst: ArmRegister::SP,
            offset: 16,
        },
        0xa9017bfd,
    )]
    #[case(
        ArmInstruction::Stp {
            is_64_bit: false,
            mode: ArmSignedAddressingMode::SignedOffset,
            first: ArmRegister::FP,
            second: ArmRegister::LR,
            dst: ArmRegister::SP,
            offset: 16,
        },
        0x29027bfd,
    )]
    #[case(
        ArmInstruction::LdrImmediate {
            is_64_bit: false,
            mode: ArmUnsignedAddressingMode::UnsignedOffset,
            dst: ArmRegister::X8,
            base_ptr: ArmRegister::SP,
            offset: 8,
        },
        0xb9400be8,
    )]
    #[case(
        ArmInstruction::StrImmediate {
            is_64_bit: false,
            mode: ArmUnsignedAddressingMode::UnsignedOffset,
            src: ArmRegister::X0,
            base_ptr: ArmRegister::SP,
            offset: 8,
        },
        0xb9000be0,
    )]
    fn encode_instruction(#[case] input: ArmInstruction, #[case] expected: u32) {
        let actual = input.encode(0, &HashMap::new());
        assert_eq!(expected, actual, "actual was: 0x{actual:x}");
    }
}
