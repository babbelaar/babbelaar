// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::Display;

use crate::backend::AllocatableRegister;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ArmRegister {
    pub(super) number: u8,
}

#[allow(unused)]
impl ArmRegister {
    pub const X0: Self = Self { number: 0 };
    pub const X1: Self = Self { number: 1 };
    pub const X8: Self = Self { number: 8 };
    pub const X30: Self = Self { number: 30 };

    /// Frame Pointer
    pub const FP: Self = Self { number: 29 };

    /// Link Register
    pub const LR: Self = Self { number: 30 };

    /// Stack Pointer
    pub const SP: Self = Self { number: 31 };
}

impl AllocatableRegister for ArmRegister {
    fn return_register() -> Self {
        Self::X0
    }

    fn count() -> usize {
        31
    }

    fn argument_nth(n: usize) -> Self {
        debug_assert!(n < Self::count());

        Self {
            number: n as _,
        }
    }

    fn callee_saved_registers() -> &'static [ArmRegister] {
        const REGISTERS: &'static [ArmRegister] = &[
            ArmRegister { number: 19 },
            ArmRegister { number: 20 },
            ArmRegister { number: 21 },
            ArmRegister { number: 22 },
            ArmRegister { number: 23 },
            ArmRegister { number: 24 },
            ArmRegister { number: 25 },
            ArmRegister { number: 26 },
            ArmRegister { number: 27 },
            ArmRegister { number: 28 },
        ];
        REGISTERS
    }

    fn caller_saved_registers() -> &'static [ArmRegister] {
        const REGISTERS: &'static [ArmRegister] = &[
            ArmRegister { number: 0 },
            ArmRegister { number: 1 },
            ArmRegister { number: 2 },
            ArmRegister { number: 3 },
            ArmRegister { number: 4 },
            ArmRegister { number: 5 },
            ArmRegister { number: 6 },
            ArmRegister { number: 7 },
            ArmRegister { number: 8 },
            ArmRegister { number: 9 },
            ArmRegister { number: 10 },
            ArmRegister { number: 11 },
            ArmRegister { number: 12 },
            ArmRegister { number: 13 },
            ArmRegister { number: 14 },
            ArmRegister { number: 15 },
        ];
        REGISTERS
    }
}

impl Display for ArmRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::FP => f.write_str("fp"),
            Self::LR => f.write_str("lr"),
            Self::SP => f.write_str("sp"),

            _ => {
                f.write_str("x")?;
                self.number.fmt(f)
            }
        }
    }
}
