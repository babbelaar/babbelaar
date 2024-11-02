// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::Display;

use crate::backend::AllocatableRegister;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ArmRegister {
    pub(super) number: u8,
}

#[allow(unused)]
impl ArmRegister {
    pub const X0: Self = Self { number: 0 };
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
        12
    }

    fn nth(n: usize) -> Self {
        debug_assert!(n < Self::count());

        Self {
            number: n as _,
        }
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
