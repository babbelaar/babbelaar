// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::{Display, Write};

use crate::{backend::AllocatableRegister, AbstractRegister, Platform};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ArmRegister {
    pub(super) number: u8,
}

#[allow(unused)]
impl ArmRegister {
    pub const X0: Self = Self { number: 0 };
    pub const X1: Self = Self { number: 1 };
    pub const X2: Self = Self { number: 2 };
    pub const X8: Self = Self { number: 8 };
    pub const X19: Self = Self { number: 19 };
    pub const X20: Self = Self { number: 20 };
    pub const X30: Self = Self { number: 30 };

    /// Frame Pointer
    pub const FP: Self = Self { number: 29 };

    /// Link Register
    pub const LR: Self = Self { number: 30 };

    /// Stack Pointer
    pub const SP: Self = Self { number: 31 };
}

impl ArmRegister {
    #[must_use]
    pub fn name(&self, is_64_bit: &bool) -> impl Display {
        ArmRegisterDisplay {
            mode: if *is_64_bit { ArmRegisterMode::Bit64 } else { ArmRegisterMode::Bit32 },
            reg: *self
        }
    }

    #[must_use]
    pub const fn name64(&self) -> impl Display {
        ArmRegisterDisplay {
            mode: ArmRegisterMode::Bit64,
            reg: *self
        }
    }
}

impl AllocatableRegister for ArmRegister {
    fn return_register(platform: &Platform) -> Self {
        _ = platform;
        Self::X0
    }

    fn count() -> usize {
        31
    }

    fn argument_nth_opt(platform: &Platform, n: usize) -> Option<Self> {
        _ = platform;
        debug_assert!(n < Self::count());

        if n > 7 {
            return None;
        }

        Some(Self {
            number: n as _,
        })
    }

    fn callee_saved_registers(platform: &Platform) -> &'static [ArmRegister] {
        _ = platform;
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

    fn caller_saved_registers(platform: &Platform) -> &'static [ArmRegister] {
        _ = platform;
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

    fn display(&self) -> impl Display {
        self.name64()
    }
}

struct ArmRegisterDisplay {
    mode: ArmRegisterMode,
    reg: ArmRegister,
}

impl Display for ArmRegisterDisplay {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.mode {
            ArmRegisterMode::Bit32 => f.write_char('w')?,
            ArmRegisterMode::Bit64 => f.write_char('x')?,
        }

        match self.reg {
            ArmRegister::FP => f.write_str("fp"),
            ArmRegister::LR => f.write_str("lr"),
            ArmRegister::SP => f.write_str("sp"),

            _ => self.reg.number.fmt(f)
        }
    }
}

impl AbstractRegister for ArmRegister {
    fn name8(&self) -> std::borrow::Cow<'_, str> {
        self.name(&false).to_string().into()
    }

    fn name16(&self) -> std::borrow::Cow<'_, str> {
        self.name(&false).to_string().into()
    }

    fn name32(&self) -> std::borrow::Cow<'_, str> {
        self.name(&false).to_string().into()
    }

    fn name64(&self) -> std::borrow::Cow<'_, str> {
        self.name(&true).to_string().into()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ArmRegisterMode {
    Bit32,
    Bit64,
}
