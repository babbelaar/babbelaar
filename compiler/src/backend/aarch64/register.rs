// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ArmRegister {
    pub(super) number: u8,
}

impl ArmRegister {
    pub const X0: Self = Self { number: 0 };
    pub const X30: Self = Self { number: 30 };
}

impl Display for ArmRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("x")?;
        self.number.fmt(f)
    }
}
