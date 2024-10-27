// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::Display;

use crate::JumpCondition;


/// **Reference:** [Arm® Architecture Reference Manual for A-profile architecture](arm) \
/// **Section:** C1.2.4 Condition code \
///
/// [arm]: https://developer.arm.com/documentation/ddi0487/latest
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
#[allow(unused)]
pub enum ArmConditionCode {
    /// Equal Equal Z == 1
    EQ = 0b0000,

    /// Not equal Not equal or unordered Z == 0
    NE = 0b0001,

    /// or HS Carry set Greater than, equal, or unordered C == 1
    CS = 0b0010,

    /// or LO Carry clear Less than C == 0
    CC = 0b0011,

    /// Minus, negative Less than N == 1
    MI = 0b0100,

    /// Plus, positive or zero Greater than, equal, or unordered N == 0
    PL = 0b0101,

    /// Overflow Unordered V == 1
    VS = 0b0110,

    /// No overflow Ordered V == 0
    VC = 0b0111,

    /// Unsigned higher Greater than, or unordered C ==1 && Z == 0
    HI = 0b1000,

    /// Unsigned lower or same Less than or equal !(C ==1 && Z ==0)
    LS = 0b1001,

    /// Signed greater than or equal Greater than or equal N == V
    GE = 0b1010,

    /// Signed less than Less than, or unordered N != V
    LT = 0b1011,

    /// Signed greater than Greater than Z == 0 && N == V
    GT = 0b1100,

    /// Signed less than or equal Less than, equal, or unordered !(Z == 0 && N == V)
    LE = 0b1101,

    /// Always Always Any
    AL = 0b1110,

    /// Always Always Any
    NV = 0b1111,
}

impl ArmConditionCode {
    #[must_use]
    pub const fn name(&self) -> &'static str {
        match self {
            Self::EQ => "eq",
            Self::NE => "ne",
            Self::CS => "cs",
            Self::CC => "cc",
            Self::MI => "mi",
            Self::PL => "pl",
            Self::VS => "vs",
            Self::VC => "vc",
            Self::HI => "hi",
            Self::LS => "ls",
            Self::GE => "ge",
            Self::LT => "lt",
            Self::GT => "gt",
            Self::LE => "le",
            Self::AL => "al",
            Self::NV => "nv",
        }
    }
}

impl Display for ArmConditionCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name())
    }
}

impl From<JumpCondition> for ArmConditionCode {
    fn from(value: JumpCondition) -> Self {
        match value {
            JumpCondition::Equal => Self::EQ,
            JumpCondition::Greater => Self::GT,
            JumpCondition::GreaterOrEqual => Self::GE,
            JumpCondition::Less => Self::LT,
            JumpCondition::LessOrEqual => Self::LE,
            JumpCondition::NotEqual => Self::NE,
        }
    }
}
