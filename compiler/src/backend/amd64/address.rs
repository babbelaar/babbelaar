// Copyright (C) 2025 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::{Display, Write};

use crate::{backend::VirtOrPhysReg, AbstractRegister, TargetInstructionInfo};

use super::{Amd64ModRMMode, Amd64Register, SibScale};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Amd64Address<Reg> {
    base: Reg,
    index: Option<Reg>,
    displacement: Amd64Displacement,
    scale: SibScale,
}

impl<Reg: Copy> Amd64Address<Reg> {
    #[must_use]
    pub fn new(base: Reg) -> Self {
        Self {
            base,
            index: Default::default(),
            displacement: Default::default(),
            scale: Default::default(),
        }
    }

    #[must_use]
    pub fn with_index(self, index: Reg, scale: SibScale) -> Self {
        Self {
            index: Some(index),
            scale,
            ..self
        }
    }

    #[must_use]
    pub fn with_displacement(self, displacement: Amd64Displacement) -> Self {
        Self {
            displacement,
            ..self
        }
    }

    #[must_use]
    pub const fn base(&self) -> Reg {
        self.base
    }

    #[must_use]
    pub const fn index(&self) -> Option<Reg> {
        self.index
    }

    #[must_use]
    pub const fn displacement(&self) -> Amd64Displacement {
        self.displacement
    }

    #[must_use]
    pub const fn scale(&self) -> SibScale {
        self.scale
    }
}

impl Amd64Address<Amd64Register> {
    #[must_use]
    pub const fn index_is_64_extended_register(&self) -> bool {
        match self.index {
            Some(index) => index.is_64_extended_register(),
            None => false,
        }
    }
}

impl Amd64Address<VirtOrPhysReg<Amd64Register>> {
    pub fn add_as_source_info(&self, info: &mut TargetInstructionInfo<Amd64Register>) {
        info.add_src(self.base);

        if let Some(index) = self.index {
            info.add_src(index);
        }
    }
}

impl<Reg: AbstractRegister> Display for Amd64Address<Reg> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('[')?;
        f.write_str(&self.base.name64())?;

        if let Some(index) = self.index {
            f.write_str(" + ")?;
            f.write_str(&index.name64())?;

            if self.scale != SibScale::Scale1 {
                f.write_str(" * ")?;
                self.scale.fmt(f)?;
            }
        }

        let displacement = self.displacement().as_i32();
        if displacement != 0 {
            f.write_str(" + ")?;
            displacement.fmt(f)?;
        }

        f.write_char(']')?;

        Ok(())
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum Amd64Displacement {
    #[default]
    None,
    Disp8(i8),
    Disp32(i32),
}

impl Amd64Displacement {
    #[must_use]
    pub const fn mod_rm_mode(&self) -> Amd64ModRMMode {
        match self {
            Self::None => Amd64ModRMMode::NoDisp,
            Self::Disp8(..) => Amd64ModRMMode::Disp8,
            Self::Disp32(..) => Amd64ModRMMode::Disp32,
        }
    }
}

impl Amd64Displacement {
    #[must_use]
    pub fn try_new(value: i64) -> Option<Self> {
        if value == 0 {
            Some(Self::None)
        } else if let Ok(value) = value.try_into() {
            Some(Self::Disp8(value))
        } else if let Ok(value) = value.try_into() {
            Some(Self::Disp32(value))
        } else {
            None
        }
    }
}

impl Amd64Displacement {
    pub const fn as_i32(&self) -> i32 {
        match self {
            Self::None => 0,
            Self::Disp8(val) => *val as i32,
            Self::Disp32(val) => *val,
        }
    }
}

impl From<i8> for Amd64Displacement {
    fn from(value: i8) -> Self {
        if value == 0 {
            Self::None
        } else {
            Self::Disp8(value)
        }
    }
}

#[cfg(test)]
mod tests {
    use rstest::rstest;

    use crate::backend::amd64::register::Amd64Register;

    use super::Amd64Address;

    #[rstest]
    #[case(
        Amd64Address::new(Amd64Register::Rax),
        "[rax]"
    )]
    fn display_test(#[case] input: Amd64Address<Amd64Register>, #[case] expected: &str) {
        assert_eq!(input.to_string(), expected);
    }
}
