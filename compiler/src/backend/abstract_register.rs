// Copyright (C) 2025 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{borrow::Cow, fmt::Debug};

use crate::Register;

pub trait AbstractRegister: Debug + Clone + Copy + PartialEq {
    #[must_use] fn name8(&self) -> Cow<'_, str>;
    #[must_use] fn name16(&self) -> Cow<'_, str>;
    #[must_use] fn name32(&self) -> Cow<'_, str>;
    #[must_use] fn name64(&self) -> Cow<'_, str>;
}

impl AbstractRegister for Register {
    fn name8(&self) -> Cow<'_, str> {
        self.to_string().into()
    }

    fn name16(&self) -> Cow<'_, str> {
        self.to_string().into()
    }

    fn name32(&self) -> Cow<'_, str> {
        self.to_string().into()
    }

    fn name64(&self) -> Cow<'_, str> {
        self.to_string().into()
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum VirtOrPhysReg<Phys: AbstractRegister> {
    Virtual(Register),
    Physical(Phys),
}

impl<Phys: AbstractRegister> VirtOrPhysReg<Phys> {
    #[must_use]
    pub fn as_virtual(&self) -> Option<Register> {
        if let Self::Virtual(register) = self {
            Some(*register)
        } else {
            None
        }
    }

    #[must_use]
    pub fn as_physical(&self) -> Option<Phys> {
        if let Self::Physical(register) = self {
            Some(*register)
        } else {
            None
        }
    }
}

impl<Phys: AbstractRegister> AbstractRegister for VirtOrPhysReg<Phys> {
    fn name8(&self) -> Cow<'_, str> {
        match self {
            Self::Physical(r) => r.name8(),
            Self::Virtual(r) => r.name8(),
        }
    }

    fn name16(&self) -> Cow<'_, str> {
        match self {
            Self::Physical(r) => r.name16(),
            Self::Virtual(r) => r.name16(),
        }
    }

    fn name32(&self) -> Cow<'_, str> {
        match self {
            Self::Physical(r) => r.name32(),
            Self::Virtual(r) => r.name32(),
        }
    }

    fn name64(&self) -> Cow<'_, str> {
        match self {
            Self::Physical(r) => r.name64(),
            Self::Virtual(r) => r.name64(),
        }
    }
}

impl<Phys: AbstractRegister> From<&VirtOrPhysReg<Phys>> for VirtOrPhysReg<Phys> {
    fn from(value: &VirtOrPhysReg<Phys>) -> Self {
        value.clone()
    }
}
