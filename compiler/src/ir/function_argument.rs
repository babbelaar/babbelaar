// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::TypeInfo;

use super::{PrimitiveType, Register};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionArgument {
    register: Register,
    type_info: TypeInfo,
    primitive_type: PrimitiveType,
}

impl FunctionArgument {
    #[must_use]
    pub fn new(register: Register, type_info: TypeInfo, primitive_type: PrimitiveType) -> Self {
        Self {
            register,
            type_info,
            primitive_type,
        }
    }

    #[must_use]
    pub fn register(&self) -> Register {
        self.register
    }

    #[must_use]
    pub fn size(&self) -> usize {
        self.primitive_type.bytes()
    }

    pub(crate) fn set_register(&mut self, register: Register) {
        self.register = register;
    }
}
