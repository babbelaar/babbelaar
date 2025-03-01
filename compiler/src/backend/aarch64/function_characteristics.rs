// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{Function, Instruction};

#[derive(Debug)]
pub struct AArch64FunctionCharacteristics {
    is_leaf: bool,
}

impl AArch64FunctionCharacteristics {
    #[must_use]
    pub fn analyze(function: &Function) -> Self {
        let mut this = Self {
            is_leaf: true,
        };

        for instruction in function.instructions() {
            if let Instruction::Call { .. } = instruction {
                this.is_leaf = false;
            }
        }

        this
    }

    #[must_use]
    pub fn is_leaf_function(&self) -> bool {
        self.is_leaf
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AArch64VarArgsConvention {
    RegistersAndStack,
    StackOnly,
}
