// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{Function, Instruction};

#[derive(Debug)]
pub struct Amd64FunctionCharacteristics {
    is_leaf: bool,
}

impl Amd64FunctionCharacteristics {
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
