// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use babbelaar::BabString;

use super::{Immediate, Register};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Label {
    position: usize,
}

impl Label {
    #[must_use]
    pub fn position(&self) -> usize {
        self.position
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Call {
        name: BabString,
        arguments: Vec<Register>,
        ret_val_reg: Register,
    },

    LoadImmediate {
        immediate: Immediate,
        destination_reg: Register,
    },

    Move {
        source: Register,
        destination: Register,
    },

    Return {
        value_reg: Option<Register>,
    },
}
