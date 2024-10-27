// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::Display;

use crate::Label;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArmBranchLocation {
    Label(Label),

    #[allow(unused)]
    PcRelativeOffset(i32),
}

impl Display for ArmBranchLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Label(label) => label.fmt(f),
            Self::PcRelativeOffset(value) => value.fmt(f),
        }
    }
}
