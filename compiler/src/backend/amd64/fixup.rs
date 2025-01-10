// Copyright (C) 2025 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{backend::VirtOrPhysReg, DataSectionOffset, TargetInstructionInfo};

use super::register::Amd64Register;

#[derive(Debug, Clone, PartialEq)]
pub enum Amd64FixUp {
    MoveAddress {
        destination: VirtOrPhysReg<Amd64Register>,
        offset: DataSectionOffset,
    },

    StackAlloc {
        dst: VirtOrPhysReg<Amd64Register>,
        instruction_id: usize,
    },
}

impl Amd64FixUp {
    pub fn add_info(&self, info: &mut TargetInstructionInfo<Amd64Register>) {
        match self {
            Self::MoveAddress { destination, offset: _ } => {
                info.add_dst(destination);
            }

            Self::StackAlloc { dst, instruction_id: _ } => {
                info.add_dst(dst);
            }
        }
    }
}
