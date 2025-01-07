// Copyright (C) 2025 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{backend::VirtOrPhysReg, TargetInstructionInfo};

use super::ArmRegister;

#[derive(Debug, Clone, PartialEq)]
pub enum AArch64FixUp {
    StackAlloc {
        dst: VirtOrPhysReg<ArmRegister>,
        instruction_id: usize,
    },
    StoreVariadic {
        is_64_bit: bool,
        src: VirtOrPhysReg<ArmRegister>,
        instruction_id: usize,
        offset: i16,
    },
}

impl AArch64FixUp {
    pub fn add_info(&self, info: &mut TargetInstructionInfo<ArmRegister>) {
        match self {
            Self::StackAlloc { dst, instruction_id: _ } => {
                info.add_dst(dst);
            }

            Self::StoreVariadic { is_64_bit: _, src, instruction_id: _, offset: _ } => {
                info.add_src(src);
            }
        }
    }
}
