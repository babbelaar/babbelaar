// Copyright (C) 2025 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{Instruction, JumpCondition, Label, Register};

use super::{AbstractRegister, VirtOrPhysReg};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TargetBranchInfo {
    is_return: bool,
    target_location: Option<Label>,
    condition: Option<JumpCondition>,
}

impl TargetBranchInfo {
    #[must_use]
    pub fn new_jump(location: Label, condition: Option<JumpCondition>) -> Self {
        Self {
            is_return: false,
            target_location: Some(location),
            condition,
        }
    }

    #[must_use]
    pub fn new_return(condition: Option<JumpCondition>) -> Self {
        Self {
            is_return: true,
            target_location: None,
            condition,
        }
    }

    #[must_use]
    pub const fn is_unconditional(&self) -> bool {
        self.condition.is_none()
    }

    /// Returns where the branch will be taken to. Returns [`None`] if it is a return.
    #[must_use]
    pub const fn target_location(&self) -> Option<Label> {
        self.target_location
    }

    #[must_use]
    pub const fn is_return(&self) -> bool {
        self.is_return
    }
}


/// TODO: use smallvec or something
pub trait TargetInstruction {
    type PhysReg: AbstractRegister;

    #[must_use] fn info(&self) -> TargetInstructionInfo<Self::PhysReg>;

    #[must_use] fn branch_info(&self) -> Option<TargetBranchInfo>;

    #[must_use] fn as_label(&self) -> Option<Label>;

    #[must_use] fn is_call(&self) -> bool;


    #[must_use]
    fn is_return(&self) -> bool {
        self.branch_info().is_some_and(|i| i.is_return())
    }
}

impl TargetInstruction for Instruction {
    type PhysReg = Register;

    fn info(&self) -> TargetInstructionInfo<Self::PhysReg> {
        let mut info = TargetInstructionInfo::new();

        if let Some(dst) = self.destination_register() {
            info.add_dst(VirtOrPhysReg::Virtual(dst));
        }

        for src in self.source_registers() {
            info.add_src(VirtOrPhysReg::Virtual(src));
        }

        info
    }

    fn branch_info(&self) -> Option<TargetBranchInfo> {
        match self {
            Self::Jump { location } => Some(TargetBranchInfo::new_jump(*location, None)),
            Self::JumpConditional { location, condition } => Some(TargetBranchInfo::new_jump(*location, Some(*condition))),

            Self::Return { .. } => Some(TargetBranchInfo::new_return(None)),

            _ => None,
        }
    }

    fn as_label(&self) -> Option<Label> {
        if let Self::Label(label) = self {
            Some(label.clone())
        } else {
            None
        }
    }

    fn is_call(&self) -> bool {
        matches!(self, Self::Call { .. })
    }
}

pub struct TargetInstructionInfo<PhysReg: AbstractRegister> {
    destinations: Vec<VirtOrPhysReg<PhysReg>>,
    sources: Vec<VirtOrPhysReg<PhysReg>>,
}

impl<PhysReg: AbstractRegister> TargetInstructionInfo<PhysReg> {
    #[must_use]
    pub fn new() -> Self {
        Self {
            destinations: Vec::new(),
            sources: Vec::new(),
        }
    }

    #[must_use]
    pub fn virtual_destinations(&self) -> impl Iterator<Item = Register> + '_ {
        self.destinations.iter().filter_map(VirtOrPhysReg::as_virtual)
    }

    #[must_use]
    pub fn virtual_sources(&self) -> impl Iterator<Item = Register> + '_ {
        self.sources.iter().filter_map(VirtOrPhysReg::as_virtual)
    }

    pub fn add_dst(&mut self, dst: impl Into<VirtOrPhysReg<PhysReg>>) {
        self.destinations.push(dst.into());
    }

    pub fn add_src(&mut self, src: impl Into<VirtOrPhysReg<PhysReg>>) {
        self.sources.push(src.into());
    }
}
