// Copyright (C) 2024 - 2025 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

mod aarch64;
mod abstract_register;
mod amd64;
mod data_section;
mod object;
mod register_allocation;
mod relocation;
mod target_instruction;

use crate::Function;

pub use self::{
    aarch64::AArch64CodeGenerator,
    abstract_register::{
        AbstractRegister,
        VirtOrPhysReg,
    },
    amd64::Amd64CodeGenerator,
    data_section::{
        DataSection,
        DataSectionKind,
        DataSectionOffset,
    },
    object::{
        CompiledFunction,
        CompiledObject,
    },
    register_allocation::{
        AllocatableRegister,
        LifeAnalysis,
        RegisterAllocator,
        RegisterLifetime,
    },
    relocation::{
        Relocation,
        RelocationMethod,
        RelocationType,
    },
    target_instruction::{
        TargetBranchInfo,
        TargetInstruction,
        TargetInstructionInfo,
    },
};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum Backend {
    Babbelaar,
    #[default]
    Cranelift,
}

pub trait CodeGenerator {
    #[must_use]
    fn compile(function: &Function) -> CompiledFunction;
}
