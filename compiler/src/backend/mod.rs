// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

mod aarch64;
mod amd64;
mod data_section;
mod object;
mod register_allocation;
mod relocation;

use crate::Function;

pub use self::{
    aarch64::AArch64CodeGenerator,
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
};

pub trait CodeGenerator {
    #[must_use]
    fn compile(function: &Function) -> CompiledFunction;
}
