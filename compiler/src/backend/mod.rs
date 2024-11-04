// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

mod aarch64;
mod link;
mod object;
mod register_allocation;

use crate::Function;

pub use self::{
    aarch64::AArch64CodeGenerator,
    link::{
        FunctionLink,
        FunctionLinkMethod,
    },
    object::{
        CompiledFunction,
        CompiledObject,
    },
    register_allocation::{
        AllocatableRegister,
        RegisterAllocator,
    }
};

pub trait CodeGenerator {
    #[must_use]
    fn compile(function: &Function) -> CompiledFunction;
}
