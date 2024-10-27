// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

mod aarch64;
mod object;

pub use self::{
    aarch64::AArch64CodeGenerator,
    object::{
        CompiledFunction,
        CompiledObject,
    },
};
