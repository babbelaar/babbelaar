// Copyright (C) 2024 - 2025 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

mod allocator;
mod allocatable_register;
mod life_analysis;
mod register_lifetime;

pub use self::{
    allocatable_register::AllocatableRegister,
    allocator::RegisterAllocator,
    life_analysis::LifeAnalysis,
    register_lifetime::RegisterLifetime,
};
