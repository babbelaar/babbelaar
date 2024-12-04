// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{fmt::{Debug, Display}, hash::Hash};

pub trait AllocatableRegister: Debug + Display + Clone + Copy + PartialEq + Eq + Hash + 'static {
    /// Return what register is the return value for this architecture / platform (e.g. x0 on ARM64).
    #[must_use]
    fn return_register() -> Self;

    /// How many general purpose registers there are on this platform (excluding e.g. ESP/X29).
    #[must_use]
    fn count() -> usize;

    #[must_use]
    fn callee_saved_registers() -> &'static [Self];

    #[must_use]
    fn caller_saved_registers() -> &'static [Self];

    #[must_use]
    fn argument_nth(n: usize) -> Self;

    #[must_use]
    fn is_caller_saved(&self) -> bool {
        Self::caller_saved_registers().contains(self)
    }

    #[must_use]
    fn is_callee_saved(&self) -> bool {
        Self::callee_saved_registers().contains(self)
    }
}
