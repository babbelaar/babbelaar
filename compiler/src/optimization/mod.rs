// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

mod dead_code;
mod register;
mod strength;

use dead_code::DeadCodeEliminator;
use log::debug;
use strength::StrengthReductor;

use self::{
    dead_code::DeadStoreEliminator,
    register::{
        RegisterDeduplicator,
        RegisterInliner,
    },
};

use crate::{Function, Program};

/// An optimization that goes through one function.
pub trait FunctionOptimizer: Default {
    /// Returns whether or not running the optimization is likely to be useful,
    /// e.g. actually optimizing something.
    #[must_use]
    fn is_useful(&self, function: &Function) -> bool {
        _ = function;
        true
    }

    fn optimize(&mut self, function: &mut Function);
}

pub fn optimize_program(program: &mut Program) {
    debug!("Programma voor optimalisatie: {program}");
    for function in program.functions_mut() {
        optimize_function(function);
    }
}

pub fn optimize_function(function: &mut Function) {
    run_optimization::<RegisterInliner>(function);
    run_optimization::<StrengthReductor>(function);
    run_optimization::<RegisterDeduplicator>(function);
    run_optimization::<DeadStoreEliminator>(function);
    run_optimization::<DeadCodeEliminator>(function);
}

fn run_optimization<O: FunctionOptimizer>(function: &mut Function) {
    let mut opt = O::default();

    if opt.is_useful(function) {
        opt.optimize(function);
    }
}
