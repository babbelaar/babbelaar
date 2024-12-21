// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

mod dead_code;
mod register;
mod strength;
mod string;

use dead_code::DeadCodeEliminator;
use log::debug;
use strength::StrengthReductor;

use self::{
    dead_code::DeadStoreEliminator,
    register::{
        RegisterDeduplicator,
        RegisterInliner,
    },
    string::StringOptimizer,
};

use crate::{DataSection, Function, Program};

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

    fn optimize_with_context(&mut self, function: &mut Function, ctx: &mut OptimizationContext) {
        _ = ctx;
        self.optimize(function);
    }
}

#[derive(Debug)]
pub struct OptimizationContext<'a> {
    read_only_data: &'a mut DataSection,
}

pub fn optimize_program(program: &mut Program) {
    debug!("Programma voor optimalisatie: {program}");

    let mut ctx = OptimizationContext {
        read_only_data: program.read_only_data.as_mut().unwrap(),
    };

    for function in &mut program.functions {
        optimize_function(function, &mut ctx);
    }
}

fn optimize_function(function: &mut Function, ctx: &mut OptimizationContext) {
    run_optimization::<RegisterInliner>(function, ctx);
    run_optimization::<StringOptimizer>(function, ctx);
    run_optimization::<RegisterInliner>(function, ctx);
    run_optimization::<StrengthReductor>(function, ctx);
    run_optimization::<RegisterDeduplicator>(function, ctx);
    run_optimization::<DeadStoreEliminator>(function, ctx);
    run_optimization::<DeadCodeEliminator>(function, ctx);
}

fn run_optimization<O: FunctionOptimizer>(function: &mut Function, ctx: &mut OptimizationContext) {
    let mut opt = O::default();

    if opt.is_useful(function) {
        opt.optimize_with_context(function, ctx);
    }
}
