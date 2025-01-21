// Copyright (C) 2024 - 2025 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::collections::BTreeMap;

use log::debug;

use crate::{ControlFlowGraph, Register as IrRegister, TargetInstruction};

use super::RegisterLifetime;

#[derive(Debug)]
pub struct LifeAnalysis {
    result: LifeAnalysisResult,
    function_calls: Vec<usize>,

    cfg: ControlFlowGraph,
}

impl LifeAnalysis {
    #[must_use]
    pub fn analyze<I: TargetInstruction>(argument_registers: &[IrRegister], instructions: &[I]) -> LifeAnalysisResult {
        let mut this = Self {
            result: Default::default(),
            function_calls: Default::default(),

            cfg: ControlFlowGraph::new(instructions),
        };

        this.cfg.dump();

        this.add_argument_registers(argument_registers);

        for (index, instruction) in instructions.iter().enumerate() {
            this.add_instruction(index, instruction);
        }

        for call_index in this.function_calls {
            for (_, lifetime) in &mut this.result.lifetimes {
                if lifetime.first_use() < call_index && lifetime.last_use() > call_index {
                    lifetime.did_use_between_calls();
                }
            }
        }

        for loop_range in this.cfg.loop_ranges() {
            let mut function_calls = 0;
            for instruction in &instructions[loop_range.clone()] {
                if instruction.is_call() {
                    function_calls += 1;
                }
            }

            for lifetime in this.result.lifetimes.values_mut() {
                if !loop_range.contains(&lifetime.first_use()) && !loop_range.contains(&lifetime.last_use()) {
                    continue;
                }

                if function_calls != 0 {
                    lifetime.did_use_between_calls();
                }

                lifetime.did_use_during_loop(loop_range.clone());

                if lifetime.first_use() < loop_range.start {
                    // een register die buiten een loop gedefinieerd is, maar eindigt binnen een loop, moet binnen die hele loop nog gebruikt worden
                    lifetime.did_use_at(loop_range.end);
                }
            }
        }

        this.result
    }

    fn add_instruction<I: TargetInstruction>(&mut self, index: usize, instruction: &I) {
        let info = instruction.info();

        let is_call = instruction.is_call();
        let is_return = instruction.is_return();

        for dst in info.virtual_destinations() {
            self.add_lifetime(&dst, index);
        }

        for (idx, src) in info.virtual_sources().enumerate() {
            let info = self.add_lifetime(&src, index);

            if is_call {
                info.did_use_for_argument(idx);
            }

            if is_return {
                info.did_use_for_return();
            }
        }

        if is_call {
            self.add_call(index);
        }
    }

    fn add_lifetime(&mut self, register: &IrRegister, index: usize) -> &mut RegisterLifetime {
        let lifetime = self.result.lifetimes.entry(*register)
            .or_insert(RegisterLifetime::new(index));

        lifetime.did_use_at(index);
        lifetime
    }

    fn add_argument_registers(&mut self, argument_registers: &[IrRegister]) {
        for register in argument_registers {
            self.add_lifetime(register, 0);
        }
    }

    fn add_call(&mut self, index: usize) {
        self.function_calls.push(index);
    }
}

#[derive(Debug, Default)]
pub struct LifeAnalysisResult {
    lifetimes: BTreeMap<IrRegister, RegisterLifetime>,
}

impl LifeAnalysisResult {
    #[must_use]
    pub fn find_only_return_register(&self) -> Option<IrRegister> {
        let mut result_lifetime = None;
        let mut result = None;

        for (register, lifetime) in &self.lifetimes {
            if lifetime.times_used_as_return() != 0 {
                // When there is more than 1 register used as the return register, we should not return anything.
                if result.is_some() {
                    return None;
                }

                result_lifetime = Some(lifetime);
                result = Some(*register);
            }
        }

        if result_lifetime?.times_used_between_calls() != 0 {
            return None;
        }

        result
    }

    #[must_use]
    pub fn lifetimes(&self) -> &BTreeMap<IrRegister, RegisterLifetime> {
        &self.lifetimes
    }

    #[must_use]
    pub fn into_sorted_vec(self) -> Vec<(IrRegister, RegisterLifetime)>  {
        let mut s: Vec<(IrRegister, RegisterLifetime)> = self.lifetimes.into_iter().collect();
        s.sort_by(|a, b| a.1.first_use().cmp(&b.1.first_use()));
        s
    }

    #[allow(unused)]
    pub fn dump_result(&self) {
        for (reg, lifetime) in self.lifetimes() {
            debug!("Register {reg} has a lifetime of {} instructions. start={} end={}", lifetime.length(), lifetime.first_use(), lifetime.last_use());
            if lifetime.times_used_as_return() != 0 {
                debug!("    and was used {}x as the return value", lifetime.times_used_as_return());
            }
            if lifetime.times_used_between_calls() != 0 {
                debug!("    and was used {}x between subroutine calls", lifetime.times_used_between_calls());
            }
            if lifetime.times_used_as_argument() != 0 {
                debug!("    and was used {}x as an argument to a function", lifetime.times_used_as_argument());
            }
            if let Some(idx) = lifetime.last_loop_index() {
                debug!("    and was used during loop ending at {idx}");
            }
        }
    }
}
