// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::{HashMap, HashSet, VecDeque}, fmt::Debug};

use log::debug;

use crate::{Function, Register as IrRegister};

mod allocatable_register;
mod life_analysis;
mod register_lifetime;

pub use self::allocatable_register::AllocatableRegister;

use self::{
    life_analysis::LifeAnalysis,
    register_lifetime::RegisterLifetime,
};

#[derive(Debug)]
pub struct RegisterAllocator<R: AllocatableRegister> {
    mappings: HashMap<IrRegister, Option<R>>,
    registers_to_save: Vec<R>,
    currently_mapped: HashMap<IrRegister, R>,
    currently_available: VecDeque<R>,
    only_return_register: Option<IrRegister>,
}

impl<R: AllocatableRegister> RegisterAllocator<R> {
    #[must_use]
    pub fn new(function: &Function) -> Self {
        let mut this = Self {
            mappings: HashMap::new(),
            registers_to_save: Vec::new(),
            currently_mapped: HashMap::new(),
            currently_available: R::caller_saved_registers().iter()
                .chain(R::callee_saved_registers().iter())
                .copied()
                .collect::<VecDeque<R>>(),
            only_return_register: None,
        };

        this.allocate(function);

        this
    }

    /// Which callee-saved self.register_lifetimes do we use? These need to be saved by the
    /// architecture-specific code generator.
    #[must_use]
    pub fn callee_saved_registers_to_save(&self) -> &[R] {
        &self.registers_to_save
    }

    #[must_use]
    pub fn get_mapping(&self, reg: &IrRegister) -> Option<R> {
        self.mappings.get(reg).copied()?
    }

    fn allocate(&mut self, function: &Function) {
        let analysis = LifeAnalysis::analyze(function.argument_registers(), function.instructions());
        analysis.dump_result();

        self.only_return_register = analysis.find_only_return_register();
        let register_lifetimes = analysis.into_sorted_vec();

        self.map_argument_registers(function);

        self.map_only_return_register();
        self.map_registers(function, register_lifetimes);

        self.dump_mappings();
    }

    fn map_registers(&mut self, function: &Function, register_lifetimes: Vec<(IrRegister, RegisterLifetime)>) {
        for (index, _) in function.instructions().iter().enumerate() {
            for (reg, lifetime) in &register_lifetimes {
                if !lifetime.is_active_at(index) && lifetime.first_use() < index {
                    if let Some(mapped) = self.currently_mapped.remove(reg) {
                        if !self.currently_available.contains(&mapped) {
                            self.currently_available.push_front(mapped);
                        }
                    }

                    continue;
                }

                // Already mapped
                if self.mappings.contains_key(reg) {
                    continue;
                }

                let register_available_after_this_instruction: Vec<_> = register_lifetimes.iter()
                    .filter(|(_, lifetime)| lifetime.last_use() == index)
                    .filter_map(|(reg, _)| Some((*reg, *self.currently_mapped.get(reg)?)))
                    .collect();

                if lifetime.times_used_between_calls() > 0 {
                    if let Some((prev_avail_register, available_register)) = register_available_after_this_instruction.iter().find(|(_, reg)| reg.is_callee_saved()) {
                        self.currently_mapped.remove(prev_avail_register);

                        self.map(*reg, *available_register);
                        continue;
                    }

                    let available = self.currently_available.iter()
                        .cloned()
                        .enumerate()
                        .filter(|(_, reg)| reg.is_callee_saved())
                        .next();

                    if let Some((idx, available_register)) = available {
                        self.currently_available.remove(idx);
                        self.map(*reg, available_register);
                        continue;
                    }

                    panic!("Spilled!")
                }

                if let Some((prev_avail_register, available_register)) = register_available_after_this_instruction.first() {
                    self.currently_mapped.remove(prev_avail_register);

                    self.map(*reg, *available_register);
                    continue;
                }

                // Check if there is a register available, otherwise we should spill
                if let Some(available_register) = self.currently_available.pop_front() {
                    self.map(*reg, available_register);
                    continue;
                }

                self.mappings.insert(*reg, None);
            }
        }
    }

    #[allow(unused)]
    fn dump_mappings(&self) {
        debug!("Register mappings:");
        for (virtual_reg, physical_reg) in &self.mappings {
            if let Some(physical_reg) = physical_reg {
                debug!("    IR {virtual_reg} is mapped to {physical_reg}");
            } else {
                debug!("    IR {virtual_reg} is spilled");
            }
        }
        debug!("");
    }

    /// TODO: this is a hack! We should instead try to avoid situations in which
    /// RISC-architectures need more registers than was allocated up front.
    #[must_use]
    pub fn hacky_random_available_register(&self) -> Option<R> {
        let mut available = HashSet::new();
        available.extend(R::caller_saved_registers().iter().map(|r| *r));

        for mapping in self.mappings.values().flatten() {
            available.remove(mapping);
        }

        available.into_iter().next()
    }

    fn map_argument_registers(&mut self, function: &Function) {
        for (idx, register) in function.argument_registers().iter().enumerate() {
            let reg = R::argument_nth(idx);

            self.map(*register, reg);

            if let Some((index, _)) = self.currently_available.iter().enumerate().find(|(_, avail_reg)| **avail_reg == reg) {
                self.currently_available.remove(index);
            }
        }
    }

    /// This is an optimization where we try to find the IR registers that are used as the return value,
    /// and if there is only one, we always allocate that to the ISA return register.
    fn map_only_return_register(&mut self) {
        let Some(return_register) = self.only_return_register else {
            return;
        };

        self.map(return_register, R::return_register());

        if let Some((idx, _)) = self.currently_available.iter().enumerate().find(|(_, r)| **r == R::return_register()) {
            self.currently_available.remove(idx);
        }
    }

    fn map(&mut self, ir: IrRegister, isa: R) {
        if isa.is_callee_saved() {
            if !self.registers_to_save.contains(&isa) {
                self.registers_to_save.push(isa);
            }
        }

        self.currently_mapped.insert(ir, isa);
        self.mappings.insert(ir, Some(isa));
    }
}
