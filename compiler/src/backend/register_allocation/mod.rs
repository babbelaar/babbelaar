// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::{HashMap, VecDeque}, fmt::Debug};

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
}

impl<R: AllocatableRegister> RegisterAllocator<R> {
    #[must_use]
    pub fn new(function: &Function) -> Self {
        let mut this = Self {
            mappings: HashMap::new(),
        };

        this.allocate(function);

        this
    }

    #[must_use]
    pub fn get_mapping(&self, reg: &IrRegister) -> Option<R> {
        self.mappings.get(reg).copied()?
    }

    fn allocate(&mut self, function: &Function) {
        let analysis = LifeAnalysis::analyze(function.argument_registers(), function.instructions());
        // analysis.dump_result();

        // TODO: reintroduce this sometime, could be a cool optimization :)
        // if let Some(return_register) = analysis.find_only_return_register() {
        //     self.mappings.insert(return_register, Some(R::return_register()));
        // }

        let registers: Vec<(IrRegister, RegisterLifetime)> = analysis.into_sorted_vec();

        let mut currently_mapped = HashMap::new();

        let mut currently_available = R::callee_saved_range()
            .chain(R::caller_saved_range())
            .map(R::nth)
            .collect::<VecDeque<R>>();

        for (idx, register) in function.argument_registers().iter().enumerate() {
            let reg = R::nth(idx);

            currently_mapped.insert(register, reg);
            self.mappings.insert(*register, Some(reg));
        }

        for (index, _) in function.instructions().iter().enumerate() {
            for (reg, lifetime) in &registers {
                if !lifetime.is_active_at(index) {
                    if let Some(mapped) = currently_mapped.remove(reg) {
                        currently_available.push_back(mapped);
                    }

                    continue;
                }

                // Already mapped
                if self.mappings.contains_key(reg) {
                    continue;
                }

                // Check if there is a register available, otherwise we should spill
                let Some(available_register) = currently_available.pop_front() else {
                    self.mappings.insert(*reg, None);
                    continue;
                };

                currently_mapped.insert(reg, available_register);
                self.mappings.insert(*reg, Some(available_register));
            }
        }

        self.dump_mappings();
    }

    #[allow(unused)]
    fn dump_mappings(&self) {
        println!("Register mappings:");
        for (virtual_reg, physical_reg) in &self.mappings {
            if let Some(physical_reg) = physical_reg {
                println!("    IR {virtual_reg} is mapped to {physical_reg}");
            } else {
                println!("    IR {virtual_reg} is spilled");
            }
        }
    }
}
