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
        analysis.dump_result();

        let only_return_register = analysis.find_only_return_register();
        let registers: Vec<(IrRegister, RegisterLifetime)> = analysis.into_sorted_vec();

        let mut currently_mapped: HashMap<IrRegister, R> = HashMap::new();

        let mut currently_available = R::caller_saved_registers().iter()
            .chain(R::callee_saved_registers().iter())
            .copied()
            .collect::<VecDeque<R>>();

        for (idx, register) in function.argument_registers().iter().enumerate() {
            let reg = R::argument_nth(idx);

            currently_mapped.insert(*register, reg);
            self.mappings.insert(*register, Some(reg));

            if let Some((index, _)) = currently_available.iter().enumerate().find(|(_, avail_reg)| **avail_reg == reg) {
                currently_available.remove(index);
            }
        }

        if let Some(return_register) = only_return_register {
            currently_mapped.insert(return_register, R::return_register());
            self.mappings.insert(return_register, Some(R::return_register()));
        }

        for (index, _) in function.instructions().iter().enumerate() {
            for (reg, lifetime) in &registers {
                if !lifetime.is_active_at(index) && lifetime.first_use() < index {
                    if let Some(mapped) = currently_mapped.remove(reg) {
                        if !currently_available.contains(&mapped) {
                            currently_available.push_front(mapped);
                        }
                    }

                    continue;
                }

                // Already mapped
                if self.mappings.contains_key(reg) {
                    continue;
                }

                let register_available_after_this_instruction = registers.iter()
                    .filter(|(_, lifetime)| lifetime.last_use() == index)
                    .filter_map(|(reg, _)| Some((*reg, *currently_mapped.get(reg)?)))
                    .next();

                if let Some((prev_avail_register, available_register)) = register_available_after_this_instruction {
                    currently_mapped.remove(&prev_avail_register);

                    currently_mapped.insert(*reg, available_register);
                    self.mappings.insert(*reg, Some(available_register));
                    continue;
                }

                // Check if there is a register available, otherwise we should spill
                if let Some(available_register) = currently_available.pop_front() {
                    currently_mapped.insert(*reg, available_register);
                    self.mappings.insert(*reg, Some(available_register));
                    continue;
                }

                self.mappings.insert(*reg, None);
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
        println!();
    }
}
