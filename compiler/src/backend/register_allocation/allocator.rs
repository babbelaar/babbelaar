// Copyright (C) 2024 - 2025 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::{HashMap, HashSet}, ops::Range};

use log::debug;

use crate::{backend::VirtOrPhysReg, Platform, Register as IrRegister, TargetInstruction};

use super::{life_analysis::LifeAnalysisResult, AllocatableRegister, LifeAnalysis, RegisterLifetime};

#[derive(Debug)]
pub struct RegisterAllocator<R: AllocatableRegister> {
    platform: Platform,
    registers_to_save: Vec<R>,
    register_mappings: HashMap<IrRegister, Vec<Allocation<R>>>,
}

impl<R: AllocatableRegister> RegisterAllocator<R> {
    #[must_use]
    pub fn new<I: TargetInstruction<PhysReg = R>>(platform: Platform, argument_registers: &[IrRegister], instructions: &[I]) -> Self {
        let mut this = Self {
            platform: platform.clone(),
            registers_to_save: Vec::new(),
            register_mappings: HashMap::new(),
        };

        this.allocate(argument_registers, instructions);

        debug!("Mappings: {:#?}", this.register_mappings);

        this
    }

    /// Which callee-saved self.register_lifetimes do we use? These need to be saved by the
    /// architecture-specific code generator.
    #[must_use]
    pub fn callee_saved_registers_to_save<'a>(&'a self) -> impl DoubleEndedIterator<Item = R> + 'a {
        self.registers_to_save.iter().cloned()
    }

    #[must_use]
    pub fn get_mapping(&self, reg: &IrRegister, instruction_id: usize) -> Option<R> {
        debug!("Get mapping {reg} during {instruction_id}");
        for alloc in self.register_mappings.get(reg)? {
            if alloc.range.contains(&instruction_id) {
                return alloc.register;
            }
        }

        None
    }


    // options:
    // - allocate to normal register
    // - spill to stack
    // - rematerialize (kind of hard for now)
    //
    // we need to:
    // - make sure function argument registers don't overwrite actual values
    //
    // we could:
    // - optimize such that the return value doesn't need to be moved to the return register
    fn allocate<I: TargetInstruction<PhysReg = R>>(&mut self, argument_registers: &[IrRegister], instructions: &[I]) {
        let analysis = LifeAnalysis::analyze(argument_registers, instructions);
        analysis.dump_result();

        let mut optimal_register_mappings = self.analyze_optimal_register_mappings(&analysis, instructions);

        debug!("optimal_register_mappings: {optimal_register_mappings:#?}");

        let instruction_count = instructions.len();
        let mut available_registers = Vec::new();
        for reg in R::caller_saved_registers(&self.platform).iter().cloned() {
            available_registers.push(PlatformRegisterSchedule::new(reg, false, instruction_count));
        }

        for reg in R::callee_saved_registers(&self.platform).iter().cloned() {
            available_registers.push(PlatformRegisterSchedule::new(reg, true, instruction_count));
        }

        for (instruction_id, instruction) in instructions.iter().enumerate() {
            let info = instruction.info();

            if instruction.is_call() {
                for (arg_idx, _) in info.virtual_sources().enumerate() {
                    let arg_reg = R::argument_nth(&self.platform, arg_idx);
                    for schedule in &mut available_registers {
                        if schedule.reg == arg_reg {
                            schedule.mark_unavailable_on(instruction_id, None);
                        }
                    }
                }
            }

            let as_mov = instruction.as_rr_move().and_then(|(dst, src)| dst.as_virtual().xor(src.as_virtual()));

            for reg in info.physical_destinations().chain(info.physical_sources()) {
                for schedule in &mut available_registers {
                    if schedule.reg == reg {
                        schedule.mark_unavailable_on(instruction_id, as_mov);
                    }
                }
            }
        }

        for (virtual_reg, lifetime) in analysis.lifetimes() {
            let Some(optimal_mappings) = optimal_register_mappings.get(virtual_reg) else { continue };

            if self.allocate_to_next_available_reg(virtual_reg, lifetime, &mut available_registers, |r| optimal_mappings.contains(&r.reg)) {
                continue;
            }

            debug!("Kon optimale niet mappen!");
            optimal_register_mappings.remove(virtual_reg);
        }

        for (virtual_reg, lifetime) in analysis.lifetimes() {
            if optimal_register_mappings.contains_key(virtual_reg) {
                continue;
            }

            let mut is_mapped = false;

            if !is_mapped {
                is_mapped = self.allocate_to_next_available_reg(virtual_reg, lifetime, &mut available_registers, |_| true);
            }

            if !is_mapped {
                let mapping = self.register_mappings.entry(*virtual_reg).or_default();
                mapping.push(Allocation {
                    range: 0..instruction_count,
                    register: None,
                });
            }
        }

        self.check_callee_saved_registers(available_registers);
    }

    fn check_callee_saved_registers(&mut self, registers: Vec<PlatformRegisterSchedule<R>>) {
        for reg in registers {
            if reg.is_callee_saved && reg.is_used() {
                self.registers_to_save.push(reg.reg);
            }
        }
    }

    #[must_use]
    fn allocate_to_next_available_reg<F: Fn(&PlatformRegisterSchedule<R>) -> bool>(
        &mut self,
        virtual_reg: &IrRegister,
        lifetime: &RegisterLifetime,
        available_registers: &mut [PlatformRegisterSchedule<R>],
        pred: F,
    ) -> bool {
        for schedule in available_registers {
            let range = lifetime.first_use()..(lifetime.last_use() + 1);

            if !pred(&schedule) {
                continue;
            }

            if !schedule.is_available_during(range.clone(), *virtual_reg) {
                debug!("Kan optimale register {} niet gebruiken wegens onbeschikbaarheid", schedule.reg.display());
                debug!("   We zoeken {range:?}");
                debug!("   We hebben maar {:?}", &schedule.availability[range]);
                continue;
            }

            if lifetime.times_used_between_calls() != 0 && !schedule.is_callee_saved {
                continue;
            }

            schedule.mark_unavailable_during(range.clone(), *virtual_reg);

            let mapping = self.register_mappings.entry(*virtual_reg).or_default();
            mapping.push(Allocation {
                range,
                register: Some(schedule.reg),
            });
            return true;
        }

        false
    }

    #[must_use]
    fn analyze_optimal_register_mappings<I: TargetInstruction<PhysReg = R>>(&self, analysis: &LifeAnalysisResult, instructions: &[I]) -> HashMap<IrRegister, HashSet<R>> {
        let only_return_register = analysis.find_only_return_register();

        let mut registers = HashMap::new();

        for (register, lifetime) in analysis.lifetimes() {
            let mut set = HashSet::new();

            if only_return_register == Some(*register) && lifetime.times_used_between_calls() == 0 {
                set.insert(R::return_register(&self.platform));
            }

            for instr in instructions {
                if let Some((VirtOrPhysReg::Physical(dst), VirtOrPhysReg::Virtual(src))) = instr.as_rr_move() {
                    if src == *register {
                        set.insert(dst);
                    }
                }
            }

            registers.insert(*register, set);
        }

        registers
    }
}

#[derive(Debug, Default)]
struct Allocation<R: AllocatableRegister> {
    range: Range<usize>,
    register: Option<R>,
}

#[derive(Debug)]
struct PlatformRegisterSchedule<R: AllocatableRegister> {
    reg: R,
    is_callee_saved: bool,
    // TODO: this can easily be replaced with a better structure
    availability: Vec<bool>,
    move_hints: Vec<Option<IrRegister>>,
}

impl<R: AllocatableRegister> PlatformRegisterSchedule<R> {
    fn new(reg: R, is_callee_saved: bool, instruction_count: usize) -> Self {
        Self {
            reg,
            availability: vec![true; instruction_count],
            is_callee_saved,
            move_hints: vec![None; instruction_count],
        }
    }

    fn mark_unavailable_on(&mut self, instruction_id: usize, mov_reg_hint: Option<IrRegister>) {
        self.availability[instruction_id] = false;
        self.move_hints[instruction_id] = mov_reg_hint;
    }

    #[must_use]
    fn is_available_during(&self, range: Range<usize>, register: IrRegister) -> bool {
        for i in range {
            if self.availability[i] {
                continue;
            }

            if self.move_hints[i] == Some(register) {
                continue;
            }

            return false;
        }

        true
    }

    fn mark_unavailable_during(&mut self, range: Range<usize>, register: IrRegister) {
        for idx in range {
            self.availability[idx] = false;
            self.move_hints[idx] = Some(register);
        }
    }

    /// Is this register used at least once?
    #[must_use]
    fn is_used(&self) -> bool {
        self.availability.iter().any(|x| !x)
    }
}
