// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::HashMap, mem::take, ops::Range};

use log::{debug, warn};

use crate::{ControlFlowGraph, Function, Instruction, Register};

use super::FunctionOptimizer;

/// Based on the lifetime of a variable, move register instructions if it does
/// not invalidate other stuff.
///
/// Falls under the "Code Motion" category, but is not LICM (Loop-invariant).
#[derive(Debug, Default)]
pub struct LifetimeBasedCodeMover {
}

impl FunctionOptimizer for LifetimeBasedCodeMover {
    fn optimize(&mut self, function: &mut Function) {


        let register_info = self.analyze_uses_and_initializations(function.instructions());
        self.check_move_writing_code(function, register_info);
        debug!("Na schuiven van schrijfcode: {function}");

        let register_info = self.analyze_uses_and_initializations(function.instructions());
        self.check_move_reading_code(function, register_info);

        debug!("Na schuiven van leescode: {function}");
    }
}

impl LifetimeBasedCodeMover {
    #[must_use]
    fn analyze_uses_and_initializations(&self, instructions: &[Instruction]) -> HashMap<Register, RegisterInfo> {
        let mut register_info = HashMap::new();

        for (index, instruction) in instructions.iter().enumerate() {
            let dst = instruction.destination_register();

            if let Some(dst) = dst {
                register_info.entry(dst)
                    .or_insert(RegisterInfo {
                        first_init: index,
                        first_read: None,
                        uses: vec![],
                        writes: vec![index],
                    })
                    .add_write(index);
            }

            for reg in instruction.source_registers() {
                let Some(reg) = register_info.get_mut(&reg) else {
                    panic!("Register {reg} belezen in {instruction} maar nog niet eerder beschreven!")
                };
                reg.add_usage(index);
            }
        }

        for (register, info) in &register_info {
            debug!("CodeMover {register} -> {:?}", info);
        }

        register_info
    }

    // Move reads earlier if possible
    fn check_move_reading_code(&self, function: &mut Function, mut register_info: HashMap<Register, RegisterInfo>) {
        let cfg = ControlFlowGraph::new(function.instructions());
        let mut moves = Vec::new();

        let mut last_call = 0;
        for (index, instruction) in function.instructions().iter().enumerate() {
            debug!("We checken: {instruction}");
            let loops_of_this_instruction: Vec<&Range<usize>> = cfg.loop_ranges().iter().filter(|range| range.contains(&index)).collect();

            if instruction.source_registers().is_empty() {
                continue;
            }

            let mut minimum_pos = 0;

            if instruction.is_call() {
                minimum_pos = last_call + 1;
                last_call = index;
            }
            debug!("    Min pos: {minimum_pos}");

            if !can_move_reading_instruction(&function.instructions[index]) {
                continue;
            }

            let registers = instruction.source_registers();
            if registers.is_empty() {
                continue;
            }

            for register in registers {
                let Some(info) = register_info.get(&register) else {
                    debug_assert!(false, "Deze register zou gevonden moeten zijn door de gebruiksanalysator?");
                    continue;
                };

                if let Some(write) = info.writes.iter().filter(|x| **x < index).max() {
                    minimum_pos = minimum_pos.max(*write + 1);
                    debug!("    Nieuwe pos: {minimum_pos} wegens register {register}");
                } else {
                    debug_assert!(false, "Deze register zou geschreven moeten zijn voordat hij gebruikt kan worden?");
                }
            }

            // Check that we don't screw up other insertions
            if let Some(dst_info) = instruction.destination_register().and_then(|reg| register_info.get(&reg)) {
                for write in &dst_info.writes {
                    // It is earlier than this one, so we're good
                    if *write < minimum_pos {
                        continue;
                    }

                    minimum_pos = *write;
                }
            }

            if minimum_pos > index {
                warn!("Verplaatsen van {instruction} was eerst {index}, maar we wilden het later zetten op {index}");
                continue;
            }

            minimum_pos += find_minimum_non_disturbing_point(&function.instructions()[minimum_pos..index]);
            debug!("    Pos na minimum nd punt: {minimum_pos}");

            // TODO: this can be much better written '_'
            while minimum_pos < index {
                let loops_around_new_pos = cfg.loop_ranges().iter().filter(|range| range.contains(&index));

                if loops_around_new_pos.eq(loops_of_this_instruction.iter().cloned()) {
                    break;
                }

                minimum_pos += 1;
                debug!("    Nog niet in lus...: {minimum_pos}");
            }

            if minimum_pos < index {
                if let Some(dst) = instruction.destination_register() {
                    if let Some(info) = register_info.get_mut(&dst) {
                        if info.first_init > minimum_pos {
                            info.first_init = minimum_pos;
                        }
                    }
                }

                moves.push(Motion {
                    from: index,
                    to: minimum_pos
                });
            }
        }

        self.move_code(moves, function);
    }

    // Move writes later
    fn check_move_writing_code(&self, function: &mut Function, register_info: HashMap<Register, RegisterInfo>) {
        let cfg = ControlFlowGraph::new(function.instructions());
        let mut moves = Vec::new();

        for info in register_info.values() {
            let Some(first_read) = info.first_read else { continue };

            let mut to = first_read;

            // if there are other writes, we can't move them further down...
            for write_idx in &info.writes {
                if *write_idx > first_read {
                    continue;
                }

                if *write_idx == info.first_init {
                    continue;
                }

                if to > *write_idx {
                    to = *write_idx;
                }
            }

            if self.can_move_writing_register(info, first_read, &cfg, &function) {
                moves.push(Motion {
                    to,
                    from: info.first_init,
                });
            }
        }

        self.move_code(moves, function);
    }

    fn move_code(&self, mut moves: Vec<Motion>, function: &mut Function) {
        moves.sort();

        for mov in &moves {
            debug!("Zometeen bewegen: {mov:?}    {}", function.instructions[mov.from]);
        }

        let mut original_instructions: Vec<Option<Instruction>> = take(&mut function.instructions).into_iter().map(|x| Some(x)).collect();

        for instr_idx in 0..original_instructions.len() {
            for mov in &moves {
                if mov.to == instr_idx {
                    function.instructions.push(original_instructions[mov.from].take().unwrap());
                    debug!("AddingImpr{instr_idx} {}", function.instructions.last().unwrap());
                }
            }

            for idx in 0..=instr_idx {
                if original_instructions[idx].is_none() {
                    continue;
                }

                if moves.iter().find(|x| x.from == idx).is_some() {
                    continue;
                }

                function.instructions.push(original_instructions[idx].take().unwrap());
                debug!("AddingOrig{instr_idx} {}", function.instructions.last().unwrap());
            }
        }
    }

    #[must_use]
    fn can_move_writing_register(&self, info: &RegisterInfo, first_read: usize, cfg: &ControlFlowGraph, function: &Function) -> bool {
        // We cannot move Calls!
        if let Instruction::Call { .. } = function.instructions()[info.first_init] {
            return false;
        }

        if let Instruction::InitArg { .. } = function.instructions()[info.first_init] {
            return false;
        }

        debug_assert_ne!(info.first_init, first_read, "Deze zouden apart moeten zijn, en zou alleen kunnen met een Move(r, r) en dat mag ook niet!");

        if first_read == info.first_init + 1 || first_read < info.first_init {
            return false;
        }

        for loop_range in cfg.loop_ranges() {
            let writes_in_this_loop = info.writes.iter()
                .filter(|x| loop_range.contains(x))
                .count();

            if writes_in_this_loop != 0 && writes_in_this_loop != info.writes.len() {
                return false;
            }
        }

        true
    }
}

#[must_use]
fn find_minimum_non_disturbing_point(instructions: &[Instruction]) -> usize {
    let mut result = 0;

    for (index, instruction) in instructions.iter().enumerate() {
        match instruction {
            Instruction::Compare { .. } => (),
            Instruction::Increment { .. } => continue,
            Instruction::InitArg { .. } => (),
            Instruction::Move { .. } => continue,
            Instruction::MoveAddress { .. } => continue,
            Instruction::MoveCondition { .. } => (),
            Instruction::Call { .. } => continue,
            Instruction::Jump { .. } => (),
            Instruction::JumpConditional { .. } => (),
            Instruction::Label(..) => (),
            Instruction::Return { .. } => (),
            Instruction::MathOperation { .. } => continue,
            Instruction::Negate { .. } => continue,
            Instruction::StackAlloc { .. } => continue,
            Instruction::LoadPtr { .. } => continue,
            Instruction::StorePtr { .. } => continue,
        }

        result = index + 1;
        debug!("    Vond ene {instruction}");
    }

    result
}

#[must_use]
fn can_move_reading_instruction(instruction: &Instruction) -> bool {
    match instruction {
        Instruction::Compare { .. } => false,
        Instruction::Increment { .. } => false,
        Instruction::InitArg { .. } => false,
        Instruction::Move { .. } => true,
        Instruction::MoveAddress { .. } => true,
        Instruction::MoveCondition { .. } => false,
        Instruction::Call { .. } => false,
        Instruction::Jump { .. } => false,
        Instruction::JumpConditional { .. } => false,
        Instruction::Label(..) => false,
        Instruction::Return { .. } => false,
        Instruction::MathOperation { .. } => true,
        Instruction::Negate { .. } => true,
        Instruction::StackAlloc { .. } => false,
        Instruction::LoadPtr { .. } => false,
        Instruction::StorePtr { .. } => false,
    }
}

#[derive(Debug, Clone)]
struct RegisterInfo {
    first_init: usize,
    first_read: Option<usize>,
    uses: Vec<usize>,
    writes: Vec<usize>,
}

impl RegisterInfo {
    pub fn add_usage(&mut self, index: usize) {
        if self.first_read.is_none() {
            self.first_read = Some(index);
        }

        if self.uses.last() != Some(&index) {
            self.uses.push(index);
        }
    }

    pub fn add_write(&mut self, index: usize) {
        if self.writes.last() != Some(&index) {
            self.writes.push(index);
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Motion {
    to: usize,
    from: usize,
}
