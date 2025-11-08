// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::collections::HashMap;

use log::debug;

use crate::{ControlFlowGraph, Function, FunctionOptimizer, Instruction, Operand, Register};

#[derive(Debug, Default)]
pub struct DeadCodeEliminator;

impl FunctionOptimizer for DeadCodeEliminator {
    fn optimize(&mut self, function: &mut Function) {
        self.remove_useless_jumps(function);
        self.remove_using_cfg(function);
    }
}

impl DeadCodeEliminator {
    fn remove_using_cfg(&self, function: &mut Function) {
        let cfg = ControlFlowGraph::new(function.instructions());

        for index in (0..function.instructions().len()).rev() {
            if cfg.is_instruction_reachable(index) {
                continue;
            }

            let instr = function.instructions.remove(index);
            debug!("[DeadCode] Removing {instr}");
        }
    }

    fn remove_useless_jumps(&self, function: &mut Function) {
        let mut indices_to_remove = Vec::new();

        for (instruction_id, instruction) in function.instructions().iter().enumerate() {
            if let Instruction::Jump { location: target_location } = instruction {
                let mut can_remove = true;

                for instruction in function.instructions().iter().skip(instruction_id) {
                    match instruction {
                        Instruction::Label(label) => {
                            if label == target_location {
                                break;
                            }
                        }

                        Instruction::Jump { location } | Instruction::JumpConditional { location, .. } => {
                            if location == target_location {
                                continue;
                            }
                        }

                        _ => (),
                    }

                    can_remove = false;
                    break;
                }

                if can_remove {
                    indices_to_remove.push(instruction_id);
                }
            }
        }

        for index in indices_to_remove.into_iter().rev() {
            let instr = function.instructions.remove(index);
            debug!("[UselessJumps] Removing {instr}");
        }
    }
}

#[derive(Debug, Default)]
pub struct DeadStoreEliminator {
    writing_instructions_per_register: HashMap<Register, Store>,
    stores_to_change: Vec<Store>,
}

impl FunctionOptimizer for DeadStoreEliminator {
    fn optimize(&mut self, function: &mut Function) {
        loop {
            *self = Self::default();

            self.analyze_instructions(function.instructions());

            // The instructions that are still in the map are unused, as they would've been removed by a read.
            for store in self.writing_instructions_per_register.values() {
                self.stores_to_change.push(*store);
            }

            if self.stores_to_change.is_empty() {
                break;
            }

            self.stores_to_change.sort();
            for store in self.stores_to_change.iter().rev() {
                let removal_index = store.index;
                match store.kind {
                    StoreKind::CallReturnValue => {
                        let Instruction::Call { ret_val_reg, .. } = &mut function.instructions[removal_index] else {
                            debug_assert!(false, "Verwachtte een 'Call' maar kreeg: {:#?}", function.instructions[removal_index]);
                            continue;
                        };

                        debug_assert!(ret_val_reg.is_some(), "Deze zou gevuld moeten zijn, waarom is ie al leeg?");

                        *ret_val_reg = None;
                    }

                    StoreKind::Normal => {
                        if !self.can_remove_instruction(&function.instructions[removal_index]) {
                            continue;
                        }

                        let instr = function.instructions.remove(removal_index);
                        debug!("[DeadStore] Removing useless {instr}");
                    }
                }
            }
        }
    }
}

impl DeadStoreEliminator {
    fn notice_read(&mut self, register: &Register) {
        self.writing_instructions_per_register.remove(register);
    }

    fn notice_non_removable_write(&mut self, register: &Register) {
        self.writing_instructions_per_register.remove(register);
    }

    fn notice_normal_write(&mut self, register: &Register, index: usize) {
        self.notice_write(register, Store {
            index,
            kind: StoreKind::Normal,
        })
    }

    fn notice_write(&mut self, register: &Register, store: Store) {
        // All instructions that simply load values to this register, and those aren't actually being read,
        // are useless, since we override that value now anyways.

        let previous_write_without_a_read = self.writing_instructions_per_register.insert(register.clone(), store);

        if let Some(useless_store) = previous_write_without_a_read {
            self.stores_to_change.push(useless_store);
        }
    }

    fn analyze_instructions(&mut self, instructions: &[Instruction]) {
        for (index, instruction) in instructions.iter().enumerate() {
            match &instruction {
                Instruction::Compare { lhs, rhs, .. } => {
                    self.notice_read(lhs);

                    if let Operand::Register(rhs) = rhs {
                        self.notice_read(rhs);
                    }
                }

                Instruction::Call { ret_val_reg, arguments, variable_arguments, .. } => {
                    for arg in arguments {
                        self.notice_read(&arg.register());
                    }

                    for arg in variable_arguments {
                        self.notice_read(&arg.register());
                    }

                    if let Some(ret_val_reg) = ret_val_reg {
                        self.notice_write(ret_val_reg, Store {
                            index,
                            kind: StoreKind::CallReturnValue,
                        });
                    }
                }

                Instruction::Increment { register, typ: _ } => {
                    self.notice_normal_write(register, index);
                }

                Instruction::InitArg { destination, arg_idx: _ } => {
                    self.notice_normal_write(destination, index);
                }

                Instruction::Jump { location } => {
                    _ = location;
                }

                Instruction::JumpConditional { condition, location } => {
                    _ = condition;
                    _ = location;
                }

                Instruction::Label(..) => (),

                Instruction::Move { source, destination, typ } => {
                    _ = typ;
                    debug_assert_ne!(*source, Operand::Register(*destination));

                    // We use the value, which means that until this point, instructions for that register were actually
                    // useful.
                    if let Operand::Register(source) = source {
                        self.notice_read(source);
                    }

                    // But writes to the destination register before this are useless, since we override that value now.
                    self.notice_normal_write(destination, index);
                }

                Instruction::MoveAddress { destination, offset } => {
                    _ = offset;
                    self.notice_normal_write(destination, index);
                }

                Instruction::MoveCondition { destination, condition } => {
                    _ = condition;
                    self.notice_normal_write(destination, index);
                }

                Instruction::MathOperation { operation, typ: _, destination, lhs, rhs } => {
                    _ = operation;

                    if let Operand::Register(lhs) = lhs {
                        self.notice_read(lhs);
                    }

                    if let Operand::Register(rhs) = rhs {
                        self.notice_read(rhs);
                    }

                    self.notice_normal_write(destination, index);
                }

                Instruction::Negate { typ: _, dst, src } => {
                    self.notice_normal_write(dst, index);
                    self.notice_read(src);
                }

                Instruction::Return { value_reg } => {
                    if let Some(return_value) = value_reg {
                        self.notice_read(return_value);
                    }
                }

                Instruction::StackAlloc { dst, size } => {
                    self.notice_normal_write(dst, index);
                    _ = size;
                }

                Instruction::LoadPtr { destination, base_ptr, offset, typ: size } => {
                    _ = size;

                    self.notice_normal_write(destination, index);
                    self.notice_read(base_ptr);

                    if let Operand::Register(offset) = offset {
                        self.notice_read(offset);
                    }
                }

                Instruction::StorePtr { base_ptr, offset, value, typ: size } => {
                    _ = size;

                    self.notice_non_removable_write(base_ptr);

                    if let Operand::Register(offset) = offset {
                        self.notice_read(offset);
                    }

                    self.notice_read(value);
                }
            }
        }
    }

    #[must_use]
    fn can_remove_instruction(&self, instruction: &Instruction) -> bool {
        match instruction {
            Instruction::Call { .. } => false,
            _ => true,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Store {
    index: usize,
    kind: StoreKind,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum StoreKind {
    #[default]
    Normal,
    CallReturnValue,
}

#[cfg(test)]
mod tests {
    use babbelaar::BabString;

    use crate::{Immediate, Label, PrimitiveType};

    use super::*;
    use rstest::rstest;
    use pretty_assertions::assert_eq;

    #[rstest]
    #[case(&[], &[])]
    #[case(
        &[
            Instruction::Return { value_reg: None }
        ],
        &[
            Instruction::Return { value_reg: None }
        ]
    )]
    #[case(
        &[
            Instruction::Move { source: Operand::Immediate(Immediate::Integer8(12)), destination: Register::new(1), typ: PrimitiveType::S32 },
            Instruction::Return { value_reg: None }
        ],
        &[
            Instruction::Move { source: Operand::Immediate(Immediate::Integer8(12)), destination: Register::new(1), typ: PrimitiveType::S32 },
            Instruction::Return { value_reg: None }
        ]
    )]
    #[case(
        &[
            Instruction::Move { source: Operand::Immediate(Immediate::Integer8(12)), destination: Register::new(1), typ: PrimitiveType::S32 },
            Instruction::Return { value_reg: Some(Register::new(1)) },
            Instruction::Return { value_reg: None }
        ],
        &[
            Instruction::Move { source: Operand::Immediate(Immediate::Integer8(12)), destination: Register::new(1), typ: PrimitiveType::S32 },
            Instruction::Return { value_reg: Some(Register::new(1)) }
        ]
    )]
    #[case(
        &[
            Instruction::Jump { location: Label::new(8) },
            Instruction::Move { source: Operand::Immediate(Immediate::Integer8(7)), destination: Register::new(2), typ: PrimitiveType::S32 },
            Instruction::Return { value_reg: Some(Register::new(7)) },
            Instruction::Label(Label::new(8)),
            Instruction::Move { source: Operand::Immediate(Immediate::Integer16(1541)), destination: Register::new(9), typ: PrimitiveType::S32 },
            Instruction::Return { value_reg: Some(Register::new(9)) },
        ],
        &[
            Instruction::Jump { location: Label::new(8) },
            Instruction::Label(Label::new(8)),
            Instruction::Move { source: Operand::Immediate(Immediate::Integer16(1541)), destination: Register::new(9), typ: PrimitiveType::S32 },
            Instruction::Return { value_reg: Some(Register::new(9)) },
        ]
    )]
    fn dead_code_eliminator(#[case] input_instructions: &[Instruction], #[case] expected_output_instructions: &[Instruction]) {
        let mut function = Function {
            name: BabString::new_static("testfunctie"),
            return_ty: PrimitiveType::S32,
            arguments: Vec::new(),
            instructions: input_instructions.to_vec(),
            label_names: HashMap::new(),
            ir_register_allocator: Default::default(),
        };

        DeadCodeEliminator::default().optimize(&mut function);

        assert_eq!(function.instructions(), expected_output_instructions);
    }
}
