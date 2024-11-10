// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::collections::HashMap;

use crate::{types::Graph, Function, FunctionOptimizer, Instruction, Label, Operand, Register};

#[derive(Debug, Default)]
pub struct DeadCodeEliminator {
    control_flow_graph: Graph<usize>,
}

impl FunctionOptimizer for DeadCodeEliminator {
    fn optimize(&mut self, function: &mut Function) {
        for instruction_index in 0..function.instructions().len() {
            self.control_flow_graph.add_node(instruction_index);
        }

        self.control_flow_graph.add_edge(0, 0);

        self.visit_section(function.instructions(), 0);

        for index in (0..function.instructions().len()).rev() {
            if self.control_flow_graph.has_edges_to(&index) {
                continue;
            }

            function.instructions.remove(index);
        }
    }
}

impl DeadCodeEliminator {
    fn position_of_label(&mut self, instructions: &[Instruction], label: &Label) -> usize {
        for (index, instruction) in instructions.iter().enumerate() {
            if let Instruction::Label(l) = instruction {
                if l == label {
                    return index;
                }
            }
        }

        panic!("Illegal label {label:?}")
    }

    fn visit_section(&mut self, instructions: &[Instruction], mut index: usize) {
        while index < instructions.len() {
            match &instructions[index] {
                Instruction::Jump { location } => {
                    let position = self.position_of_label(instructions, location);
                    if self.control_flow_graph.add_edge(index, position) {
                        self.visit_section(instructions, position);
                    }
                    return;
                }

                Instruction::JumpConditional { location, .. } => {
                    let position = self.position_of_label(instructions, location);
                    if !self.control_flow_graph.add_edge(index, position) {
                        self.visit_section(instructions, position);
                    }
                }

                Instruction::Return { .. } => {
                    return;
                }

                _ => (),
            }

            let next = index + 1;
            if next < instructions.len() {
                self.control_flow_graph.add_edge(index, next);
            }

            index += 1;
        }
    }
}

#[derive(Debug, Default)]
pub struct DeadStoreEliminator {
    writing_instructions_per_register: HashMap<Register, usize>,
    instructions_to_remove: Vec<usize>,
}

impl FunctionOptimizer for DeadStoreEliminator {
    fn optimize(&mut self, function: &mut Function) {
        loop {
            *self = Self::default();

            self.analyze_instructions(function.instructions());

            // The instructions that are still in the map are unused, as they would've been removed by a read.
            for value in self.writing_instructions_per_register.values() {
                self.instructions_to_remove.push(*value);
            }

            if self.instructions_to_remove.is_empty() {
                break;
            }

            self.instructions_to_remove.sort();
            for removal_index in self.instructions_to_remove.iter().rev() {
                if !self.can_remove_instruction(&function.instructions[*removal_index]) {
                    continue;
                }
                function.instructions.remove(*removal_index);
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

    fn notice_write(&mut self, register: &Register, index: usize) {
        // All instructions that simply load values to this register, and those aren't actually being read,
        // are useless, since we override that value now anyways.

        let previous_writes_without_a_read = self.writing_instructions_per_register.insert(register.clone(), index);

        if let Some(useless_instruction) = previous_writes_without_a_read {
            self.instructions_to_remove.push(useless_instruction);
        }
    }

    fn notice_unavoidable_write(&mut self, register: &Register) {
        let previous_writes_without_a_read = self.writing_instructions_per_register.remove(register);

        if let Some(useless_instruction) = previous_writes_without_a_read {
            self.instructions_to_remove.push(useless_instruction);
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

                Instruction::Call { ret_val_reg, arguments, .. } => {
                    for arg in arguments {
                        self.notice_read(arg);
                    }

                    self.notice_unavoidable_write(ret_val_reg);
                }

                Instruction::Increment { register } => {
                    self.notice_write(register, index);
                }

                Instruction::Jump { location } => {
                    _ = location;
                }

                Instruction::JumpConditional { condition, location } => {
                    _ = condition;
                    _ = location;
                }

                Instruction::Label(..) => (),

                Instruction::LoadImmediate { destination_reg, immediate } => {
                    _ = immediate;
                    self.notice_write(destination_reg, index);
                }

                Instruction::Move { source, destination } => {
                    debug_assert_ne!(source, destination);

                    // We use the value, which means that until this point, instructions for that register were actually
                    // useful.
                    self.notice_read(source);

                    // But writes to the destination register before this are useless, since we override that value now.
                    self.notice_write(destination, index);
                }

                Instruction::MathOperation { operation, destination, lhs, rhs } => {
                    _ = operation;

                    if let Operand::Register(lhs) = lhs {
                        self.notice_read(lhs);
                    }

                    if let Operand::Register(rhs) = rhs {
                        self.notice_read(rhs);
                    }

                    self.notice_write(destination, index);
                }

                Instruction::Return { value_reg } => {
                    if let Some(return_value) = value_reg {
                        self.notice_read(return_value);
                    }
                }

                Instruction::StackAlloc { dst, size } => {
                    self.notice_write(dst, index);
                    _ = size;
                }

                Instruction::LoadPtr { destination, base_ptr, offset, typ: size } => {
                    _ = size;

                    self.notice_write(destination, index);
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

                    if let Operand::Register(value) = value {
                        self.notice_read(value);
                    }
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

#[cfg(test)]
mod tests {
    use babbelaar::BabString;

    use crate::Immediate;

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
            Instruction::LoadImmediate { immediate: Immediate::Integer8(12), destination_reg: Register::new(1) },
            Instruction::Return { value_reg: None }
        ],
        &[
            Instruction::LoadImmediate { immediate: Immediate::Integer8(12), destination_reg: Register::new(1) },
            Instruction::Return { value_reg: None }
        ]
    )]
    #[case(
        &[
            Instruction::LoadImmediate { immediate: Immediate::Integer8(12), destination_reg: Register::new(1) },
            Instruction::Return { value_reg: Some(Register::new(1)) },
            Instruction::Return { value_reg: None }
        ],
        &[
            Instruction::LoadImmediate { immediate: Immediate::Integer8(12), destination_reg: Register::new(1) },
            Instruction::Return { value_reg: Some(Register::new(1)) }
        ]
    )]
    #[case(
        &[
            Instruction::Jump { location: Label::new(8) },
            Instruction::LoadImmediate { immediate: Immediate::Integer8(7), destination_reg: Register::new(2) },
            Instruction::Return { value_reg: Some(Register::new(7)) },
            Instruction::Label(Label::new(8)),
            Instruction::LoadImmediate { immediate: Immediate::Integer16(1541), destination_reg: Register::new(9) },
            Instruction::Return { value_reg: Some(Register::new(9)) },
        ],
        &[
            Instruction::Jump { location: Label::new(8) },
            Instruction::Label(Label::new(8)),
            Instruction::LoadImmediate { immediate: Immediate::Integer16(1541), destination_reg: Register::new(9) },
            Instruction::Return { value_reg: Some(Register::new(9)) },
        ]
    )]
    fn dead_code_eliminator(#[case] input_instructions: &[Instruction], #[case] expected_output_instructions: &[Instruction]) {
        let mut function = Function {
            name: BabString::new_static("testfunctie"),
            argument_registers: Vec::new(),
            instructions: input_instructions.to_vec(),
            label_names: HashMap::new(),
        };

        DeadCodeEliminator::default().optimize(&mut function);

        assert_eq!(function.instructions(), expected_output_instructions);
    }
}
