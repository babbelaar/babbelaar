// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.


use crate::{Graph, Function, Instruction, Label};

#[derive(Debug, Clone)]
pub struct ControlFlowGraph {
    graph: Graph<usize>,
}

impl ControlFlowGraph {
    #[must_use]
    pub fn build(function: &Function) -> Self {
        let mut this = Self {
            graph: Graph::default(),
        };

        this.visit_function(function);

        this
    }

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

    fn visit_function(&mut self, function: &Function) {
        for instruction_index in 0..function.instructions().len() {
            self.graph.add_node(instruction_index);
        }

        self.graph.add_edge(0, 0);

        self.visit_section(function.instructions(), 0);
    }

    fn visit_section(&mut self, instructions: &[Instruction], mut index: usize) {
        while index < instructions.len() {
            match &instructions[index] {
                Instruction::Jump { location } => {
                    let position = self.position_of_label(instructions, location);
                    if self.graph.add_edge(index, position) {
                        self.visit_section(instructions, position);
                    }
                    return;
                }

                Instruction::JumpConditional { location, .. } => {
                    let position = self.position_of_label(instructions, location);
                    if self.graph.add_edge(index, position) {
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
                self.graph.add_edge(index, next);
            }

            index += 1;
        }
    }

    #[must_use]
    pub fn is_instruction_reachable(&self, index: usize) -> bool {
        self.graph.has_edges_to(&index)
    }
}
