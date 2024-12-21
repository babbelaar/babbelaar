// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::ops::Range;

use log::debug;

use crate::{Graph, Instruction, Label};

#[derive(Debug, Clone)]
pub struct ControlFlowGraph {
    graph: Graph<usize>,
    loops: Vec<Range<usize>>,
}

impl ControlFlowGraph {
    #[must_use]
    pub fn new(instructions: &[Instruction]) -> Self {
        let mut this = Self {
            graph: Graph::default(),
            loops: Vec::new(),
        };

        this.visit_function(instructions);

        this
    }

    pub fn loop_ranges(&self) -> &[Range<usize>] {
        &self.loops
    }

    #[must_use]
    pub fn is_instruction_reachable(&self, index: usize) -> bool {
        self.graph.has_edges_to(&index)
    }

    #[allow(unused)]
    pub fn dump(&self) {
        debug!("Loops of ControlFlowGraph:");
        for loop_range in &self.loops {
            debug!("    From {} to {}", loop_range.start, loop_range.end);
        }
    }
}

// private methods
impl ControlFlowGraph {
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

    fn visit_function(&mut self, instructions: &[Instruction]) {
        for instruction_index in 0..instructions.len() {
            self.graph.add_node(instruction_index);
        }

        self.graph.add_edge(0, 0);

        self.visit_section(instructions, 0);
        self.analyze_loops();
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

    fn analyze_loops(&mut self) {
        for edge in self.graph.edges() {
            if edge.to() < edge.from() {
                self.loops.push(edge.to()..edge.from());
            }
        }
    }
}
