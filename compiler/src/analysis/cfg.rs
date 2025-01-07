// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::ops::Range;

use log::debug;

use crate::{Graph, Label, TargetInstruction};

#[derive(Debug, Clone)]
pub struct ControlFlowGraph {
    graph: Graph<usize>,
    loops: Vec<Range<usize>>,
}

impl ControlFlowGraph {
    #[must_use]
    pub fn new<I: TargetInstruction>(instructions: &[I]) -> Self {
        let mut this = Self {
            graph: Graph::default(),
            loops: Vec::new(),
        };

        this.visit_function(instructions);

        this
    }

    #[must_use]
    pub fn loop_ranges(&self) -> &[Range<usize>] {
        &self.loops
    }

    #[must_use]
    pub fn is_start_of_loop(&self, idx: usize) -> bool {
        self.loops.iter().find(|r| r.start == idx).is_some()
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
    fn position_of_label<I: TargetInstruction>(&mut self, instructions: &[I], label: Label) -> usize {
        for (index, instruction) in instructions.iter().enumerate() {
            if let Some(l) = instruction.as_label() {
                if l == label {
                    return index;
                }
            }
        }

        panic!("Illegal label {label:?}")
    }

    fn visit_function<I: TargetInstruction>(&mut self, instructions: &[I]) {
        for instruction_index in 0..instructions.len() {
            self.graph.add_node(instruction_index);
        }

        self.graph.add_edge(0, 0);

        self.visit_section(instructions, 0);
        self.analyze_loops();
    }

    fn visit_section<I: TargetInstruction>(&mut self, instructions: &[I], mut index: usize) {
        while index < instructions.len() {
            if let Some(branch) = instructions[index].branch_info() {
                if let Some(location) = branch.target_location() {
                    let position = self.position_of_label(instructions, location);
                    if self.graph.add_edge(index, position) {
                        self.visit_section(instructions, position);
                    }
                }

                if branch.is_unconditional() {
                    return;
                }
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
