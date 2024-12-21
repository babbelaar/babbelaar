// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::collections::HashMap;

use crate::{ir::FunctionArgument, ControlFlowGraph, DataSectionOffset, Function, Immediate, Instruction, Operand, Register};

use super::{FunctionOptimizer, OptimizationContext};

#[derive(Debug, Default)]
pub struct StringOptimizer {
    values: HashMap<Register, Value>,
}

impl FunctionOptimizer for StringOptimizer {
    fn optimize(&mut self, _: &mut Function) {}

    fn optimize_with_context(&mut self, function: &mut Function, ctx: &mut OptimizationContext) {
        let cfg = ControlFlowGraph::new(function.instructions());

        for idx in 0..function.instructions.len() {
            if idx >= function.instructions.len() {
                break;
            }

            if cfg.is_start_of_loop(idx) {
                self.values.clear();
            }

            match &function.instructions[idx] {
                Instruction::MoveAddress { destination, offset } => {
                    self.values.insert(*destination, Value {
                        data: Some(*offset)
                    });
                }

                Instruction::Call { name, arguments, ret_val_reg, .. } => {
                    let instr = match name.as_str() {
                        "Slinger__lengte" => {
                            let Some(ret_val_reg) = ret_val_reg else {
                                function.instructions.remove(idx);
                                continue;
                            };

                            self.optimize_slinger_lengte(arguments, *ret_val_reg)
                        }

                        "Slinger__voegSamen" => {
                            let Some(ret_val_reg) = ret_val_reg else {
                                function.instructions.remove(idx);
                                continue;
                            };

                            self.optimize_slinger_voeg_samen(arguments, *ret_val_reg, ctx)
                        }

                        _ => None,
                    };

                    if let Some(instr) = instr {
                        function.instructions[idx] = instr;
                    }
                }

                instr => {
                    if let Some(reg) = instr.destination_register() {
                        self.values.remove(&reg);
                    }
                }
            }
        }
    }
}

impl StringOptimizer {
    fn optimize_slinger_lengte(&mut self, arguments: &[FunctionArgument], reg: Register) -> Option<Instruction> {
        let str_reg = arguments[0].register();

        let value = self.values.get(&str_reg)?;
        let data = value.data.as_ref()?;

        Some(Instruction::Move {
            destination: reg,
            source: Operand::Immediate(Immediate::Integer64(data.size()? as _))
        })
    }

    fn optimize_slinger_voeg_samen(&mut self, arguments: &[FunctionArgument], reg: Register, ctx: &mut OptimizationContext) -> Option<Instruction> {
        let lhs = self.string(arguments.get(0)?.register(), ctx)?;
        let rhs = self.string(arguments.get(1)?.register(), ctx)?;

        let offset = ctx.read_only_data.add_null_terminated_string(&format!("{lhs}{rhs}"));
        self.values.insert(reg, Value {
            data: Some(offset),
        });

        Some(Instruction::MoveAddress {
            destination: reg,
            offset,
        })
    }

    fn string<'o>(&self, reg: Register, ctx: &'o OptimizationContext) -> Option<&'o str> {
        let value = self.values.get(&reg)?;
        let data = value.data.as_ref()?;

        let start = data.offset();
        let end = start + data.size()?;

        let bytes = &ctx.read_only_data.data()[start..end];
        Some(std::str::from_utf8(bytes).expect("ongeldige slinger"))
    }
}

#[derive(Debug, Default, Clone)]
struct Value {
    data: Option<DataSectionOffset>,
}
