// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{Function, Immediate, Instruction, MathOperation, Operand, Register};

use super::FunctionOptimizer;

#[derive(Default, Debug)]
pub struct StrengthReductor {

}

impl FunctionOptimizer for StrengthReductor {
    fn optimize(&mut self, function: &mut Function) {
        for instruction in &mut function.instructions {
            match instruction {
                Instruction::MathOperation { operation, destination, lhs, rhs } => {
                    if let Some(instr) = self.optimize_math_op(*operation, *destination, *lhs, *rhs) {
                        *instruction = instr;
                    }
                }

                _ => (),
            }
        }
    }
}

impl StrengthReductor {
    fn optimize_math_op(&self, operation: MathOperation, destination: Register, lhs: Operand, rhs: Operand) -> Option<Instruction> {
        let Operand::Immediate(rhs) = rhs else {
            return None;
        };

        let rhs = rhs.as_i64();

        match operation {
            MathOperation::Multiply => {
                if rhs == 0 {
                    return Some(Instruction::Move { destination, source: Operand::Immediate(Immediate::Integer64(0)) });
                }

                if rhs < 0 {
                    // TODO: we can do more stuff here!!
                    return None;
                }

                if rhs % 2 == 0 {
                    let power_of_two = rhs.ilog2();

                    Some(Instruction::MathOperation {
                        operation: MathOperation::LeftShift,
                        destination,
                        lhs,
                        rhs: Operand::Immediate(Immediate::Integer64(power_of_two as _))
                    })
                } else {
                    None
                }
            }

            MathOperation::Divide => {
                // TODO: based on unsigned or signed arithmetic, we can optimize powers of two here...
                None
            }

            _ => None
        }
    }
}
