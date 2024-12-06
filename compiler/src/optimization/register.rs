// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::collections::HashMap;

use crate::{Function, FunctionOptimizer, Immediate, Instruction, MathOperation, Operand, Register};

#[derive(Debug, Default)]
pub struct RegisterInliner {
    values: HashMap<Register, Immediate>,
}

impl FunctionOptimizer for RegisterInliner {
    fn optimize(&mut self, function: &mut Function) {
        for instruction in function.instructions.iter_mut() {
            match &instruction {
                Instruction::Call { ret_val_reg, .. } => {
                    // we don't know the return value of the register, so after
                    // this point, we can't make assumptions about the data.
                    self.values.remove(ret_val_reg);
                }

                Instruction::Compare { lhs, rhs } => {
                    let lhs = lhs.clone();

                    let Operand::Register(register) = rhs else { continue };
                    let Some(rhs) = self.values.get(register) else { continue };
                    let rhs = Operand::Immediate(*rhs);

                    *instruction = Instruction::Compare { lhs, rhs };
                }

                Instruction::Increment { register } => {
                    if let Some(value) = self.values.get(register) {
                        self.values.insert(register.clone(), Immediate::Integer64(value.as_i64() + 1));
                    }
                }

                Instruction::Jump { location } => {
                    _ = location;
                    self.values.clear();
                }

                Instruction::JumpConditional { condition, location } => {
                    _ = condition;
                    _ = location;
                    self.values.clear();
                }

                Instruction::Label(..) => (),

                Instruction::Move { source, destination } => {
                    match source {
                        Operand::Register(source) => {
                            if let Some(known_value) = self.values.get(source).cloned() {
                                self.values.insert(destination.clone(), known_value.clone());
                                let destination = destination.clone();
                                *instruction = Instruction::Move {
                                    source: Operand::Immediate(known_value),
                                    destination,
                                };
                            } else {
                                self.values.remove(destination);
                            }
                        }

                        Operand::Immediate(immediate) => {
                            self.values.insert(destination.clone(), immediate.clone());
                        }
                    }
                }

                Instruction::MoveAddress { destination, offset } => {
                    // We *can* optimize if we have some more magical value (not just immediate)
                    // that encompasses the fact that we have stored a pointer to the data section
                    // here.
                    _ = offset;

                    self.values.remove(destination);
                }

                Instruction::MoveCondition { destination, condition } => {
                    // We *can* optimize if we keep track of the condition variable.
                    _ = condition;
                    self.values.remove(destination);
                }

                Instruction::MathOperation { operation, destination, lhs, rhs } => {
                    *instruction = self.calculate_math_operation(*operation, *destination, lhs, rhs);
                }

                Instruction::Negate { dst, src } => {
                    let Some(val) = self.values.get(src) else { continue };

                    let immediate = match val {
                        Immediate::Integer8(i) => Immediate::Integer8(-i),
                        Immediate::Integer16(i) => Immediate::Integer16(-i),
                        Immediate::Integer32(i) => Immediate::Integer32(-i),
                        Immediate::Integer64(i) => Immediate::Integer64(-i),
                    };

                    self.values.insert(*dst, immediate);

                    *instruction = Instruction::Move { source: Operand::Immediate(immediate), destination: *dst };
                }

                Instruction::Return { .. } => {

                }

                Instruction::StackAlloc { dst, size } => {
                    _ = size;
                    self.values.remove(dst);
                }

                Instruction::LoadPtr { destination, base_ptr, offset, typ: size } => {
                    self.values.remove(destination);
                    // TODO: add known values
                    _ = base_ptr;
                    _ = offset;
                    _ = size;
                }

                Instruction::StorePtr { base_ptr, offset, value, typ: size } => {
                    self.values.remove(base_ptr);

                    let offset = self.try_inline_operand(offset);

                    *instruction = Instruction::StorePtr {
                        base_ptr: *base_ptr,
                        offset,
                        value: *value,
                        typ: *size,
                    };
                }
            }
        }
    }
}

impl RegisterInliner {
    #[must_use]
    fn resolve_operand_to_immediate(&self, operand: &Operand) -> Option<Immediate> {
        match operand {
            Operand::Immediate(immediate) => Some(immediate.clone()),
            Operand::Register(register) => self.values.get(register).cloned(),
        }
    }

    #[must_use]
    fn try_inline_operand(&self, operand: &Operand) -> Operand {
        if let Some(immediate) = self.resolve_operand_to_immediate(operand) {
            Operand::Immediate(immediate)
        } else {
            operand.clone()
        }
    }

    fn calculate_math_operation(&mut self, operation: MathOperation, destination: Register, lhs: &Operand, rhs: &Operand) -> Instruction {
        let lhs = self.try_inline_operand(lhs);
        let rhs = self.try_inline_operand(rhs);

        match (lhs, rhs) {
            (Operand::Immediate(lhs), Operand::Immediate(rhs)) => {
                return Instruction::Move {
                    destination,
                    source: Operand::Immediate(self.calculate_math_operation_using_known_immediates(operation, destination, lhs, rhs))
                };
            }

            (Operand::Register(_), Operand::Immediate(rhs)) => {
                if let Some(instruction) = self.calculate_math_operation_using_math_rules(operation, destination, lhs, rhs) {
                    return instruction;
                }
            }

            _ => (),
        }

        Instruction::MathOperation {
            operation,
            destination,
            lhs,
            rhs,
        }
    }

    fn calculate_math_operation_using_known_immediates(&mut self, operation: MathOperation, destination: Register, lhs: Immediate, rhs: Immediate) -> Immediate {
        // TODO: is it necessary to honor the bit size of the
        //       integer (wrapping at that boundary), or would
        //       the CPU also overflow?
        let value = match operation {
            MathOperation::Add => {
                Immediate::Integer64(lhs.as_i64().wrapping_add(rhs.as_i64()))
            }

            MathOperation::Multiply => {
                Immediate::Integer64(lhs.as_i64().wrapping_mul(rhs.as_i64()))
            }

            MathOperation::Subtract => {
                Immediate::Integer64(lhs.as_i64().wrapping_sub(rhs.as_i64()))
            }

            MathOperation::Divide => {
                Immediate::Integer64(lhs.as_i64().wrapping_div(rhs.as_i64()))
            }

            MathOperation::Modulo => {
                Immediate::Integer64(lhs.as_i64().wrapping_rem(rhs.as_i64()))
            }
        };

        self.values.insert(destination.clone(), value);
        value
    }

    fn calculate_math_operation_using_math_rules(&self, operation: MathOperation, destination: Register, lhs: Operand, rhs: Immediate) -> Option<Instruction> {
        if rhs.as_i64() == 0 {
            if operation == MathOperation::Add || operation == MathOperation::Subtract || operation == MathOperation::Modulo {
                return Some(Instruction::Move {
                    destination,
                    source: lhs,
                });
            }

            if operation == MathOperation::Multiply {
                return Some(Instruction::Move {
                    destination,
                    source: Operand::Immediate(Immediate::Integer64(0)),
                });
            }
        }

        if rhs.as_i64() == 1 {
            if operation == MathOperation::Multiply || operation == MathOperation::Divide {
                return Some(Instruction::Move {
                    destination,
                    source: lhs,
                });
            }
        }

        None
    }
}
