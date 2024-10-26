// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

//! Interpreter for IR instructions.

use std::collections::HashMap;

use babbelaar::BabString;

use crate::{Immediate, Instruction, Label, MathOperation, Operand, Program, Register};

pub struct Interpreter {
    program: Program,
    comparison_flags: ComparisonFlags,
    stack_frames: Vec<StackFrame>,
    label_positions: HashMap<(usize, Label), usize>,
}

impl Interpreter {
    #[must_use]
    pub fn new(program: Program) -> Self {
        let label_positions = collect_label_positions(&program);
        Self {
            program,
            comparison_flags: Default::default(),
            stack_frames: Vec::new(),
            label_positions,
        }
    }

    pub fn execute_function(&mut self, name: &BabString, arguments: Vec<Immediate>) -> Option<Immediate> {
        let function_index = self.program.function_index_by_symbol(name).unwrap();
        self.execute_function_by_index(function_index, arguments)
    }

    #[must_use]
    fn execute_function_by_index(&mut self, function_index: usize, arguments: Vec<Immediate>) -> Option<Immediate> {
        let func = self.program.function(function_index);

        let mut frame = StackFrame::new();

        for (reg, value) in func.argument_registers().iter().zip(arguments.into_iter()) {
            frame.registers.insert(*reg, value);
        }

        self.stack_frames.push(frame);

        let return_value;

        loop {
            match self.execute_instruction(function_index) {
                OperationResult::Continue => {
                    self.frame().program_counter += 1;
                }

                OperationResult::Jump(label) => {
                    let position = self.label_positions.get(&(function_index, label)).unwrap();
                    self.frame().program_counter = *position;
                }

                OperationResult::Return(value) => {
                    return_value = value;
                    break;
                }
            }
        }

        _ = self.stack_frames.pop();

        return_value
    }

    #[must_use]
    fn register(&mut self, register: &Register) -> Immediate {
        self.frame().registers.get(register).unwrap().clone()
    }

    #[must_use]
    fn operand_to_immediate(&mut self, operand: &Operand) -> Immediate {
        match operand {
            Operand::Immediate(immediate) => immediate.clone(),
            Operand::Register(register) => self.register(register),
        }
    }

    #[must_use]
    fn execute_instruction(&mut self, function_index: usize) -> OperationResult {
        let program_counter = self.frame().program_counter;

        match self.program.function(function_index).instructions()[program_counter].clone() {
            Instruction::Call { name, arguments, ret_val_reg } => {
                let function_index = self.program.function_index_by_symbol(&name).unwrap();

                let frame = self.frame();

                let arguments = arguments.iter()
                    .map(|reg| frame.registers.get(reg).unwrap().clone())
                    .collect();

                let return_value = self.execute_function_by_index(function_index, arguments);

                if let Some(return_value) = return_value {
                    self.frame().registers.insert(ret_val_reg, return_value);
                }

                OperationResult::Continue
            }

            Instruction::Compare { lhs, rhs } => {
                let lhs = self.register(&lhs);
                let rhs = self.operand_to_immediate(&rhs);

                // TODO: honor the size of the immediate (this matters for e.g. carry, overflow)
                let lhs = lhs.as_i64();
                let rhs = rhs.as_i64();

                let overflow = lhs.checked_sub(rhs).is_none();
                let value = lhs.wrapping_sub(rhs);

                self.comparison_flags = ComparisonFlags {
                    negative: value < 0,
                    zero: value == 0,
                    carry: false,
                    overflow,
                };

                OperationResult::Continue
            }

            Instruction::Increment { register } => {
                let value = Immediate::Integer64(self.register(&register).as_i64() + 1);
                self.frame().registers.insert(register, value);
                OperationResult::Continue
            }

            Instruction::Jump { location } => {
                OperationResult::Jump(location)
            }

            Instruction::JumpIfEqual { location } => {
                if self.comparison_flags.zero {
                    OperationResult::Jump(location)
                } else {
                    OperationResult::Continue
                }
            }

            Instruction::JumpIfGreater { location } => {
                if !self.comparison_flags.negative {
                    OperationResult::Jump(location)
                } else {
                    OperationResult::Continue
                }
            }

            Instruction::JumpIfGreaterOrEqual { location } => {
                if !self.comparison_flags.negative || self.comparison_flags.zero {
                    OperationResult::Jump(location)
                } else {
                    OperationResult::Continue
                }
            }

            Instruction::JumpIfLess { location } => {
                if self.comparison_flags.negative {
                    OperationResult::Jump(location)
                } else {
                    OperationResult::Continue
                }
            }

            Instruction::JumpIfLessOrEqual { location } => {
                if self.comparison_flags.negative || self.comparison_flags.zero {
                    OperationResult::Jump(location)
                } else {
                    OperationResult::Continue
                }
            }

            Instruction::JumpIfNotEqual { location } => {
                if !self.comparison_flags.zero {
                    OperationResult::Jump(location)
                } else {
                    OperationResult::Continue
                }
            }

            Instruction::Label(..) => OperationResult::Continue,

            Instruction::LoadImmediate { immediate, destination_reg } => {
                let register = destination_reg.clone();
                let value = immediate.clone();

                self.frame().registers.insert(register, value);

                OperationResult::Continue
            }

            Instruction::Move { source, destination } => {
                let value = self.frame().registers.get(&source).unwrap().clone();
                self.frame().registers.insert(destination, value);

                OperationResult::Continue
            }

            Instruction::Return { value_reg} => {
                match value_reg {
                    Some(register) => {
                        let Some(value) = self.frame().registers.get(&register) else {
                            panic!("Kan niet bekeren met register {register}, want deze heeft geen waarde!");
                        };

                        OperationResult::Return(Some(value.clone()))
                    }

                    None => OperationResult::Return(None)
                }
            }

            Instruction::MathOperation { operation, destination, lhs, rhs } => {
                let lhs = self.operand_to_immediate(&lhs);
                let rhs = self.operand_to_immediate(&rhs);

                let value = match operation {
                    MathOperation::Add => Immediate::Integer64(lhs.as_i64() + rhs.as_i64()),
                    MathOperation::Subtract => Immediate::Integer64(lhs.as_i64() - rhs.as_i64()),
                };

                self.frame().registers.insert(destination, value);
                OperationResult::Continue
            }
        }
    }

    fn frame(&mut self) -> &mut StackFrame {
        self.stack_frames.last_mut().unwrap()
    }
}

struct StackFrame {
    registers: HashMap<Register, Immediate>,
    program_counter: usize,
}

impl StackFrame {
    #[must_use]
    pub fn new() -> Self {
        Self {
            registers: HashMap::new(),
            program_counter: 0,
        }
    }
}

#[derive(Debug, Clone)]
enum OperationResult {
    Continue,
    Return(Option<Immediate>),
    Jump(Label),
}

#[derive(Debug, Clone, Default)]
#[allow(unused)]
struct ComparisonFlags {
    negative: bool,
    zero: bool,
    carry: bool,
    overflow: bool,
}

#[must_use]
fn collect_label_positions(program: &Program) -> HashMap<(usize, Label), usize> {
    let mut map = HashMap::new();

    for (func_id, func) in program.functions().iter().enumerate() {
        for (position, instruction) in func.instructions().iter().enumerate() {
            if let Instruction::Label(label) = instruction {
                map.insert((func_id, *label), position);
            }
        }
    }

    map
}

#[cfg(test)]
mod tests {
    use babbelaar::BabString;

    use crate::{Immediate, ProgramBuilder};

    use super::Interpreter;

    #[test]
    fn test_only_return() {
        let function_name = BabString::new_static("func1");

        let mut program = ProgramBuilder::new();
        program.build_function(function_name.clone(), |f| {
            f.ret();
        });
        let program = program.build();

        let mut interpreter = Interpreter::new(program);
        let return_value = interpreter.execute_function(&function_name, Vec::new());

        assert_eq!(return_value, None);
    }

    #[test]
    fn test_return_with_value() {
        let function_name = BabString::new_static("func1");

        let mut program = ProgramBuilder::new();
        program.build_function(function_name.clone(), |f| {
            let value = f.load_immediate(Immediate::Integer32(1234));
            f.ret_with(value);
        });
        let program = program.build();

        let mut interpreter = Interpreter::new(program);
        let return_value = interpreter.execute_function(&function_name, Vec::new());

        assert_eq!(return_value, Some(Immediate::Integer32(1234)));
    }
}