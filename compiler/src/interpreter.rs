// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

//! Interpreter for IR instructions.

use std::collections::HashMap;

use babbelaar::BabString;

use crate::{DataSectionKind, Immediate, Instruction, JumpCondition, Label, MathOperation, Operand, Program, Register};

pub struct Interpreter {
    program: Program,
    comparison_flags: ComparisonFlags,
    stack: Vec<u8>,
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
            stack: Vec::new(),
            stack_frames: Vec::new(),
            label_positions,
        }
    }

    pub fn execute_function(&mut self, name: &BabString, arguments: Vec<Immediate>) -> Option<Immediate> {
        if let Some(result) = self.execute_builtin_function(name, &arguments) {
            return result;
        }

        let function_index = self.program.function_index_by_symbol(name).unwrap();
        self.execute_function_by_index(function_index, arguments)
    }

    #[must_use]
    fn execute_function_by_index(&mut self, function_index: usize, arguments: Vec<Immediate>) -> Option<Immediate> {
        let func = self.program.function(function_index);

        let mut frame = StackFrame::new(self.stack.len());

        for (reg, value) in func.argument_registers().iter().zip(arguments.into_iter()) {
            frame.set_register(*reg, value);
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

        let frame = self.stack_frames.pop().unwrap();
        assert!(self.stack.len() >= frame.stack_offset);
        self.stack.resize(frame.stack_offset, 0);

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
            Instruction::Call { name, arguments, variable_arguments, ret_val_reg } => {
                let frame = self.frame();

                let arguments = arguments.iter()
                    .chain(variable_arguments.iter())
                    .map(|arg| frame.registers.get(&arg.register()).unwrap().clone())
                    .collect();

                let return_value = self.execute_function(&name, arguments);

                if let Some(return_value) = return_value {
                    if let Some(ret_val_reg) = ret_val_reg {
                        self.frame().set_register(ret_val_reg, return_value);
                    }
                }

                OperationResult::Continue
            }

            Instruction::Compare { typ, lhs, rhs } => {
                let lhs = self.register(&lhs);
                let rhs = self.operand_to_immediate(&rhs);

                _ = typ;

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

            Instruction::Increment { register, typ } => {
                _ = typ;
                let value = Immediate::Integer64(self.register(&register).as_i64() + 1);
                self.frame().set_register(register, value);
                OperationResult::Continue
            }

            Instruction::InitArg { .. } => OperationResult::Continue,

            Instruction::Jump { location } => {
                OperationResult::Jump(location)
            }

            Instruction::JumpConditional { condition, location } => {
                let is_true = match condition {
                    JumpCondition::Equal => {
                        self.comparison_flags.zero
                    }

                    JumpCondition::Greater => {
                        !self.comparison_flags.negative
                    }

                    JumpCondition::GreaterOrEqual => {
                        !self.comparison_flags.negative || self.comparison_flags.zero
                    }

                    JumpCondition::Less => {
                        self.comparison_flags.negative
                    }

                    JumpCondition::LessOrEqual => {
                        self.comparison_flags.negative || self.comparison_flags.zero
                    }

                    JumpCondition::NotEqual => {
                        !self.comparison_flags.zero
                    }
                };

                if is_true {
                    OperationResult::Jump(location)
                } else {
                    OperationResult::Continue
                }
            }

            Instruction::Label(..) => OperationResult::Continue,

            Instruction::Move { source, destination } => {
                let value = match source {
                    Operand::Immediate(immediate) => {
                        immediate
                    }

                    Operand::Register(source) => {
                        self.frame().registers.get(&source).unwrap().clone()
                    }
                };

                self.frame().set_register(destination, value);

                OperationResult::Continue
            }

            Instruction::MoveAddress { destination, offset } => {
                let section = match offset.section_kind() {
                    DataSectionKind::ReadOnly => self.program.read_only_data(),
                };

                let ptr = section.data().as_ptr();
                let ptr = Immediate::Integer64(ptr as i64 + offset.offset() as i64);
                self.frame().set_register(destination, ptr);

                OperationResult::Continue
            }

            Instruction::MoveCondition { destination, condition } => {
                let is_true = match condition {
                    JumpCondition::Equal => {
                        self.comparison_flags.zero
                    }

                    JumpCondition::Greater => {
                        !self.comparison_flags.negative
                    }

                    JumpCondition::GreaterOrEqual => {
                        !self.comparison_flags.negative || self.comparison_flags.zero
                    }

                    JumpCondition::Less => {
                        self.comparison_flags.negative
                    }

                    JumpCondition::LessOrEqual => {
                        self.comparison_flags.negative || self.comparison_flags.zero
                    }

                    JumpCondition::NotEqual => {
                        !self.comparison_flags.zero
                    }
                };

                self.frame().registers.insert(destination, Immediate::Integer8(is_true as _));
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

            Instruction::MathOperation { operation, typ: _, destination, lhs, rhs } => {
                let lhs = self.operand_to_immediate(&lhs);
                let rhs = self.operand_to_immediate(&rhs);

                let value = match operation {
                    MathOperation::Add => Immediate::Integer64(lhs.as_i64() + rhs.as_i64()),
                    MathOperation::Subtract => Immediate::Integer64(lhs.as_i64() - rhs.as_i64()),
                    MathOperation::Multiply => Immediate::Integer64(lhs.as_i64() * rhs.as_i64()),
                    MathOperation::Divide => Immediate::Integer64(lhs.as_i64() / rhs.as_i64()),
                    MathOperation::Modulo => Immediate::Integer64(lhs.as_i64() % rhs.as_i64()),
                    MathOperation::LeftShift => Immediate::Integer64(lhs.as_i64() << rhs.as_i64()),
                    MathOperation::RightShift => Immediate::Integer64(lhs.as_i64() >> rhs.as_i64()),
                    MathOperation::Xor => Immediate::Integer64(lhs.as_i64() ^ rhs.as_i64()),
                };

                self.frame().set_register(destination, value);
                OperationResult::Continue
            }

            Instruction::Negate { typ: _, dst, src } => {
                let value = match self.register(&src) {
                    Immediate::Integer8(i) => Immediate::Integer8(-i),
                    Immediate::Integer16(i) => Immediate::Integer16(-i),
                    Immediate::Integer32(i) => Immediate::Integer32(-i),
                    Immediate::Integer64(i) => Immediate::Integer64(-i),
                };

                self.frame().set_register(dst, value);
                OperationResult::Continue
            }

            Instruction::StackAlloc { dst, size } => {
                let offset = self.stack.len();
                self.stack.extend(std::iter::repeat_n(0, size));

                self.frame().set_register(dst, Immediate::Integer64(offset as _));

                OperationResult::Continue
            }

            Instruction::LoadPtr { destination, base_ptr, offset, typ } => {
                let base_ptr = self.register(&base_ptr).as_i64();
                let offset = self.operand_to_immediate(&offset).as_i64();

                let ptr = self.stack.as_ptr() as i64 + base_ptr + offset;

                match typ.bytes() {
                    1 => {
                        let ptr = ptr as *const i8;
                        let value = unsafe { ptr.read() } as i8;

                        self.frame().set_register(destination, Immediate::Integer8(value));
                    }

                    2 => {
                        let ptr = ptr as *const i16;
                        let value = unsafe { ptr.read() } as i16;

                        self.frame().set_register(destination, Immediate::Integer16(value));
                    }

                    4 => {
                        let ptr = ptr as *const i32;
                        let value = unsafe { ptr.read() } as i32;

                        self.frame().set_register(destination, Immediate::Integer32(value));
                    }

                    8 => {
                        let ptr = ptr as *const i64;
                        let value = unsafe { ptr.read() } as i64;

                        self.frame().set_register(destination, Immediate::Integer64(value));
                    }

                    _ => todo!("Primitieve grootten van {} bytes zijn niet ondersteund!", typ.bytes())
                }

                OperationResult::Continue
            }

            Instruction::StorePtr { base_ptr, offset, value, typ } => {
                let base_ptr = self.register(&base_ptr).as_i64();
                let offset = self.operand_to_immediate(&offset).as_i64();

                let ptr = self.stack.as_ptr() as i64 + base_ptr + offset;

                match typ.bytes() {
                    1 => {
                        let ptr = ptr as *mut i8;
                        let value = self.register(&value).as_i8();
                        unsafe { ptr.write(value as _) };
                    }

                    2 => {
                        let ptr = ptr as *mut i16;
                        let value = self.register(&value).as_i16();
                        unsafe { ptr.write(value as _) };
                    }

                    4 => {
                        let ptr = ptr as *mut i32;
                        let value = self.register(&value).as_i32();
                        unsafe { ptr.write(value as _) };
                    }

                    8 => {
                        let ptr = ptr as *mut i64;
                        let value = self.register(&value).as_i64();
                        unsafe { ptr.write(value as _) };
                    }

                    _ => todo!("Primitieve grootten van {} bytes zijn niet ondersteund!", typ.bytes())
                }

                OperationResult::Continue
            }
        }
    }

    fn frame(&mut self) -> &mut StackFrame {
        self.stack_frames.last_mut().unwrap()
    }

    fn execute_builtin_function(&mut self, name: &BabString, arguments: &[Immediate]) -> Option<Option<Immediate>> {
        match name.as_str() {
            "Slinger__lengte" => {
                // let ptr = arguments[0].as_i64() as _;
                // let result = unsafe { babbelaar_builtin::strlen(ptr) };
                // Some(Some(Immediate::Integer64(result as _)))
                todo!()
            }

            "schrijf" => {
                // let ptr = arguments[0].as_i64() as _;
                // unsafe { babbelaar_builtin::schrijf(ptr) };
                // Some(None)
                todo!()
            }

            _ => None,
        }
    }
}

impl babbelaar::Interpreter for Interpreter {

}

struct StackFrame {
    registers: HashMap<Register, Immediate>,
    program_counter: usize,
    stack_offset: usize,
}

impl StackFrame {
    #[must_use]
    pub fn new(stack_offset: usize) -> Self {
        Self {
            registers: HashMap::new(),
            program_counter: 0,
            stack_offset,
        }
    }

    #[inline]
    fn set_register(&mut self, register: Register, value: Immediate) {
        self.registers.insert(register, value);
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

    use crate::{ArgumentList, Immediate, ProgramBuilder};

    use super::Interpreter;

    #[test]
    fn test_only_return() {
        let function_name = BabString::new_static("func1");

        let mut program = ProgramBuilder::new();
        program.build_function(function_name.clone(), ArgumentList::new(), |f| {
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
        program.build_function(function_name.clone(), ArgumentList::new(), |f| {
            let value = f.load_immediate(Immediate::Integer32(1234));
            f.ret_with(value);
        });
        let program = program.build();

        let mut interpreter = Interpreter::new(program);
        let return_value = interpreter.execute_function(&function_name, Vec::new());

        assert_eq!(return_value, Some(Immediate::Integer32(1234)));
    }
}
