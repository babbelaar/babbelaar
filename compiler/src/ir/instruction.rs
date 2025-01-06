// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::{Display, Write};

use babbelaar::{BabString, Comparison, MathOperator};

use crate::DataSectionOffset;

use super::{FunctionArgument, Operand, Register};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Label {
    pub(super) id: usize,
}

#[cfg(test)]
impl Label {
    #[must_use]
    pub fn new(id: usize) -> Self {
        Self { id }
    }
}

impl Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Etiket")?;
        self.id.fmt(f)
    }
}

impl Label {
    #[must_use]
    pub fn id(&self) -> usize {
        self.id
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    //
    // CPU states
    //

    Compare {
        typ: PrimitiveType,
        lhs: Register,
        rhs: Operand,
    },

    //
    // Loads & stores
    //

    /// Adds `1` to the value of the register.
    Increment {
        typ: PrimitiveType,
        register: Register,
    },

    InitArg {
        destination: Register,
        arg_idx: usize,
    },

    Move {
        destination: Register,
        source: Operand,
    },

    MoveAddress {
        destination: Register,
        offset: DataSectionOffset,
    },

    MoveCondition {
        destination: Register,
        condition: JumpCondition,
    },

    //
    // Control Flow
    //

    Call {
        name: BabString,
        arguments: Vec<FunctionArgument>,
        variable_arguments: Vec<FunctionArgument>,
        ret_val_reg: Option<Register>,
    },

    /// An unconditional jump
    Jump {
        location: Label,
    },

    /// Jump when the condition is met
    JumpConditional {
        condition: JumpCondition,

        /// The location to jump to if the condition was met
        location: Label,
    },

    Label(Label),

    Return {
        value_reg: Option<Register>,
    },

    //
    // Math
    //

    MathOperation {
        typ: PrimitiveType,
        operation: MathOperation,
        destination: Register,
        lhs: Operand,
        rhs: Operand,
    },

    Negate {
        typ: PrimitiveType,
        dst: Register,
        src: Register,
    },

    //
    // Stack Allocation
    //

    StackAlloc {
        dst: Register,
        size: usize,
    },

    LoadPtr {
        destination: Register,
        base_ptr: Register,
        offset: Operand,
        typ: PrimitiveType,
    },

    StorePtr {
        base_ptr: Register,
        offset: Operand,
        value: Register,
        typ: PrimitiveType,
    },
}

impl Instruction {
    #[must_use]
    pub fn destination_register(&self) -> Option<Register> {
        match self {
            Self::Compare { .. } => None,
            Self::Label(..) => None,
            Self::Jump { .. } => None,
            Self::JumpConditional { .. } => None,
            Self::Return { .. } => None,
            Self::StorePtr { .. } => None,

            Self::Increment { register, .. } => Some(*register),
            Self::InitArg { destination, .. } => Some(*destination),
            Self::Move { destination, .. } => Some(*destination),
            Self::MoveAddress { destination, .. } => Some(*destination),
            Self::MoveCondition { destination, .. } => Some(*destination),
            Self::MathOperation { destination, .. } => Some(*destination),
            Self::Negate { dst, .. } => Some(*dst),
            Self::StackAlloc { dst, .. } => Some(*dst),
            Self::LoadPtr { destination, .. } => Some(*destination),
            Self::Call { ret_val_reg, .. } => *ret_val_reg,
        }
    }

    /// TODO: investigate if the use of a Vec here is slow.
    #[must_use]
    pub fn source_registers(&self) -> Vec<Register> {
        let mut result = Vec::new();

        match self {
            Self::Compare { lhs, rhs, typ } => {
                _ = typ;
                result.push(*lhs);
                if let Operand::Register(rhs) = rhs {
                    result.push(*rhs);
                }
            }

            Self::Increment { register, typ } => {
                _ = register;
                _ = typ;
            }

            Self::InitArg { destination, arg_idx } => {
                _ = arg_idx;
                _ = destination;
            }

            Self::Move { destination, source } => {
                _ = destination;

                if let Operand::Register(source) = source {
                    result.push(*source);
                }
            }

            Self::MoveAddress { destination, offset } => {
                _ = offset;
                _ = destination;
            }

            Self::MoveCondition { destination, condition } => {
                _ = condition;
                _ = destination;
            }

            Self::Call { name, arguments, variable_arguments, ret_val_reg } => {
                _ = name;

                for arg in arguments {
                    result.push(arg.register());
                }

                for arg in variable_arguments {
                    result.push(arg.register());
                }

                _ = ret_val_reg;
            }

            Self::Jump { location } => {
                _ = location;
            }

            Self::JumpConditional { condition, location } => {
                _ = condition;
                _ = location;
            }

            Self::Label(label) => {
                _ = label;
            }

            Self::Return { value_reg } => {
                if let Some(value_reg) = value_reg {
                    result.push(*value_reg);
                }
            }

            Self::MathOperation { operation, typ, destination, lhs, rhs } => {
                _ = operation;
                _ = typ;
                _ = destination;

                if let Operand::Register(lhs) = lhs {
                    result.push(*lhs);
                }

                if let Operand::Register(rhs) = rhs {
                    result.push(*rhs);
                }
            }

            Self::Negate { typ, dst, src } => {
                _ = typ;
                _ = dst;
                result.push(*src);
            }

            Self::StackAlloc { dst, size } => {
                _ = dst;
                _ = size;
            }

            Self::LoadPtr { destination, base_ptr, offset, typ } => {
                _ = typ;
                _ = destination;
                result.push(*base_ptr);

                if let Operand::Register(offset) = offset {
                    result.push(*offset);
                }
            }

            Self::StorePtr { base_ptr, offset, value, typ } => {
                _ = typ;

                result.push(*base_ptr);

                if let Operand::Register(offset) = offset {
                    result.push(*offset);
                }

                result.push(*value);
            }
        }

        result
    }

    #[must_use]
    pub fn is_call(&self) -> bool {
        matches!(self, Self::Call { .. })
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Compare { lhs, rhs, typ } => {
                f.write_fmt(format_args!("Vergelijk {typ}, {lhs}, {rhs}"))
            }

            Instruction::Move { source, destination } => {
                f.write_str("Verplaats ")?;
                destination.fmt(f)?;
                f.write_str(", ")?;
                source.fmt(f)
            }

            Instruction::MoveAddress { destination, offset } => {
                f.write_str("Verplaats ")?;
                destination.fmt(f)?;
                f.write_str(", ")?;
                offset.fmt(f)
            }

            Instruction::MoveCondition { destination, condition } => {
                f.write_str("VerplaatsVoorwaarde ")?;
                destination.fmt(f)?;
                f.write_str(", ")?;
                condition.fmt(f)
            }

            Instruction::Call { name, arguments, variable_arguments, ret_val_reg } => {
                f.write_str("RoepAap ")?;
                if let Some(ret_val_reg) = ret_val_reg {
                    ret_val_reg.fmt(f)?;
                }
                f.write_fmt(format_args!(", {name}"))?;

                for arg in arguments {
                    f.write_str(", ")?;
                    arg.register().fmt(f)?;
                }

                for (idx, arg) in variable_arguments.iter().enumerate() {
                    f.write_str(", ")?;

                    if idx == 0 {
                        f.write_str("flexArgs: ")?;
                    }

                    arg.register().fmt(f)?;
                }

                Ok(())
            }

            Instruction::Increment { register, typ } => {
                f.write_fmt(format_args!("Verhoog {typ}, {register}"))
            }

            Instruction::InitArg { destination, arg_idx } => {
                f.write_fmt(format_args!("InitArg {destination}, Arg{arg_idx}"))
            }

            Instruction::Jump { location } => {
                f.write_str("Spring ")?;
                location.fmt(f)
            }

            Instruction::JumpConditional { condition, location } => {
                f.write_str("SpringBij")?;
                condition.fmt(f)?;
                f.write_char(' ')?;
                location.fmt(f)
            }

            Instruction::Label(label) => label.fmt(f),

            Instruction::Return { value_reg } => {
                f.write_str("Bekeer")?;

                if let Some(value_reg) = value_reg {
                    f.write_char(' ')?;
                    value_reg.fmt(f)?;
                }

                Ok(())
            }

            Instruction::MathOperation { operation, typ, destination, lhs, rhs } => {
                operation.fmt(f)?;
                f.write_char(' ')?;

                typ.fmt(f)?;
                f.write_str(", ")?;

                destination.fmt(f)?;
                f.write_str(", ")?;

                lhs.fmt(f)?;
                f.write_str(", ")?;

                rhs.fmt(f)?;

                Ok(())
            }

            Instruction::Negate { typ, dst, src } => {
                f.write_fmt(format_args!("KeerNegatief {typ}, {dst}, {src}"))
            }

            Instruction::StackAlloc { dst, size } => {
                f.write_fmt(format_args!("StapelAllocatie {dst}, #{size}"))
            }

            Instruction::LoadPtr { destination, base_ptr, offset, typ } => {
                f.write_fmt(format_args!("HaalOp {destination}, {base_ptr} + {offset} ({typ} bytes)"))
            }

            Instruction::StorePtr { base_ptr, offset, value, typ } => {
                f.write_fmt(format_args!("SlaOp {base_ptr} + {offset}, {value} ({typ} bytes)"))
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JumpCondition {
    /// Jump if lhs == rhs
    Equal,

    /// Jump if lhs > rhs
    Greater,

    /// Jump if lhs >= rhs
    GreaterOrEqual,

    /// Jump if lhs < rhs
    Less,

    /// Jump if lhs <= rhs
    LessOrEqual,

    /// Jump if lhs != rhs
    NotEqual,
}

impl JumpCondition {
    #[must_use]
    pub const fn name(&self) -> &'static str {
        match self {
            Self::Equal => "Gelijk",
            Self::Greater => "Groter",
            Self::GreaterOrEqual => "GroterOfGelijk",
            Self::Less => "Kleiner",
            Self::LessOrEqual => "KleinerOfGelijk",
            Self::NotEqual => "Ongelijk",
        }
    }
}

impl Display for JumpCondition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name())
    }
}

impl From<Comparison> for JumpCondition {
    fn from(value: Comparison) -> Self {
        match value {
            Comparison::Equality => JumpCondition::Equal,
            Comparison::GreaterThan => JumpCondition::Greater,
            Comparison::GreaterThanOrEqual => JumpCondition::GreaterOrEqual,
            Comparison::Inequality => JumpCondition::NotEqual,
            Comparison::LessThan => JumpCondition::Less,
            Comparison::LessThanOrEqual => JumpCondition::LessOrEqual,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MathOperation {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    LeftShift,
    RightShift,
    Xor,
}

impl MathOperation {
    #[must_use]
    pub const fn name(&self) -> &'static str {
        match self {
            Self::Add => "TelOp",
            Self::Subtract => "TrekAf",
            Self::Multiply => "Vermenigvuldig",
            Self::Divide => "DeelDoor",
            Self::Modulo => "Modulo",
            Self::LeftShift => "SchuifLinks",
            Self::RightShift => "SchuifRechts",
            Self::Xor => "ExclusieveOf",
        }
    }
}

impl From<MathOperator> for MathOperation {
    fn from(value: MathOperator) -> Self {
        match value {
            MathOperator::Add => MathOperation::Add,
            MathOperator::Subtract => MathOperation::Subtract,
            MathOperator::Multiply => MathOperation::Multiply,
            MathOperator::Divide => MathOperation::Divide,
            MathOperator::Modulo => MathOperation::Modulo,
            MathOperator::LeftShift => MathOperation::LeftShift,
            MathOperator::RightShift => MathOperation::RightShift,

            _ => todo!("Ondersteun {value:?} als wiskundige operator in de compileerder"),
        }
    }
}

impl Display for MathOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.name().fmt(f)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PrimitiveType {
    signed: bool,
    bytes: usize,
}

impl PrimitiveType {
    #[must_use]
    pub const fn new(bytes: usize, signed: bool) -> Self {
        Self {
            signed,
            bytes,
        }
    }

    #[must_use]
    pub const fn is_signed(&self) -> bool {
        self.signed
    }

    #[must_use]
    pub const fn bytes(&self) -> usize {
        self.bytes
    }

    #[must_use]
    pub const fn is_arm_64_bit(&self) -> bool {
        self.bytes >= 8
    }
}

impl Display for PrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_signed() {
            f.write_char('s')?;
        } else {
            f.write_char('u')?;
        }
        self.bytes.fmt(f)
    }
}
