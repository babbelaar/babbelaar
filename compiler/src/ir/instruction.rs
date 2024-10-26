// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::{Display, Write};

use babbelaar::BabString;

use super::{Immediate, Operand, Register};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Label {
    pub(super) id: usize,
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

#[derive(Debug, Clone)]
pub enum Instruction {
    //
    // CPU states
    //

    Compare {
        lhs: Register,
        rhs: Operand,
    },

    //
    // Loads & stores
    //

    LoadImmediate {
        immediate: Immediate,
        destination_reg: Register,
    },

    Move {
        source: Register,
        destination: Register,
    },

    //
    // Control Flow
    //

    Call {
        name: BabString,
        arguments: Vec<Register>,
        ret_val_reg: Register,
    },

    /// An unconditional jump
    Jump {
        location: Label,
    },

    /// Jump only if the comparison was equal
    JumpIfEqual {
        location: Label,
    },

    /// Jump only if the comparison was not equal
    JumpIfNotEqual {
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
        operation: MathOperation,
        destination: Register,
        lhs: Operand,
        rhs: Operand,
    },
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Compare { lhs, rhs } => {
                f.write_str("Vergelijk ")?;
                lhs.fmt(f)?;
                f.write_str(", ")?;
                rhs.fmt(f)
            }

            Instruction::LoadImmediate { immediate, destination_reg } => {
                f.write_str("Laad ")?;
                destination_reg.fmt(f)?;
                f.write_str(", ")?;
                immediate.fmt(f)
            }

            Instruction::Move { source, destination } => {
                f.write_str("Verplaats")?;
                destination.fmt(f)?;
                f.write_str(", ")?;
                source.fmt(f)
            }

            Instruction::Call { name, arguments, ret_val_reg } => {
                f.write_fmt(format_args!("RoepAan {ret_val_reg}, {name}"))?;
                for arg in arguments {
                    f.write_str(", ")?;
                    arg.fmt(f)?;
                }

                Ok(())
            }

            Instruction::Jump { location } => {
                f.write_str("Spring ")?;
                location.fmt(f)
            }

            Instruction::JumpIfEqual { location } => {
                f.write_str("SpringBijGelijkheid ")?;
                location.fmt(f)
            }

            Instruction::JumpIfNotEqual { location } => {
                f.write_str("SpringBijOngelijkheid ")?;
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

            Instruction::MathOperation { operation, destination, lhs, rhs } => {
                operation.fmt(f)?;
                f.write_char(' ')?;

                destination.fmt(f)?;
                f.write_str(", ")?;

                lhs.fmt(f)?;
                f.write_str(", ")?;

                rhs.fmt(f)?;

                Ok(())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MathOperation {
    Add,
    Subtract,
}

impl MathOperation {
    #[must_use]
    pub const fn name(&self) -> &'static str {
        match self {
            Self::Add => "TelOp",
            Self::Subtract => "TrekAf",
        }
    }
}

impl Display for MathOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.name().fmt(f)
    }
}
