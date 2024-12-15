// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::Display;

use crate::{BabString, FileRange, PrimaryExpression, Ranged};

pub type AttributeList = Vec<Ranged<Attribute>>;

#[derive(Debug, Clone)]
pub struct Attribute {
    pub at_range: FileRange,
    pub name: Ranged<AttributeName>,
    pub arguments: Ranged<Vec<AttributeArgument>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AttributeName {
    Unknown(BabString),
    Extern,
    VarArgs,
}

impl AttributeName {
    const NAME_EXTERN: &'static str = "uitheems";
    const NAME_VAR_ARGS: &'static str = "flexibeleArgumenten";

    #[must_use]
    pub fn as_str(&self) -> BabString {
        match self {
            Self::Unknown(s) => s.clone(),
            Self::Extern => BabString::new_static(Self::NAME_EXTERN),
            Self::VarArgs => BabString::new_static(Self::NAME_EXTERN),
        }
    }
}

impl From<BabString> for AttributeName {
    fn from(value: BabString) -> Self {
        match value.as_str() {
            Self::NAME_EXTERN => Self::Extern,
            Self::NAME_VAR_ARGS => Self::VarArgs,
            _ => Self::Unknown(value),
        }
    }
}

impl Display for AttributeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_str().fmt(f)
    }
}

#[derive(Debug, Clone)]
pub struct AttributeArgument {
    pub name: Ranged<BabString>,
    pub value: Ranged<PrimaryExpression>,
}


impl Display for Attribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("@")?;
        self.name.fmt(f)?;

        if !self.arguments.is_empty() {
            f.write_str("(")?;

            for (idx, arg) in self.arguments.iter().enumerate() {
                if idx != 0 {
                    f.write_str(", ")?;
                }

                arg.fmt(f)?;
            }

            f.write_str(")")?;
        }

        Ok(())
    }
}

impl Display for AttributeArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)?;
        f.write_str(": ")?;
        self.value.fmt(f)
    }
}
