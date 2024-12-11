// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::Display;

use crate::{BabString, FileRange, PrimaryExpression, Ranged};

pub type AttributeList = Vec<Ranged<Attribute>>;

#[derive(Debug, Clone)]
pub struct Attribute {
    pub at_range: FileRange,
    pub name: Ranged<BabString>,
    pub arguments: Ranged<Vec<AttributeArgument>>,
}

impl Attribute {
    pub const NAME_EXTERN: &'static str = "uitheems";
    pub const NAME_VAR_ARGS: &'static str = "flexibeleArgumenten";
}

#[derive(Debug, Clone)]
pub struct AttributeArgument {
    pub name: Ranged<BabString>,
    pub value: Ranged<PrimaryExpression>,
}


impl Display for Attribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("@")?;
        f.write_str(&self.name)?;

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
