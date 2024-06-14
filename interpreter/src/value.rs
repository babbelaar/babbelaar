// Copyright (C) 2023 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{cmp::Ordering, fmt::Display};

use crate::{Builtin, BuiltinType, Comparison};

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    /// hehe 5 billion dollar problem
    Null,

    Bool(bool),
    Integer(i64),
    String(String),
}
impl Value {
    #[must_use]
    pub fn is_true(&self) -> bool {
        matches!(self, Self::Bool(true))
    }

    #[must_use]
    pub fn compare(&self, other: &Self, comparison: Comparison) -> bool {
        let Some(ordering) = self.partial_cmp(other) else {
            return false;
        };

        match comparison {
            Comparison::Equality => ordering == Ordering::Equal,
            Comparison::Inequality => ordering != Ordering::Equal,
            Comparison::GreaterThan => ordering == Ordering::Greater,
            Comparison::GreaterThanOrEqual => ordering != Ordering::Less,
            Comparison::LessThan => ordering == Ordering::Less,
            Comparison::LessThanOrEqual => ordering != Ordering::Greater,
        }
    }

    pub fn typ(&self) -> &'static BuiltinType {
        match self {
            Self::Bool(..) => &Builtin::TYPE_BOOL,
            Self::Integer(..) => &Builtin::TYPE_G32,
            Self::Null => &Builtin::TYPE_NULL,
            Self::String(..) => &Builtin::TYPE_SLINGER,
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Self::Null, Self::Null) => Some(Ordering::Equal),
            (Self::Bool(this), Self::Bool(that)) => Some(this.cmp(that)),
            (Self::Integer(this), Self::Integer(that)) => Some(this.cmp(that)),
            (Self::String(this), Self::String(that)) => Some(this.cmp(that)),
            _ => None,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Null => f.write_str("null"),
            Self::Bool(b) => b.fmt(f),
            Self::Integer(i) => i.fmt(f),
            Self::String(str) => f.write_str(str),
        }
    }
}
