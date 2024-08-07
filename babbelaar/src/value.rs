// Copyright (C) 2023 - 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{cmp::Ordering, fmt::Display, hash::{DefaultHasher, Hash, Hasher}};

use crate::{BuiltinFunction, BuiltinType, Comparison, FunctionStatement};

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    /// hehe 5 billion dollar problem
    Null,

    Bool(bool),
    Integer(i64),
    String(String),
    MethodReference {
        lhs: Box<Value>,
        method: &'static BuiltinFunction,
    },
    Function {
        name: String,
        id: FunctionId,
    },
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

    pub fn typ(&self) -> BuiltinType {
        match self {
            Self::Bool(..) => BuiltinType::Bool,
            Self::Integer(..) => BuiltinType::G32,
            Self::Null => BuiltinType::Null,
            Self::String(..) => BuiltinType::Slinger,
            Self::MethodReference { .. } => todo!(),
            Self::Function { .. } => todo!(),
        }
    }

    pub fn get_method(&self, method_name: &str) -> Option<Value> {
        for method in self.typ().methods().iter() {
            if method.name == method_name {
                return Some(Value::MethodReference {
                    lhs: Box::new(self.clone()),
                    method,
                });
            }
        }

        None
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
            Self::Bool(false) => f.write_str("onwaar"),
            Self::Bool(true) => f.write_str("waar"),
            Self::Integer(i) => i.fmt(f),
            Self::String(str) => f.write_str(str),
            Self::MethodReference { lhs, method } => f.write_fmt(format_args!("{lhs}.{}()", method.name)),
            Self::Function { name, .. } => f.write_fmt(format_args!("functie {name}() {{ .. }}")),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId {
    pub namespace: usize,
    pub id: usize,
}

impl<'source_code> From<&FunctionStatement<'source_code>> for FunctionId {
    fn from(value: &FunctionStatement<'source_code>) -> Self {
        let mut hasher = DefaultHasher::new();
        value.name.value().hash(&mut hasher);
        Self {
            namespace: 0,
            id: hasher.finish() as usize,
        }
    }
}
