// Copyright (C) 2023 - 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{cell::RefCell, cmp::Ordering, collections::HashMap, fmt::Display, hash::{DefaultHasher, Hash, Hasher}, rc::Rc};

use crate::{BuiltinFunction, BuiltinType, Comparison, FunctionStatement, Structure};

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
    MethodIdReference {
        lhs: Box<Value>,
        method: MethodId,
    },
    Function {
        name: String,
        id: FunctionId,
    },
    Object {
        structure: StructureId,
        fields: Rc<RefCell<HashMap<String, Value>>>,
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

    pub fn typ(&self) -> ValueType {
        match self {
            Self::Bool(..) => BuiltinType::Bool.into(),
            Self::Integer(..) => BuiltinType::G32.into(),
            Self::Null => BuiltinType::Null.into(),
            Self::String(..) => BuiltinType::Slinger.into(),
            Self::MethodReference { .. } => todo!(),
            Self::MethodIdReference { .. } => todo!(),
            Self::Function { .. } => todo!(),
            Self::Object { structure, .. } => structure.into(),
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
            Self::Bool(false) => f.write_str("onwaar"),
            Self::Bool(true) => f.write_str("waar"),
            Self::Integer(i) => i.fmt(f),
            Self::String(str) => f.write_str(str),
            Self::MethodReference { lhs, method } => f.write_fmt(format_args!("{lhs}.{}()", method.name)),
            Self::MethodIdReference { .. } => f.write_str("werkwijze"),
            Self::Function { name, .. } => f.write_fmt(format_args!("werkwijze {name}() {{ .. }}")),
            Self::Object { .. } => f.write_str("te-doen(object-waarde-formatteren)"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub enum ValueType {
    Builtin(BuiltinType),
    Structure(StructureId),
}

impl From<BuiltinType> for ValueType {
    fn from(value: BuiltinType) -> Self {
        Self::Builtin(value)
    }
}

impl From<&BuiltinType> for ValueType {
    fn from(value: &BuiltinType) -> Self {
        Self::Builtin(*value)
    }
}

impl From<&StructureId> for ValueType {
    fn from(value: &StructureId) -> Self {
        Self::Structure(*value)
    }
}

impl From<StructureId> for ValueType {
    fn from(value: StructureId) -> Self {
        Self::Structure(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId {
    pub namespace: usize,
    pub id: usize,
}

impl From<&FunctionStatement> for FunctionId {
    fn from(value: &FunctionStatement) -> Self {
        let mut hasher = DefaultHasher::new();
        "Function-".hash(&mut hasher);
        value.name.value().hash(&mut hasher);
        Self {
            namespace: 0,
            id: hasher.finish() as usize,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StructureId {
    pub namespace: usize,
    pub id: usize,
}

impl From<&Structure> for StructureId {
    fn from(value: &Structure) -> Self {
        let mut hasher = DefaultHasher::new();
        "Structure-".hash(&mut hasher);
        value.name.value().hash(&mut hasher);
        Self {
            namespace: 0,
            id: hasher.finish() as usize,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MethodId {
    pub structure: StructureId,
    pub index: usize,
}
