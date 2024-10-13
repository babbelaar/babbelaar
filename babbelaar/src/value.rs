// Copyright (C) 2023 - 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{borrow::Cow, cell::RefCell, cmp::Ordering, collections::HashMap, fmt::{Display, Write}, hash::{DefaultHasher, Hash, Hasher}, rc::Rc};

use crate::{BabString, BuiltinMethodReference, BuiltinType, Comparison, FunctionStatement, Structure};

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    /// hehe 5 billion dollar problem
    Null,

    Array {
        ty: ValueType,
        values: Rc<RefCell<Vec<Value>>>,
    },

    ArrayElementReference {
        array: Rc<RefCell<Vec<Value>>>,
        index: usize,
    },

    Bool(bool),
    Integer(i64),
    String(String),
    Character(char),
    MethodReference {
        lhs: Box<Value>,
        method: BuiltinMethodReference,
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
        generic_types: HashMap<BabString, ValueType>,
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
            Self::Array{ ty, .. } => ValueType::Array(Box::new(ty.clone())),
            Self::ArrayElementReference { array, index } => array.borrow()[*index].typ(),
            Self::Bool(..) => BuiltinType::Bool.into(),
            Self::Integer(..) => BuiltinType::G32.into(),
            Self::Null => BuiltinType::Null.into(),
            Self::String(..) => BuiltinType::Slinger.into(),
            Self::Character(..) => BuiltinType::Teken.into(),
            Self::MethodReference { .. } => todo!(),
            Self::MethodIdReference { .. } => todo!(),
            Self::Function { .. } => todo!(),
            Self::Object { structure, generic_types, .. } => ValueType::Structure(*structure, generic_types.clone()),
        }
    }

    #[must_use]
    pub fn actual_value(&self) -> Cow<'_, Value> {
        match self {
            Self::ArrayElementReference { array, index } => {
                Cow::Owned(array.borrow()[*index].clone())
            }
            _ => Cow::Borrowed(self),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let this = self.actual_value();
        let that = other.actual_value();

        match (this.as_ref(), that.as_ref()) {
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
            Self::Array { values, .. } => {
                f.write_str("[")?;

                for (idx, val) in values.borrow().iter().enumerate() {
                    if idx != 0 {
                        f.write_str(", ")?;
                    }

                    val.fmt(f)?;
                }

                f.write_str("]")
            }
            Self::ArrayElementReference { array, index } => array.borrow()[*index].fmt(f),
            Self::Null => f.write_str("null"),
            Self::Bool(false) => f.write_str("onwaar"),
            Self::Bool(true) => f.write_str("waar"),
            Self::Integer(i) => i.fmt(f),
            Self::String(str) => f.write_str(str),
            Self::Character(c) => f.write_char(*c),
            Self::MethodReference { lhs, method } => f.write_fmt(format_args!("{lhs}.{}()", method.name())),
            Self::MethodIdReference { .. } => f.write_str("werkwijze"),
            Self::Function { name, .. } => f.write_fmt(format_args!("werkwijze {name}() {{ .. }}")),
            Self::Object { .. } => f.write_str("te-doen(object-waarde-formatteren)"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueType {
    Array(Box<ValueType>),
    Builtin(BuiltinType),
    Structure(StructureId, HashMap<BabString, ValueType>),
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

impl From<BuiltinType> for StructureId {
    fn from(value: BuiltinType) -> Self {
        Self{
            namespace: 1,
            id: value as u8 as usize,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MethodId {
    pub structure: StructureId,
    pub index: usize,
}
