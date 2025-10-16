// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use core::{slice, str};
use std::ptr::null;

use crate::{BabString, Interpreter, SemanticType, Value, ValueType};

use super::{functions::BuiltinFunctionSignature, BuiltinType};

pub struct BuiltinPointer;

impl BuiltinPointer {
    pub fn methods(&self) -> &'static [PointerMethod] {
        METHODS_ARRAY
    }
}

#[derive(Clone, Copy)]
pub struct PointerMethod {
    pub name: &'static str,
    pub documentation: &'static str,
    pub inline_detail: &'static str,
    pub function: BuiltinFunctionSignature,
    pub lsp_completion: Option<&'static str>,
    pub parameters: &'static [PointerMethodParameter],
    pub return_type: PointerTypeRef,
    pub must_use: bool,
}

impl PointerMethod {
    pub fn lsp_completion(&self) -> BabString {
        match self.lsp_completion {
            Some(completion) => BabString::new_static(&completion),
            None => {
                let mut result = format!("{}(", self.name);

                for (idx, parameter) in self.parameters.iter().enumerate() {
                    result += &format!("${{{index}:{name}}}", index = idx + 1, name = parameter.name);
                }

                result += ")$0";
                BabString::new(result)
            }
        }
    }

    #[must_use]
    pub fn lsp_label(&self) -> String {
        let params = if self.parameters.is_empty() { "" } else { "…" };
        format!("{}({params})", self.name)
    }
}

impl PartialEq for PointerMethod {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

#[derive(Debug)]
pub struct PointerMethodParameter {
    pub name: &'static str,
    pub typ: PointerTypeRef,
}

#[derive(Debug, Clone, Copy)]
pub enum PointerTypeRef {
    Builtin(BuiltinType),
    ElementType,
}

impl PointerTypeRef {
    #[must_use]
    pub const fn bool() -> Self {
        Self::Builtin(BuiltinType::Bool)
    }

    #[must_use]
    pub const fn g32() -> Self {
        Self::Builtin(BuiltinType::G32)
    }

    #[must_use]
    pub fn resolve(&self, typ: SemanticType) -> SemanticType {
        match self {
            Self::Builtin(ty) => SemanticType::Builtin(*ty),
            Self::ElementType => typ,
        }
    }
}

impl From<BuiltinType> for PointerTypeRef {
    fn from(value: BuiltinType) -> Self {
        Self::Builtin(value)
    }
}

pub(super) static METHODS_ARRAY: &'static [PointerMethod] = &[
    PointerMethod {
        name: "krijg",
        documentation: "Geeft de waarde waar de wijzer naartoe wijst terug.",
        inline_detail: "Geeft de waarde waar de wijzer naartoe wijst terug.",
        function: &pointer_krijg,
        lsp_completion: None,
        parameters: &[],
        return_type: PointerTypeRef::ElementType,
        must_use: true,
    },
];

fn pointer_krijg(_: &mut dyn Interpreter, _parameters: Vec<Value>, this: Option<Value>) -> Value {
    let Value::Pointer { address, ty } = this.unwrap() else {
        panic!("Invalid type given");
    };

    match ty {
        ValueType::Array(..) => todo!(),

        ValueType::Builtin(ty) => match ty {
            BuiltinType::Bool => Value::Bool(unsafe { *(address as *const bool) }),
            BuiltinType::G8 => Value::Integer(unsafe { *(address as *const i8) } as i64),
            BuiltinType::G16 => Value::Integer(unsafe { *(address as *const i16) } as i64),
            BuiltinType::G32 => Value::Integer(unsafe { *(address as *const i32) } as i64),
            BuiltinType::G64 => Value::Integer(unsafe { *(address as *const i64) }),
            BuiltinType::Null => Value::Null,
            BuiltinType::Slinger => {
                let start = address as *const u8;

                let mut len = 0;
                let mut end = start;
                while end != null() {
                    let value = unsafe { *end };

                    if value == b'\0' {
                        break;
                    }

                    end = unsafe { end.add(size_of::<*const ()>()) };
                    len += 1;
                }

                let slice = unsafe { slice::from_raw_parts(start, len) };

                Value::String(String::from_utf8_lossy(slice).into_owned())
            },
            BuiltinType::Teken => Value::Character(unsafe { *(address as *const char) }),
        }

        ValueType::Pointer(value_type) => {
            Value::Pointer {
                address: unsafe { *(address as *const *const ()) as usize },
                ty: *value_type,
            }
        }
    }
}
