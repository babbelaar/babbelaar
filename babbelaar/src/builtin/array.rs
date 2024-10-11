// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{BabString, Interpreter, SemanticType, Value};

use super::{functions::BuiltinFunctionSignature, BuiltinType};

pub struct BuiltinArray;

impl BuiltinArray {
    pub fn methods(&self) -> &'static [ArrayMethod] {
        METHODS_ARRAY
    }
}

#[derive(Clone, Copy)]
pub struct ArrayMethod {
    pub name: &'static str,
    pub documentation: &'static str,
    pub inline_detail: &'static str,
    pub function: BuiltinFunctionSignature,
    pub lsp_completion: Option<&'static str>,
    pub parameters: &'static [ArrayMethodParameter],
    pub return_type: ArrayTypeRef,
    pub must_use: bool,
}

impl ArrayMethod {
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

impl PartialEq for ArrayMethod {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

#[derive(Debug)]
pub struct ArrayMethodParameter {
    pub name: &'static str,
    pub typ: ArrayTypeRef,
}

#[derive(Debug, Clone, Copy)]
pub enum ArrayTypeRef {
    Builtin(BuiltinType),
    ElementType,
}

impl ArrayTypeRef {
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

impl From<BuiltinType> for ArrayTypeRef {
    fn from(value: BuiltinType) -> Self {
        Self::Builtin(value)
    }
}

pub(super) static METHODS_ARRAY: &'static [ArrayMethod] = &[
    ArrayMethod {
        name: "isLeeg",
        documentation: "Geeft `waar` terug als de opeenvolging leeg is, anders `onwaar`.",
        inline_detail: "Bevat de opeenvolging elementen.",
        function: &array_is_leeg,
        lsp_completion: None,
        parameters: &[],
        return_type: ArrayTypeRef::bool(),
        must_use: true,
    },
    ArrayMethod {
        name: "lengte",
        documentation: "Krijg de lengte van de opeenvolging.\n## Voorbeeld\n```babbelaar\nstel getallen = nieuw g32[5];\ngetallen.lengte() // = 5\n```",
        inline_detail: "Krijg de lengte van de opeenvolging.",
        function: &array_lengte,
        lsp_completion: None,
        parameters: &[],
        return_type: ArrayTypeRef::g32(),
        must_use: true,
    },
    ArrayMethod {
        name: "bevat",
        documentation: "Controleer of een element in de opeenvolging zit.\n## Voorbeeld\n```babbelaar\nstel getallen = nieuw g32[5];\ngetallen.bevat(123) // = onwaar\ngetallen[0] = 123;\ngetallen.bevat(123) // = waar\n```",
        inline_detail: "Controleer of een element in de opeenvolging zit.",
        function: &array_bevat,
        lsp_completion: None,
        parameters: &[],
        return_type: ArrayTypeRef::bool(),
        must_use: true,
    },
];

fn array_lengte(_: &mut dyn Interpreter, _parameters: Vec<Value>, this: Option<Value>) -> Value {
    let Value::Array { values, .. } = this.unwrap() else {
        panic!("Invalid type given");
    };
    let values = values.borrow();
    Value::Integer(values.len() as _)
}

fn array_bevat(_: &mut dyn Interpreter, parameters: Vec<Value>, this: Option<Value>) -> Value {
    let Value::Array { values, .. } = this.unwrap() else {
        panic!("Invalid type given");
    };
    let values = values.borrow();
    Value::Bool(values.contains(&parameters[0]))
}

fn array_is_leeg(_: &mut dyn Interpreter, _parameters: Vec<Value>, this: Option<Value>) -> Value {
    let Value::Array { values, .. } = this.unwrap() else {
        panic!("Invalid type given");
    };
    let values = values.borrow();
    Value::Bool(values.is_empty())
}
