// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{BabString, FileRange, IntoBabString};

use super::{FunctionReference, SemanticLocalKind, SemanticType};

#[derive(Debug, Clone, PartialEq)]
pub struct SemanticReference {
    pub local_name: BabString,
    pub local_kind: SemanticLocalKind,
    pub declaration_range: FileRange,
    pub typ: SemanticType,
}

impl SemanticReference {
    pub fn function_name(&self) -> BabString {
        match &self.typ {
            SemanticType::Array(..) => todo!(),
            SemanticType::Builtin(..) => todo!(),
            SemanticType::Custom { .. } => todo!(),
            SemanticType::Function(func) => BabString::clone(&func.name),
            SemanticType::FunctionReference(func) => func.name(),
            SemanticType::IndexReference(..) => todo!(),
            SemanticType::Interface { .. } => todo!(),
            SemanticType::Generic(..) => todo!(),
            SemanticType::Pointer(..) => todo!(),
        }
    }

    pub fn documentation(&self) -> Option<BabString> {
        match &self.typ {
            SemanticType::Array(..) => None,
            SemanticType::Builtin(builtin) => Some(builtin.documentation().into_bab_string()),
            SemanticType::Custom { .. } => None,
            SemanticType::Function(..) => None,
            SemanticType::FunctionReference(func) => func.documentation(),
            SemanticType::IndexReference(..) => None,
            SemanticType::Interface { .. } => None,
            SemanticType::Generic(..) => None,
            SemanticType::Pointer(..) => None,
        }
    }

    pub fn inline_detail(&self) -> Option<BabString> {
        match &self.typ {
            SemanticType::Array(..) => None,
            SemanticType::Builtin(builtin) => Some(builtin.inline_detail()),
            SemanticType::Custom { .. } => None,
            SemanticType::Function(..) => None,
            SemanticType::FunctionReference(func) => func.inline_detail(),
            SemanticType::IndexReference(..) => None,
            SemanticType::Interface { .. } => None,
            SemanticType::Generic(..) => None,
            SemanticType::Pointer(..) => None,
        }
    }

    pub fn lsp_completion(&self) -> BabString {
        match &self.typ {
            SemanticType::Array(ty) => format!("{}[]", ty.name()).into(),
            SemanticType::Builtin(builtin) => builtin.name(),
            SemanticType::Custom { .. } => self.typ.to_string().into(),
            SemanticType::Function(func) => BabString::new(format!("{}($1);$0", func.name.value())),
            SemanticType::FunctionReference(func) => func.lsp_completion(),
            SemanticType::IndexReference(..) => BabString::empty(),
            SemanticType::Interface { .. } => BabString::empty(),
            SemanticType::Generic(..) => BabString::empty(),
            SemanticType::Pointer(ty) => format!("{}*", ty.name()).into(),
        }
    }

    pub fn hover(&self) -> String {
        match self.local_kind {
            SemanticLocalKind::Function | SemanticLocalKind::FunctionReference => {
                format!("werkwijze {}(..)\n```", self.local_name)
            }

            SemanticLocalKind::Method => {
                let mut str = format!("werkwijze {}(", self.local_name);

                if let SemanticType::FunctionReference(func) = &self.typ {
                    match func {
                        FunctionReference::Custom(custom) => {
                            for (idx, param) in custom.parameters.iter().enumerate() {
                                if idx != 0 {
                                    str += ", ";
                                }
                                str += &param.name;
                                str += ": ";
                                str += &param.ty.to_string();
                            }
                        }
                        FunctionReference::Builtin(..) => str += "..",
                    }
                }

                str += ")";
                str
            }

            SemanticLocalKind::FieldReference => {
                format!("veld {}: {}", self.local_name, self.typ)
            }

            SemanticLocalKind::StructureReference => {
                let mut fields = String::new();

                let typ = match &self.typ {
                    SemanticType::Array(item) => &*item,
                    SemanticType::Pointer(item) => &*item,
                    other => other,
                };

                if let SemanticType::Custom { base: typ, .. } = typ {
                    for field in &typ.fields {
                        fields += &format!("\n    veld {}: {}", field.name.value(), field.ty);
                    }

                    for method in &typ.methods {
                        fields += &format!("\n    werkwijze {}(..) {{ /* ... */ }}", method.function.name.value());
                    }
                }

                format!("structuur {} {{{fields}\n}}", typ.to_string())
            }

            SemanticLocalKind::Variable => {
                format!("stel {}: {}", self.local_name, self.typ)
            }

            _ => {
                format!("{}: {}", self.local_name, self.typ)
            }
        }
    }

    #[must_use]
    pub fn return_value(&self) -> SemanticType {
        match &self.typ {
            SemanticType::Function(f) => {
                SemanticType::clone(&f.return_type)
            }

            SemanticType::FunctionReference(FunctionReference::Builtin(builtin)) => {
                SemanticType::Builtin(builtin.return_type)
            }

            SemanticType::FunctionReference(FunctionReference::Custom(f)) => {
                SemanticType::clone(&f.return_type)
            }

            _ => SemanticType::null()
        }
    }

    #[must_use]
    pub fn has_variable_arguments(&self) -> bool {
        match &self.typ {
            SemanticType::Function(func) => {
                func.has_variable_arguments
            }

            SemanticType::FunctionReference(FunctionReference::Custom(func)) => {
                func.has_variable_arguments
            }

            _ => false,
        }
    }
}
