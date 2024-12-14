// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::HashMap, rc::Rc};

use babbelaar::{BabString, Builtin, BuiltinType, Expression, Structure, Type};

use crate::PrimitiveType;

#[derive(Debug, Clone)]
pub struct FieldLayout {
    offset: usize,

    /// How much space is actually used for the field
    size: usize,

    /// How much should the offset advance to the next field. Might be different
    /// from the `size` when it is e.g. aligned.
    stride: usize,

    default_value_expression: Option<Rc<Expression>>,

    type_id: TypeId,
}

impl FieldLayout {
    #[must_use]
    pub fn default_value_expression(&self) -> Option<&Rc<Expression>> {
        self.default_value_expression.as_ref()
    }

    #[must_use]
    pub const fn offset(&self) -> usize {
        self.offset
    }

    #[must_use]
    pub const fn size(&self) -> usize {
        self.size
    }

    #[must_use]
    pub const fn stride(&self) -> usize {
        self.stride
    }

    #[must_use]
    pub const fn type_id(&self) -> TypeId {
        self.type_id
    }

    pub fn primitive_type(&self) -> PrimitiveType {
        PrimitiveType::new(self.size, true)
    }
}

/// Specifies how a structure is laid out in memory.
#[derive(Debug, Clone)]
pub struct StructureLayout {
    name: BabString,
    type_id: TypeId,
    size: usize,
    fields: Vec<FieldLayout>,
    field_names: HashMap<BabString, usize>,
}

impl StructureLayout {
    #[must_use]
    pub fn name(&self) -> &BabString {
        &self.name
    }

    #[must_use]
    pub const fn size(&self) -> usize {
        self.size
    }

    #[must_use]
    pub fn fields(&self) -> &[FieldLayout] {
        &self.fields
    }

    #[must_use]
    pub fn field(&self, name: &BabString) -> &FieldLayout {
        let Some(index) = self.field_names.get(name).copied() else {
            panic!("ICE: ongeldige veldnaam gegeven: `{name}`!")
        };

        &self.fields[index]
    }

    #[must_use]
    pub fn type_id(&self) -> &TypeId {
        &self.type_id
    }

    fn add_field(&mut self, field_name: BabString, field: FieldLayout) {
        self.field_names.insert(field_name, self.fields.len());
        self.fields.push(field);
    }

    #[must_use]
    pub fn primitive_type(&self) -> PrimitiveType {
        PrimitiveType::new(self.size, true)
    }
}

#[derive(Debug, Clone, Default)]
pub struct TypeManager {
    types: Vec<StructureLayout>,
    type_names: HashMap<BabString, usize>,
}

impl TypeManager {
    #[must_use]
    pub fn new() -> Self {
        let mut this = Self::default();
        this.add_primitives();
        this.add_type_slinger();
        this
    }

    pub fn add_structure(&mut self, structure: &Structure) {
        let mut layout = StructureLayout {
            name: structure.name.value().clone(),
            type_id: TypeId {
                index: self.types.len(),
            },
            size: 0,
            fields: Vec::new(),
            field_names: HashMap::new(),
        };

        let mut offset = 0;
        for ast_field in &structure.fields {
            let size = self.size_of(&ast_field.ty);
            let type_id = self.layout_of(&ast_field.ty.specifier.unqualified_name()).type_id;

            let field = FieldLayout {
                offset,
                size,
                stride: size,
                default_value_expression: ast_field.default_value.as_ref().map(|x| Rc::new(x.value().clone())),
                type_id,
            };

            offset += field.stride;

            layout.add_field(ast_field.name.value().clone(), field);
        }

        layout.size = offset;

        self.add_type(layout);
    }

    #[must_use]
    pub fn size_of(&self, ty: &Type) -> usize {
        assert!(ty.qualifiers.is_empty());

        self.layout_of(&ty.specifier.unqualified_name()).size
    }

    pub fn layout(&self, ty: TypeId) -> &StructureLayout {
        &self.types[ty.index]
    }

    pub fn layout_of(&self, name: &BabString) -> &StructureLayout {
        let Some(index) = self.type_names.get(name).copied() else {
            panic!("ICE: ongeldige typenaam gegeven: `{name}`!")
        };

        &self.types[index]
    }

    #[must_use]
    pub const fn pointer_size(&self) -> usize {
        // TODO: support 32-bit applications
        8
    }

    #[must_use]
    pub fn usize_type_info(&self) -> TypeInfo {
        let type_id = match self.pointer_size() {
            1 => TypeId::G8,
            2 => TypeId::G16,
            4 => TypeId::G32,
            8 => TypeId::G64,
            unsupported_size => unimplemented!("Ongeldige wijzergrootte `{unsupported_size}`!"),
        };

        TypeInfo::Plain(type_id)
    }

    #[must_use]
    pub const fn usize_primitive_type(&self) -> PrimitiveType {
        PrimitiveType::new(self.pointer_size(), false)
    }

    #[must_use]
    pub const fn platform_alignment_size(&self) -> usize {
        // TODO: this assumes ARM-based architecture, which requires loads/stores of 4-byte aligned addresses
        4
    }

    fn add_type_slinger(&mut self) {
        let mut layout = StructureLayout {
            name: BuiltinType::Slinger.name(),
            type_id: TypeId::SLINGER,
            fields: Vec::new(),
            field_names: HashMap::new(),
            size: 0,
        };

        let mut offset = 0;
        for field_name in ["start", "end"] {
            let size = self.pointer_size();

            let field = FieldLayout {
                offset,
                size,
                stride: size,
                default_value_expression: None,
                type_id: TypeId::G32,
            };

            offset += field.stride;
            layout.add_field(BabString::new_static(field_name), field);
        }

        layout.size = offset;

        self.add_type(layout);
    }

    fn add_primitives(&mut self) {
        for ty in Builtin::TYPES {
            let size = match ty {
                BuiltinType::Bool => 1,
                BuiltinType::G8 => 1,
                BuiltinType::G16 => 2,
                BuiltinType::G32 => 4,
                BuiltinType::G64 => 8,
                BuiltinType::Teken => 4,

                BuiltinType::Null => continue,
                BuiltinType::Slinger => continue,
            };

            let type_id = match ty {
                BuiltinType::Bool => TypeId::BOOL,
                BuiltinType::G8 => TypeId::G8,
                BuiltinType::G16 => TypeId::G16,
                BuiltinType::G32 => TypeId::G32,
                BuiltinType::G64 => TypeId::G64,
                BuiltinType::Teken => TypeId::TEKEN,
                _ => unreachable!(),
            };

            let layout = StructureLayout {
                name: ty.name(),
                type_id,
                size,
                fields: Vec::new(),
                field_names: HashMap::new(),
            };

            self.add_type(layout);
        }
    }

    fn add_type(&mut self, layout: StructureLayout) {
        self.type_names.insert(layout.name.clone(), layout.type_id.index);
        self.types.insert(layout.type_id.index, layout);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeId {
    index: usize,
}

impl TypeId {
    pub const BOOL: Self = Self { index: 0 };
    pub const G8: Self = Self { index: 1 };
    pub const G16: Self = Self { index: 2 };
    pub const G32: Self = Self { index: 3 };
    pub const G64: Self = Self { index: 4 };
    pub const TEKEN: Self = Self { index: 5 };
    pub const SLINGER: Self = Self { index: 6 };

    #[must_use]
    pub const fn is_integer(&self) -> bool {
        self.index == Self::G8.index || self.index == Self::G16.index || self.index == Self::G32.index || self.index == Self::G64.index
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeInfo {
    Array(Box<TypeInfo>),
    Plain(TypeId),
}

impl TypeInfo {
    #[must_use]
    pub fn type_id(&self) -> TypeId {
        match self {
            Self::Array(info) => info.type_id(),
            Self::Plain(ty) => *ty,
        }
    }
}

impl From<TypeId> for TypeInfo {
    fn from(value: TypeId) -> Self {
        Self::Plain(value)
    }
}
