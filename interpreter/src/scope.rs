// Copyright (C) 2023 - 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::collections::HashMap;

use babbelaar::{BabString, InterfaceId, StructureId, ValueType};

use crate::{Builtin, FunctionId, Value};

#[derive(Default, Debug)]
pub struct Scope {
    pub parent: Option<Box<Scope>>,
    pub variables: HashMap<BabString, Value>,
    pub structures: HashMap<BabString, StructureId>,
    pub interfaces: HashMap<BabString, InterfaceId>,
    pub generic_types: HashMap<BabString, ValueType>,
    pub this: Option<Value>,
}

impl Scope {
    pub fn new(this: Option<Value>) -> Self {
        Self {
            parent: None,
            variables: HashMap::new(),
            structures: HashMap::new(),
            interfaces: HashMap::new(),
            generic_types: HashMap::new(),
            this,
        }
    }

    pub fn new_top_level() -> Self {
        let mut this = Self::new(None);

        for (func_idx, func) in Builtin::FUNCTIONS.iter().enumerate() {
            let id = FunctionId {
                namespace: usize::MAX,
                id: func_idx,
            };
            this.variables.insert(BabString::new_static(func.name), Value::Function { name: func.name.to_string(), id });
        }

        for ty in Builtin::TYPES {
            this.structures.insert(ty.name(), StructureId::from(*ty));
        }

        this
    }

    pub fn push(self) -> Self {
        let this = self.this.clone();
        Self {
            parent: Some(Box::new(self)),
            variables: HashMap::new(),
            structures: HashMap::new(),
            interfaces: HashMap::new(),
            generic_types: HashMap::new(),
            this,
        }
    }

    pub fn push_function(self, this: Option<Value>) -> Self {
        Self {
            parent: Some(Box::new(self)),
            variables: HashMap::new(),
            structures: HashMap::new(),
            interfaces: HashMap::new(),
            generic_types: HashMap::new(),
            this,
        }
    }

    pub fn pop(self) -> Self {
        *self.parent.expect("Top-level scope popped!")
    }

    pub fn find(&self, reference: &BabString) -> Value {
        if let Some(value) = self.variables.get(reference) {
            return value.clone();
        }

        if let Some(parent) = self.parent.as_ref() {
            return parent.find(reference);
        }

        Value::Null
    }

    pub fn find_mut(&mut self, reference: &BabString) -> Option<&mut Value> {
        if let Some(value) = self.variables.get_mut(reference) {
            return Some(value);
        }

        if let Some(parent) = self.parent.as_mut() {
            return parent.find_mut(reference);
        }

        None
    }

    pub fn find_generic_type(&self, name: &BabString) -> Option<ValueType> {
        if let Some(ty) = self.generic_types.get(name) {
            return Some(ty.clone());
        }

        if let Some(parent) = &self.parent {
            return parent.find_generic_type(name);
        }

        None
    }

    pub fn find_structure_id(&self, name: &BabString) -> Option<StructureId> {
        if let Some(id) = self.structures.get(name) {
            return Some(*id);
        }

        if let Some(parent) = self.parent.as_ref() {
            return parent.find_structure_id(name);
        }

        None
    }

    pub fn find_interface_id(&self, name: &BabString) -> Option<InterfaceId> {
        if let Some(id) = self.interfaces.get(name) {
            return Some(*id);
        }

        if let Some(parent) = self.parent.as_ref() {
            return parent.find_interface_id(name);
        }

        None
    }

    pub fn overwrite(&mut self, reference: &BabString, new: Value) -> bool {
        if let Some(value) = self.variables.get_mut(reference) {
            *value = new;
            return true;
        }

        if let Some(parent) = self.parent.as_mut() {
            return parent.overwrite(reference, new);
        }

        false
    }
}
