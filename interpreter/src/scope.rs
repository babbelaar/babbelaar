// Copyright (C) 2023 - 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::HashMap, rc::Rc};

use babbelaar::Structure;

use crate::{Builtin, FunctionId, Value};

#[derive(Default, Debug)]
pub struct Scope {
    pub parent: Option<Box<Scope>>,
    pub variables: HashMap<String, Value>,
    pub structures: HashMap<String, Rc<Structure>>,
    pub this: Option<Value>,
}

impl Scope {
    pub fn new(this: Option<Value>) -> Self {
        Self {
            parent: None,
            variables: HashMap::new(),
            structures: HashMap::new(),
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
            this.variables.insert(func.name.to_string(), Value::Function { name: func.name.to_string(), id });
        }

        this
    }

    pub fn push(self) -> Self {
        let this = self.this.clone();
        Self {
            parent: Some(Box::new(self)),
            variables: HashMap::new(),
            structures: HashMap::new(),
            this,
        }
    }

    pub fn push_function(self, this: Option<Value>) -> Self {
        Self {
            parent: Some(Box::new(self)),
            variables: HashMap::new(),
            structures: HashMap::new(),
            this,
        }
    }

    pub fn pop(self) -> Self {
        *self.parent.expect("Top-level scope popped!")
    }

    pub fn find(&self, reference: &str) -> Value {
        if let Some(value) = self.variables.get(reference) {
            return value.clone();
        }

        if let Some(parent) = self.parent.as_ref() {
            return parent.find(reference);
        }

        Value::Null
    }

    pub fn find_mut(&mut self, reference: &str) -> Option<&mut Value> {
        if let Some(value) = self.variables.get_mut(reference) {
            return Some(value);
        }

        if let Some(parent) = self.parent.as_mut() {
            return parent.find_mut(reference);
        }

        None
    }

    pub fn overwrite(&mut self, reference: &str, new: Value) -> bool {
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
