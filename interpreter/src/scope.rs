// Copyright (C) 2023 - 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::HashMap, rc::Rc};

use babbelaar::Structure;

use crate::{Builtin, FunctionId, Value};

#[derive(Default)]
pub struct Scope<'source_code> {
    pub parent: Option<Box<Scope<'source_code>>>,
    pub variables: HashMap<String, Value>,
    pub structures: HashMap<String, Rc<Structure<'source_code>>>,
}

impl<'source_code> Scope<'source_code> {
    pub fn new() -> Self {
        Self {
            parent: None,
            variables: HashMap::new(),
            structures: HashMap::new(),
        }
    }

    pub fn new_top_level() -> Self {
        let mut this = Self::new();

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
        Self {
            parent: Some(Box::new(self)),
            variables: HashMap::new(),
            structures: HashMap::new(),
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

        return Value::Null;
    }
}
