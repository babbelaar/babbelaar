// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::sync::Arc;

use babbelaar::{AttributeList, BabString, FunctionStatement, Structure};

#[derive(Debug)]
pub struct InterpreterFunction {
    pub attributes: AttributeList,
    pub function: FunctionStatement,
}

#[derive(Debug)]
pub struct InterpreterStructure {
    pub structure: Structure,
    pub methods: Vec<Arc<InterpreterFunction>>,
}

impl InterpreterStructure {
    #[must_use]
    pub fn name(&self) -> &BabString {
        &self.structure.name
    }
}
