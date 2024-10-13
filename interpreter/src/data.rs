// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::collections::HashMap;

use babbelaar::{AttributeList, BabString, FunctionStatement, MethodId, Structure};

#[derive(Debug)]
pub struct InterpreterFunction {
    pub attributes: AttributeList,
    pub function: FunctionStatement,
}

#[derive(Debug)]
pub struct InterpreterStructure {
    pub method_ids: HashMap<BabString, MethodId>,
    pub structure: Structure,
}

impl InterpreterStructure {
    pub fn ast(&self) -> &Structure {
        &self.structure
    }

    #[must_use]
    pub fn name(&self) -> &BabString {
        &self.structure.name
    }
}
