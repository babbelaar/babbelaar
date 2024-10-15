// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::collections::HashMap;

use babbelaar::{AttributeList, BabString, FunctionStatement, InterfaceStatement, MethodId, Structure};

#[derive(Debug)]
pub struct InterpreterFunction {
    pub attributes: AttributeList,
    pub function: FunctionStatement,
}

#[derive(Debug)]
pub struct InterpreterInterface {
    pub method_ids: HashMap<BabString, MethodId>,
    pub interface: InterfaceStatement,
}

impl InterpreterInterface {
    #[must_use]
    pub fn name(&self) -> &BabString {
        &self.interface.name
    }
}

#[derive(Debug)]
pub struct InterpreterStructure {
    pub method_ids: HashMap<BabString, MethodId>,
    pub structure: Structure,
}

impl InterpreterStructure {
    #[must_use]
    pub fn name(&self) -> &BabString {
        &self.structure.name
    }
}
