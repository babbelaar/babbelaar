// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::collections::HashMap;

use babbelaar::{AttributeList, BabString, ExtensionId, FunctionStatement, InterfaceId, InterfaceStatement, MethodId, Structure};

#[derive(Debug)]
pub struct InterpreterExtension {
    pub interface: Option<InterfaceId>,
    pub methods: HashMap<BabString, MethodId>,
}

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
    pub extension_ids: Vec<ExtensionId>,
    pub extension_method_ids: HashMap<BabString, MethodId>,
    pub structure: Structure,
}

impl InterpreterStructure {
    pub fn get_method_by_name(&self, name: &BabString) -> Option<MethodId> {
        self.method_ids.get(name)
            .or_else(|| self.extension_method_ids.get(name))
            .copied()
    }
}

impl InterpreterStructure {
    #[must_use]
    pub fn name(&self) -> &BabString {
        &self.structure.name
    }
}
