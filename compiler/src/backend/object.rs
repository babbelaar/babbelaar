// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use babbelaar::BabString;

#[derive(Debug, Clone)]
pub struct CompiledFunction {
    pub(super) name: BabString,
    pub(super) byte_code: Vec<u8>,
}

impl CompiledFunction {
    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    #[must_use]
    pub fn byte_code(&self) -> &[u8] {
        &self.byte_code
    }
}

#[derive(Debug, Default, Clone)]
pub struct CompiledObject {
    pub(super) functions: Vec<CompiledFunction>,
}

impl CompiledObject {
    pub fn functions(&self) -> &[CompiledFunction] {
        &self.functions
    }
}
