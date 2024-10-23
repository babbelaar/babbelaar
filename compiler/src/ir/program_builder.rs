// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::collections::HashMap;

use babbelaar::BabString;

use super::{FunctionBuilder, Program, RegisterAllocator};

#[derive(Debug)]
pub struct ProgramBuilder {
    program: Program,
}

impl ProgramBuilder {
    #[must_use]
    pub fn new() -> Self {
        Self {
            program: Program::new(),
        }
    }

    pub fn build_function<F: FnOnce(&mut FunctionBuilder)>(&mut self, name: BabString, f: F) {
        assert!(!name.is_empty(), "Kan geen lege naam als werkwijzenaam hebben.");

        let mut builder = FunctionBuilder {
            name: name.clone(),
            program: &mut self.program,
            register_allocator: RegisterAllocator::new(),
            argument_registers: Vec::new(),
            instructions: Vec::new(),
            locals: HashMap::new(),
            labels: Vec::new(),
        };

        f(&mut builder);

        let function = builder.build();
        self.program.add_function(name, function);
    }

    #[must_use]
    pub fn build(self) -> Program {
        self.program
    }
}
