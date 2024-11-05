// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::collections::HashMap;

use babbelaar::{BabString, Structure};

use crate::{ir::{function_builder::FunctionLocal, ArgumentName}, ArgumentList, TypeId, TypeManager};

use super::{FunctionBuilder, Program, RegisterAllocator};

#[derive(Debug)]
pub struct ProgramBuilder {
    pub(super) program: Program,
    pub(super) type_manager: TypeManager,
}

impl ProgramBuilder {
    #[must_use]
    pub fn new() -> Self {
        Self {
            program: Program::new(),
            type_manager: TypeManager::new(),
        }
    }

    pub fn build_function<F: FnOnce(&mut FunctionBuilder)>(&mut self, name: BabString, arguments: ArgumentList, f: F) {
        assert!(!name.is_empty(), "Kan geen lege naam als werkwijzenaam hebben.");

        let mut builder = FunctionBuilder {
            name: name.clone(),
            program_builder: self,
            register_allocator: RegisterAllocator::new(),
            argument_registers: Vec::new(),
            this: None,
            instructions: Vec::new(),
            locals: HashMap::new(),
            label_counter: 0,
            label_names: HashMap::new(),
            label_positions: HashMap::new(),
        };

        for (name, type_id) in arguments.iter().cloned() {
            let register = builder.register_allocator.next();
            builder.argument_registers.push(register);

            match name {
                ArgumentName::Name(name) => {
                    builder.locals.insert(name, FunctionLocal {
                        register,
                        type_id,
                    });
                }

                ArgumentName::This => {
                    debug_assert_eq!(builder.this, None);
                    builder.this = Some((type_id, register));
                }
            }
        }

        f(&mut builder);

        let function = builder.build();
        self.program.add_function(name, function);
    }

    #[must_use]
    pub fn build(self) -> Program {
        self.program
    }

    pub fn add_structure(&mut self, structure: &Structure)  {
        self.type_manager.add_structure(structure);
    }

    #[must_use]
    pub fn type_id_for_structure(&self, name: &BabString) -> TypeId {
        self.type_manager.layout_of(name).type_id().clone()
    }
}
