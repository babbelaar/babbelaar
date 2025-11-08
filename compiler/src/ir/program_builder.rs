// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::collections::HashMap;

use babbelaar::{BabString, BuiltinType, SemanticType, Structure};

use crate::{ir::{function_builder::FunctionLocal, ArgumentName}, ArgumentList, FunctionParameter, Instruction, PrimitiveType, TypeId, TypeManager};

use super::{FunctionAttribute, FunctionBuilder, Program, RegisterAllocator};

#[derive(Debug)]
pub struct ProgramBuilder {
    pub(super) program: Program,
    pub(super) function_attributes: HashMap<BabString, Vec<FunctionAttribute>>,
    pub(super) function_return_types: HashMap<BabString, TypeId>,
    pub(super) type_manager: TypeManager,
}

impl ProgramBuilder {
    #[must_use]
    pub fn new() -> Self {
        Self {
            program: Program::new(),
            function_attributes: HashMap::new(),
            function_return_types: HashMap::new(),
            type_manager: TypeManager::new(),
        }
    }

    pub fn build_function<F: FnOnce(&mut FunctionBuilder)>(&mut self, name: BabString, arguments: ArgumentList, f: F) {
        assert!(!name.is_empty(), "Kan geen lege naam als werkwijzenaam hebben.");

        log::trace!("{:#?}", self.function_return_types);
        let return_ty = match self.function_return_types.get(&name) {
            Some(return_ty) => self.type_manager.layout(*return_ty).primitive_type(),
            None => PrimitiveType::new(0, false),
        };

        let mut builder = FunctionBuilder {
            name: name.clone(),
            program_builder: self,
            register_allocator: RegisterAllocator::new(),
            return_ty,
            arguments: Vec::new(),
            this: None,
            instructions: Vec::new(),
            locals: HashMap::new(),
            label_counter: 0,
            label_names: HashMap::new(),
            label_positions: HashMap::new(),
        };

        for (idx, (name, type_info)) in arguments.iter().cloned().enumerate() {
            let register = builder.register_allocator.next();
            let primitive = builder.primitive_type_of(&type_info);
            builder.arguments.push(FunctionParameter::new(register, type_info.clone(), primitive));

            match name {
                ArgumentName::Name(name) => {
                    builder.locals.insert(name, FunctionLocal {
                        register,
                        type_info,
                        primitive,
                    });
                }

                ArgumentName::This => {
                    debug_assert_eq!(builder.this, None);
                    builder.this = Some((type_info, register, primitive));
                }
            }

            builder.instructions.push(Instruction::InitArg {
                destination: register,
                arg_idx: idx,
            });
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
    pub fn type_id_for_builtin(&self, ty: BuiltinType) -> TypeId {
        match ty {
            BuiltinType::Bool => TypeId::BOOL,
            BuiltinType::G8 => TypeId::G8,
            BuiltinType::G16 => TypeId::G16,
            BuiltinType::G32 => TypeId::G32,
            BuiltinType::G64 => TypeId::G64,
            BuiltinType::Null => TypeId::NULL,
            BuiltinType::Slinger => TypeId::SLINGER,
            BuiltinType::Teken => TypeId::TEKEN,
        }
    }

    #[must_use]
    pub fn type_id_for_structure(&self, name: &BabString) -> TypeId {
        self.type_manager.layout_of(name).type_id().clone()
    }

    #[must_use]
    pub fn type_id_for_semantic(&self, ty: &SemanticType) -> TypeId {
        match ty {
            SemanticType::Builtin(ty) => self.type_id_for_builtin(*ty),
            SemanticType::Custom { name, .. } => self.type_id_for_structure(&name),
            SemanticType::Pointer(..) => TypeId::G64,
            _ => todo!("TypeId for ty: {ty:#?}"),
        }
    }

    pub fn add_function_alias(&mut self, name: &BabString, actual_name: &BabString) {
        self.program.add_function_alias(name, actual_name);
    }

    pub fn add_function_attribute(&mut self, name: BabString, attr: FunctionAttribute) {
        self.function_attributes.entry(name).or_default().push(attr);
    }

    #[must_use]
    pub fn function_attributes_of(&self, name: &BabString) -> &[FunctionAttribute] {
        match self.function_attributes.get(name) {
            Some(attributes) => &attributes,
            None => &[],
        }
    }

    pub fn add_function_return_type(&mut self, name: BabString, type_id: TypeId) {
        self.function_return_types.insert(name, type_id);
    }
}
