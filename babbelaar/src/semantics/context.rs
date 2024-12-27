// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::HashMap, sync::Arc};

use crate::{BabString, Constants, ExtensionStatement, FileId, FileLocation, FileRange, FunctionStatement, InterfaceStatement, ParseTree, Ranged, Structure};

use super::{SemanticLocal, SemanticReference, scope::SemanticScope, FunctionReference, SemanticFunction, SemanticGenericType, SemanticInterface, SemanticLocalKind, SemanticScopeKind, SemanticStructure, SemanticType};

#[derive(Debug)]
pub struct SemanticContext {
    pub scope: Vec<SemanticScope>,
    pub previous_scopes: Vec<SemanticScope>,

    pub definition_tracker: Option<HashMap<FileRange, SemanticReference>>,
    pub declaration_tracker: Option<Vec<SemanticReference>>,
    pub value_type_tracker: Option<HashMap<FileRange, SemanticType>>,
}

impl SemanticContext {
    pub fn new() -> Self {
        Self {
            scope: vec![
                SemanticScope::new_top_level(),
            ],
            previous_scopes: Vec::new(),
            definition_tracker: Some(HashMap::new()),
            declaration_tracker: Some(Vec::new()),
            value_type_tracker: Some(HashMap::new()),
        }
    }

    #[must_use]
    pub fn current(&mut self) -> &mut SemanticScope {
        self.scope.last_mut().unwrap()
    }

    #[must_use]
    pub fn current_scope(&self) -> &SemanticScope {
        self.scope.last().unwrap()
    }

    pub fn announce_file(&mut self, tree: &ParseTree) {
        let location_end = tree.all()
            .map(|statement| statement.range.end())
            .max()
            .unwrap_or_else(|| FileLocation::new(FileId::from_path(tree.path()), 0, 0, 0));

        let location_start = FileLocation::new(location_end.file_id(), 0, 0, 0);

        self.scope[0].range = FileRange::new(location_start, location_end);
    }

    pub fn push_function_scope(&mut self, function: &FunctionStatement, this: Option<SemanticType>) -> &mut SemanticScope {
        self.scope.push(SemanticScope {
            range: function.range,
            locals: HashMap::new(),
            structures: HashMap::new(),
            generic_types: HashMap::new(),
            this,
            return_type: None,
            kind: SemanticScopeKind::Function {
                name: function.name.value().clone(),
                right_parameter_range: function.parameters_right_paren_range,
            },
            extensions: Vec::new(),
            interfaces: HashMap::new(),
        });
        self.scope.last_mut().expect("we just pushed a scope")
    }

    pub fn push_block_scope(&mut self, range: FileRange) -> &mut SemanticScope {
        let this = self.scope.last().and_then(|x| x.this.clone());
        let return_type = self.scope.last().and_then(|x| x.return_type.clone());
        self.scope.push(SemanticScope {
            range,
            locals: HashMap::new(),
            structures: HashMap::new(),
            generic_types: HashMap::new(),
            this,
            return_type,
            kind: SemanticScopeKind::Default,
            extensions: Vec::new(),
            interfaces: HashMap::new(),
        });
        self.scope.last_mut().expect("we just pushed a scope")
    }

    pub fn push_structure_scope(&mut self, structure: &Structure) {
        let this = self.scope.last().and_then(|x| x.this.clone());
        let return_type = self.scope.last().and_then(|x| x.return_type.clone());
        self.scope.push(SemanticScope {
            range: FileRange::new(structure.left_curly_range.start(), structure.right_curly_range.end()),
            locals: HashMap::new(),
            structures: HashMap::new(),
            generic_types: structure.generic_types
                .iter()
                .enumerate()
                .map(|(index, x)| {
                    let ty = SemanticGenericType {
                        index,
                        name: x.value().clone(),
                        declaration_range: x.range(),
                    };
                    (x.value().clone(), ty)
                })
                .collect(),
            this,
            return_type,
            kind: SemanticScopeKind::Structure,
            extensions: Vec::new(),
            interfaces: HashMap::new(),
        });
    }

    pub fn push_interface_scope(&mut self, interface: &InterfaceStatement) {
        let this = self.scope.last().and_then(|x| x.this.clone());
        let return_type = self.scope.last().and_then(|x| x.return_type.clone());
        self.scope.push(SemanticScope {
            range: FileRange::new(interface.left_curly_range.start(), interface.right_curly_range.end()),
            locals: HashMap::new(),
            structures: HashMap::new(),
            generic_types: interface.generic_types
                .iter()
                .enumerate()
                .map(|(index, x)| {
                    let ty = SemanticGenericType {
                        index,
                        name: x.value().clone(),
                        declaration_range: x.range(),
                    };
                    (x.value().clone(), ty)
                })
                .collect(),
            this,
            return_type,
            kind: SemanticScopeKind::Structure,
            extensions: Vec::new(),
            interfaces: HashMap::new(),
        });
    }

    pub fn push_extension_scope(&mut self, extension: &ExtensionStatement, range: FileRange) {
        let this = self.scope.last().and_then(|x| x.this.clone());
        let return_type = self.scope.last().and_then(|x| x.return_type.clone());
        self.scope.push(SemanticScope {
            range,
            locals: HashMap::new(),
            structures: HashMap::new(),
            generic_types: extension.generic_types
                .iter()
                .enumerate()
                .map(|(index, x)| {
                    let ty = SemanticGenericType {
                        index,
                        name: x.value().clone(),
                        declaration_range: x.range(),
                    };
                    (x.value().clone(), ty)
                })
                .collect(),
            this,
            return_type,
            kind: SemanticScopeKind::Structure,
            extensions: Vec::new(),
            interfaces: HashMap::new(),
        });
    }

    pub fn push_function(&mut self, function: SemanticFunction, range: FileRange) {
        let name = function.name.value().clone();
        let declaration_range = function.name.range();

        if let Some(tracker) = &mut self.declaration_tracker {
            tracker.push(SemanticReference {
                local_name: function.name.value().clone(),
                local_kind: SemanticLocalKind::Function,
                declaration_range,
                typ: SemanticType::Function(function.clone()),
            });
        }

        let mut local = SemanticLocal::new(
            SemanticLocalKind::Function,
            SemanticType::Function(function),
            declaration_range,
        ).with_declaration_range(range);

        if name == Constants::MAIN_FUNCTION {
            local.add_usage();
        }

        self.scope.last_mut().unwrap().locals.insert(name, local);
    }

    pub fn push_structure(&mut self, structure: Arc<SemanticStructure>) {
        if let Some(tracker) = &mut self.declaration_tracker {
            tracker.push(SemanticReference {
                local_name: structure.name.value().clone(),
                local_kind: SemanticLocalKind::StructureReference,
                declaration_range: structure.name.range(),
                typ: SemanticType::Custom {
                    base: Arc::clone(&structure),
                    parameters: Vec::new(),
                },
            });

            for method in &structure.methods {
                tracker.push(SemanticReference {
                    local_name: method.name().clone(),
                    local_kind: SemanticLocalKind::Method,
                    declaration_range: method.function.name.range(),
                    typ: SemanticType::FunctionReference(FunctionReference::Custom(method.function.clone())), // is this okay?
                });
            }

            for field in &structure.fields {
                tracker.push(SemanticReference {
                    local_name: field.name.value().clone(),
                    local_kind: SemanticLocalKind::FieldReference,
                    declaration_range: field.name.range(),
                    typ: field.ty.clone(),
                });
            }
        }

        let previous_idx = self.scope.len() - 2;
        self.scope[previous_idx].structures.insert(structure.name.value().clone(), structure);
    }

    pub fn push_interface(&mut self, interface: Arc<SemanticInterface>) {
        if let Some(tracker) = &mut self.declaration_tracker {
            tracker.push(SemanticReference {
                local_name: interface.name.value().clone(),
                local_kind: SemanticLocalKind::StructureReference,
                declaration_range: interface.name.range(),
                typ: SemanticType::Interface {
                    base: Arc::clone(&interface),
                    parameters: Vec::new(),
                },
            });

            for method in &interface.methods {
                tracker.push(SemanticReference {
                    local_name: method.name().clone(),
                    local_kind: SemanticLocalKind::Method,
                    declaration_range: method.function.name.range(),
                    typ: SemanticType::FunctionReference(FunctionReference::Custom(method.function.clone())), // is this okay?
                });
            }
        }

        let previous_idx = self.scope.len() - 2;
        self.scope[previous_idx].interfaces.insert(interface.name.value().clone(), interface);
    }

    pub fn pop_scope(&mut self) {
        debug_assert!(self.scope.len() > 1);
        let scope = self.scope.pop().unwrap();

        self.previous_scopes.push(scope);
    }

    pub fn push_local(&mut self, name: &Ranged<BabString>, local: SemanticLocal) {
        if name.value() == &Constants::DISCARDING_IDENT {
            return;
        }

        if let Some(tracker) = &mut self.declaration_tracker {
            tracker.push(SemanticReference {
                local_name: name.value().clone(),
                local_kind: local.kind,
                declaration_range: name.range(),
                typ: local.typ.clone(),
            });
        }

        self.scope.last_mut().as_mut().unwrap().locals.insert(name.value().clone(), local);
    }
}
