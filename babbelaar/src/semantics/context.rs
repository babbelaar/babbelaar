// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::HashMap, sync::Arc};

use crate::{BabString, Constants, ExtensionStatement, FileId, FileLocation, FileRange, FunctionStatement, InterfaceStatement, ParseTree, Ranged, SemanticExtensionId, SemanticScopeId, Structure, StructureId, semantics::{SemanticExtension, SemanticField, SemanticFieldId}};

use super::{SemanticLocal, SemanticReference, scope::SemanticScope, FunctionReference, SemanticFunction, SemanticGenericType, SemanticInterface, SemanticLocalKind, SemanticScopeKind, SemanticStructure, SemanticType};

#[derive(Debug)]
pub struct SemanticContext {
    pub scope: SemanticContextScopes,
    pub structures: HashMap<StructureId, SemanticStructure>,
    pub extensions: Vec<SemanticExtension>,

    pub definition_tracker: Option<HashMap<FileRange, SemanticReference>>,
    pub declaration_tracker: Option<Vec<SemanticReference>>,
    pub value_type_tracker: Option<HashMap<FileRange, SemanticType>>,
}

impl SemanticContext {
    pub fn new() -> Self {
        Self {
            scope: SemanticContextScopes::new(),
            structures: Default::default(),
            extensions: Default::default(),
            definition_tracker: Some(HashMap::new()),
            declaration_tracker: Some(Vec::new()),
            value_type_tracker: Some(HashMap::new()),
        }
    }

    #[must_use]
    pub fn scopes_iter(&self) -> impl Iterator<Item = &SemanticScope> + DoubleEndedIterator {
        self.scope.iter()
    }

    #[must_use]
    pub fn scopes_iter_mut<'a>(&'a mut self) -> ScopeIterMut<'a> {
        self.scope.iter_mut()
    }

    #[must_use]
    pub fn current(&mut self) -> &mut SemanticScope {
        self.scope.current_mut()
    }

    #[must_use]
    pub fn current_scope(&self) -> &SemanticScope {
        self.scope.current()
    }

    pub fn announce_file(&mut self, tree: &ParseTree) {
        let location_end = tree.all()
            .map(|statement| statement.range.end())
            .max()
            .unwrap_or_else(|| FileLocation::new(FileId::from_path(tree.path()), 0, 0, 0));

        let location_start = FileLocation::new(location_end.file_id(), 0, 0, 0);

        self.scope.all_scopes[0].range = FileRange::new(location_start, location_end);
    }

    pub fn push_function_scope(&mut self, function: &FunctionStatement, this: Option<SemanticType>) -> &mut SemanticScope {
        self.push_scope(SemanticScope {
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
        }).1
    }

    pub fn push_block_scope(&mut self, range: FileRange) -> &mut SemanticScope {
        let this = self.current_scope().this.clone();
        let return_type = self.current_scope().return_type.clone();
        self.push_scope(SemanticScope {
            range,
            locals: HashMap::new(),
            structures: HashMap::new(),
            generic_types: HashMap::new(),
            this,
            return_type,
            kind: SemanticScopeKind::Default,
            extensions: Vec::new(),
            interfaces: HashMap::new(),
        }).1
    }

    pub fn push_structure_scope(&mut self, structure: &Structure) {
        let this = self.current_scope().this.clone();
        let return_type = self.current_scope().return_type.clone();
        self.push_scope(SemanticScope {
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
        let this = self.current_scope().this.clone();
        let return_type = self.current_scope().return_type.clone();
        self.push_scope(SemanticScope {
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
        let this = self.current_scope().this.clone();
        let return_type = self.current_scope().return_type.clone();
        self.push_scope(SemanticScope {
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

        let function = Arc::new(function);
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

        self.current().locals.insert(name, local);
    }

    pub fn push_structure(&mut self, structure: SemanticStructure, id: StructureId) {
        if let Some(tracker) = &mut self.declaration_tracker {
            tracker.push(SemanticReference {
                local_name: structure.name.value().clone(),
                local_kind: SemanticLocalKind::StructureReference,
                declaration_range: structure.name.range(),
                typ: SemanticType::Custom {
                    base: id,
                    parameters: Vec::new(),
                    name: structure.name.clone(),
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

        let previous_idx = self.scope.scope.len() - 2;
        self.scope.all_scopes[self.scope.scope[previous_idx].id()].structures.insert(structure.name.value().clone(), id);
        self.structures.insert(id, structure);
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

        let previous_idx = self.scope.scope.len() - 2;
        self.scope.all_scopes[self.scope.scope[previous_idx].id()].interfaces.insert(interface.name.value().clone(), interface);
    }

    pub fn push_extension(&mut self, extension: SemanticExtension) -> SemanticExtensionId {
        let id = SemanticExtensionId::new(self.extensions.len());
        if let Some(tracker) = &mut self.declaration_tracker {
            for method in extension.methods.values() {
                tracker.push(SemanticReference {
                    local_name: method.name().clone(),
                    local_kind: SemanticLocalKind::Method,
                    declaration_range: method.function.name.range(),
                    typ: SemanticType::FunctionReference(FunctionReference::Custom(method.function.clone())), // is this okay?
                });
            }
        }

        let previous_idx = self.scope.scope.len() - 2;

        self.extensions.push(extension);
        self.scope.all_scopes[self.scope.scope[previous_idx].id()].extensions.push(id);

        id
    }

    pub fn pop_scope(&mut self) {
        self.scope.pop();
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

        self.current().locals.insert(name.value().clone(), local);
    }

    #[must_use]
    pub fn structure(&self, id: StructureId) -> &SemanticStructure {
        log::trace!("Get structure with ID {id:?}");
        self.structures.get(&id).expect("Cannot find structure with ID")
    }

    #[must_use]
    pub fn field(&self, structure_id: StructureId, field_id: SemanticFieldId) -> &SemanticField {
        &self.structure(structure_id).fields[field_id.id()]
    }

    fn push_scope(&mut self, scope: SemanticScope) -> (SemanticScopeId, &mut SemanticScope) {
        self.scope.push(scope)
    }
}

#[derive(Debug)]
pub struct SemanticContextScopes {
    pub scope: Vec<SemanticScopeId>,
    pub previous_scopes: Vec<SemanticScopeId>,
    pub all_scopes: Vec<SemanticScope>,
}

impl SemanticContextScopes {
    #[must_use]
    pub fn new() -> Self {
        Self {
            scope: vec![
                SemanticScopeId::new(0),
            ],
            previous_scopes: Vec::new(),
            all_scopes: vec![
                SemanticScope::new_top_level(),
            ],
        }
    }

    fn push(&mut self, scope: SemanticScope) -> (SemanticScopeId, &mut SemanticScope) {
        let id = SemanticScopeId::new(self.all_scopes.len());

        self.scope.push(id);
        self.all_scopes.push(scope);

        (id, self.all_scopes.last_mut().expect("we hebben net een scoop toegevoegd"))
    }

    fn pop(&mut self) {
        debug_assert!(self.scope.len() > 1);
        let scope = self.scope.pop().unwrap();

        self.previous_scopes.push(scope);
    }

    #[must_use]
    pub fn current(&self) -> &SemanticScope {
        &self.all_scopes[self.scope.last().unwrap().id()]
    }

    #[must_use]
    pub fn current_mut(&mut self) -> &mut SemanticScope {
        &mut self.all_scopes[self.scope.last_mut().unwrap().id()]
    }

    #[must_use]
    pub fn parent_mut(&mut self) -> &mut SemanticScope {
        &mut self.all_scopes[self.scope[self.scope.len() - 2].id()]
    }

    #[must_use]
    pub fn iter(&self) -> impl Iterator<Item = &SemanticScope> + DoubleEndedIterator {
        self.scope.iter()
            .map(|x| &self.all_scopes[x.id()])
    }

    #[must_use]
    pub fn iter_mut<'c>(&'c mut self) -> ScopeIterMut<'c> {
        let bottom_idx = self.scope.len();
        ScopeIterMut { ctx: self, idx: 0, bottom_idx }
    }
}

pub struct ScopeIterMut<'c> {
    ctx: &'c mut SemanticContextScopes,
    idx: usize,
    bottom_idx: usize,
}

impl<'c> Iterator for ScopeIterMut<'c> {
    type Item = &'c mut SemanticScope;

    fn next(&mut self) -> Option<Self::Item> {
        if self.idx >= self.bottom_idx {
            return None;
        }

        let scope_id = self.ctx.scope.get(self.idx).copied()?;
        self.idx += 1;

        let ptr = self.ctx.all_scopes.as_mut_ptr();
        unsafe {
            Some(&mut *ptr.add(scope_id.id()))
        }
    }
}

impl<'c> DoubleEndedIterator for ScopeIterMut<'c> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.idx >= self.bottom_idx {
            return None;
        }

        self.bottom_idx -= 1;
        let scope_id = self.ctx.scope.get(self.bottom_idx).copied()?;

        let ptr = self.ctx.all_scopes.as_mut_ptr();
        unsafe {
            Some(&mut *ptr.add(scope_id.id()))
        }
    }
}
