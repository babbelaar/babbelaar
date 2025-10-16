// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::HashMap, sync::Arc};

use crate::{BabString, Builtin, FileId, FileLocation, FileRange, Ranged, StructureId};

use super::{FunctionReference, SemanticExtension, SemanticFunction, SemanticGenericType, SemanticInterface, SemanticLocal, SemanticLocalKind, SemanticType};

#[derive(Debug)]
pub struct SemanticScope {
    pub range: FileRange,
    pub locals: HashMap<BabString, SemanticLocal>,
    pub structures: HashMap<BabString, StructureId>,
    pub generic_types: HashMap<BabString, SemanticGenericType>,
    pub this: Option<SemanticType>,
    pub return_type: Option<Ranged<SemanticType>>,
    pub kind: SemanticScopeKind,
    pub extensions: Vec<SemanticExtension>,
    pub interfaces: HashMap<BabString, Arc<SemanticInterface>>,
}

impl SemanticScope {
    pub fn new_top_level() -> Self {
        let mut this = Self {
            range: FileRange::new(
                FileLocation::new(FileId::INTERNAL, 0, 0, 0),
                FileLocation::new(FileId::INTERNAL, usize::MAX, usize::MAX, usize::MAX),
            ),
            structures: HashMap::new(),
            locals: HashMap::default(),
            this: None,
            return_type: None,
            generic_types: HashMap::new(),
            kind: SemanticScopeKind::TopLevel,
            extensions: Vec::new(),
            interfaces: HashMap::new(),
        };

        for func in Builtin::FUNCTIONS {
            this.locals.insert(BabString::new_static(func.name), SemanticLocal::new(
                SemanticLocalKind::Function,
                SemanticType::FunctionReference(FunctionReference::Builtin(func)),
                FileRange::INTERNAL,
            ));
        }

        this
    }

    #[must_use]
    pub fn is_location_inside(&self, location: FileLocation) -> bool {
        match &self.kind {
            SemanticScopeKind::TopLevel => true,
            _ => self.range.contains(location),
        }
    }

    #[must_use]
    pub fn get_function_mut(&mut self, name: &BabString) -> Option<&mut Arc<SemanticFunction>> {
        let local = self.locals.get_mut(name)?;
        match &mut local.typ {
            SemanticType::Function(semantic_function) => Some(semantic_function),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub enum SemanticScopeKind {
    #[default]
    Default,

    TopLevel,
    Structure,

    Function {
        name: BabString,
        right_parameter_range: FileRange,
    },
}

impl SemanticScopeKind {
    #[must_use]
    pub const fn is_function(&self) -> bool {
        matches!(self, Self::Function { .. })
    }
}
