// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::{HashMap, HashSet}, sync::Arc};

use crate::{BabString, FileRange};

use super::{SemanticInterface, SemanticMethod, SemanticType};

#[derive(Debug, Clone)]
pub struct SemanticExtension {
    pub ty: SemanticType,
    pub generic_types: HashSet<BabString>,
    pub interface: Option<Arc<SemanticInterface>>,
    pub methods: HashMap<BabString, SemanticMethod>,
    pub range: FileRange,
    pub right_curly_bracket: FileRange,
}

impl SemanticExtension {
    #[must_use]
    pub fn is_for_type(&self, typ: &SemanticType) -> bool {
        let SemanticType::Custom { base: req_base, parameters: req_params } = typ else {
            return &self.ty == typ;
        };

        let SemanticType::Custom { base: ext_base, parameters: ext_params } = &self.ty else {
            return &self.ty == typ;
        };

        if req_base != ext_base {
            // TODO: i think this makes things like Lijst<Lijst<T>> not work... we should do generic type erasure for `ext_base` too
            return false;
        }

        if req_params.len() != ext_params.len() {
            return false;
        }

        for (req_param, ext_param) in req_params.iter().zip(ext_params.iter()) {
            if req_param == ext_param {
                continue;
            }

            if self.generic_types.contains(&ext_param.name()) {
                continue;
            }

            log::warn!("Trace: typen {req_param} en {ext_param} komen niet overeen");
            return false;
        }

        true
    }
}
