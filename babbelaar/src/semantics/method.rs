// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::sync::Arc;

use crate::{BabString, FileRange};

use super::{PureValue, SemanticFunction, SemanticType, SemanticUsage};

#[derive(Debug, Clone)]
pub struct SemanticMethod {
    pub range: FileRange,
    pub function: Arc<SemanticFunction>,
    pub is_static: bool,
}

impl SemanticMethod {
    #[must_use]
    pub fn name(&self) -> &BabString {
        &self.function.name
    }

    #[must_use]
    pub fn return_type(&self) -> SemanticType {
        self.function.return_type.as_ref().clone()
    }

    #[must_use]
    pub fn return_type_usage(&self) -> SemanticUsage {
        SemanticUsage::Pure(PureValue::ReturnValue)
    }
}
