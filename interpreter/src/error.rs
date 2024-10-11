// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::Display;

use babbelaar::BabString;

#[derive(Debug, Clone)]
pub struct RuntimeError {
    message: BabString,
}

impl RuntimeError {
    #[must_use]
    pub fn array_out_of_bounds(array_size: usize, index: i64) -> Self {
        Self {
            message: format!("Opeenvolging-index is buiten bereik, index={index} terwijl grootte={array_size}").into(),
        }
    }

    #[must_use]
    pub fn message(&self) -> BabString {
        self.message.clone()
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message)
    }
}
