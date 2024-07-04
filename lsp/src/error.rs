// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use babbelaar::ParseDiagnostic;
use thiserror::Error;

type IoError = std::io::Error;

pub type BabbelaarLspResult<T> = core::result::Result<T, BabbelaarLspError>;

#[derive(Debug, Error)]
pub enum BabbelaarLspError {
    #[error("I/O-fout: {0}")]
    IoError(IoError),

    #[error("parseerfout: {0}")]
    ParseError(String),
}

impl From<IoError> for BabbelaarLspError {
    fn from(value: IoError) -> Self {
        Self::IoError(value)
    }
}

impl From<ParseDiagnostic<'_>> for BabbelaarLspError {
    fn from(value: ParseDiagnostic<'_>) -> Self {
        Self::ParseError(value.to_string())
    }
}
