// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use babbelaar::ParseDiagnostic;
use thiserror::Error;
use tower_lsp::jsonrpc::ErrorCode;

type IoError = std::io::Error;
type LspError = tower_lsp::jsonrpc::Error;

pub type BabbelaarLspResult<T> = core::result::Result<T, BabbelaarLspError>;

#[derive(Debug, Error)]
pub enum BabbelaarLspError {
    #[error("I/O-fout: {0}")]
    IoError(IoError),

    #[error("parseerfout: {0}")]
    ParseError(ParseDiagnostic),

    #[error("geopend document heeft geen bestandspad as URL")]
    UrlNotFilePath,

    #[error("ongeldige data verstuurd: {explanation}")]
    InvalidDataSent { explanation: String },
}

impl From<IoError> for BabbelaarLspError {
    fn from(value: IoError) -> Self {
        Self::IoError(value)
    }
}

impl From<ParseDiagnostic> for BabbelaarLspError {
    fn from(value: ParseDiagnostic) -> Self {
        Self::ParseError(value)
    }
}

impl From<BabbelaarLspError> for LspError {
    fn from(value: BabbelaarLspError) -> Self {
        LspError {
            code: ErrorCode::ServerError(1),
            message: value.to_string().into(),
            data: None,
        }
    }
}
