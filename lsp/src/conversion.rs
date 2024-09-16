// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::path::PathBuf;

use babbelaar::{FileLocation, FileRange, Token};
use tower_lsp::lsp_types::{Location, Position, Range, Url};

use crate::{BabbelaarLspError, BabbelaarLspResult};

pub fn convert_file_range_to_location(uri: Url, range: FileRange) -> Location {
    Location {
        uri,
        range: convert_file_range(range),
    }
}

pub fn convert_token_range(token: &Token) -> Range {
    convert_file_range(token.range())
}

pub fn convert_file_range(range: FileRange) -> Range {
    Range {
        start: convert_position(range.start()),
        end: convert_position(range.end()),
    }
}

pub fn convert_position(location: FileLocation) -> Position {
    Position {
        line: location.line() as _,
        character: location.column() as _,
    }
}

pub trait UrlExtension {
    fn to_path(&self) -> BabbelaarLspResult<PathBuf>;
}

impl UrlExtension for Url {
    fn to_path(&self) -> BabbelaarLspResult<PathBuf> {
        self.to_file_path().map_err(|()| BabbelaarLspError::UrlNotFilePath)
    }
}

pub trait StrExtension {
    #[must_use]
    fn canonicalize_position(&self, position: Position) -> FileLocation;
}

impl StrExtension for str {
    fn canonicalize_position(&self, position: Position) -> FileLocation {
        let line = position.line as usize - 1;
        let column = position.character as usize;

        let line_offset = self.char_indices()
            .filter(|(_, c)| *c == '\n')
            .map(|(offset, _)| offset + 1)
            .nth(line)
            .unwrap_or(usize::MAX);

        FileLocation::new(line_offset.saturating_add(column), line, column)
    }
}
