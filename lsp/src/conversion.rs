// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use babbelaar::{FileLocation, FileRange, Token};
use tower_lsp::lsp_types::{Location, Position, Range, Url};

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
