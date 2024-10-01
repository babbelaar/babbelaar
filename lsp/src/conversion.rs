// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::path::PathBuf;

use babbelaar::{BabbelaarCommand, FileId, FileLocation, FileRange, SourceCode, Token};
use tower_lsp::lsp_types::{Command, Location, Position, Range, Uri as Url};

use crate::{BabbelaarLspError, BabbelaarLspResult};

#[derive(Debug, Clone)]
pub struct Converter {
    source_code: SourceCode,
    encoding: TextEncoding,
}

impl Converter {
    pub fn new(source_code: SourceCode, encoding: TextEncoding) -> Self {
        Self {
            source_code,
            encoding,
        }
    }

    #[must_use]
    pub fn convert_file_range_to_location(&self, uri: Url, range: FileRange) -> Location {
        Location {
            uri,
            range: self.convert_file_range(range),
        }
    }

    #[must_use]
    pub fn convert_token_range(&self, token: &Token) -> Range {
        self.convert_file_range(token.range())
    }

    #[must_use]
    pub fn convert_file_range(&self, range: FileRange) -> Range {
        Range {
            start: self.convert_position(range.start()),
            end: self.convert_position(range.end()),
        }
    }

    #[must_use]
    pub fn convert_position(&self, location: FileLocation) -> Position {
        match self.encoding {
            TextEncoding::Utf8 => {
                Position {
                    line: location.line() as _,
                    character: location.column() as _,
                }
            }

            TextEncoding::Utf16 => {
                let line = self.source_code.lines().nth(location.line() as _).unwrap();
                let line = &line[..location.column() as _];

                Position {
                    line: location.line() as _,
                    character: line.encode_utf16().count() as _,
                }
            }
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum TextEncoding {
    #[default]
    Utf16,
    Utf8,
}

#[must_use]
pub fn convert_command(command: &BabbelaarCommand) -> Command {
    match command {
        BabbelaarCommand::RenameField => Command {
            title: command.to_string(),
            command: "editor.action.rename".into(),
            arguments: None,
        },

        BabbelaarCommand::RenameParameter => Command {
            title: command.to_string(),
            command: "editor.action.rename".into(),
            arguments: None,
        },

        BabbelaarCommand::RenameFunction => Command {
            title: command.to_string(),
            command: "editor.action.rename".into(),
            arguments: None,
        },
    }
}

pub trait UrlExtension {
    fn to_path(&self) -> BabbelaarLspResult<PathBuf>;
}

impl UrlExtension for Url {
    fn to_path(&self) -> BabbelaarLspResult<PathBuf> {
        let url = match url::Url::parse(self.as_str()) {
            Ok(url) => url,
            Err(e) => {
                log::error!("Invalid URL: \"{}\" reason: {e}", self.as_str());
                return Err(BabbelaarLspError::UrlNotFilePath);
            }
        };

        url.to_file_path().map_err(|()| BabbelaarLspError::UrlNotFilePath)
    }
}

pub trait StrExtension {
    #[must_use]
    fn canonicalize_position(&self, file_id: FileId, position: Position) -> FileLocation;
}

impl StrExtension for str {
    fn canonicalize_position(&self, file_id: FileId, position: Position) -> FileLocation {
        let line = (position.line as usize).saturating_sub(1);
        let column = position.character as usize;

        let line_offset = self.char_indices()
            .filter(|(_, c)| *c == '\n')
            .map(|(offset, _)| offset + 1)
            .nth(line)
            .unwrap_or(usize::MAX);

        FileLocation::new(file_id, line_offset.saturating_add(column), line, column)
    }
}
