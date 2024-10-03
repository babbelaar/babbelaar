// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::path::PathBuf;

use babbelaar::{BabbelaarCommand, FileLocation, FileRange, SourceCode, Token};
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
        debug_assert_eq!(location.file_id(), self.source_code.file_id(), "Incompatible file IDs!");

        match self.encoding {
            TextEncoding::Utf8 => {
                Position {
                    line: location.line() as _,
                    character: location.column() as _,
                }
            }

            TextEncoding::Utf16 => {
                let line = match self.source_code.lines().nth(location.line()) {
                    Some(line) => line,

                    None => {
                        let line_count = self.source_code.lines().count();
                        if location.line() == line_count {
                            // we always have an empty trailing line
                            ""
                        } else {
                            panic!("Illegal line {}, we have {line_count} lines!", location.line());
                        }
                    }
                };

                // TODO: improve this (see comment at `FileLocation` in util.rs`)
                // This basically converts the *UTF-32 FileLocation::column()* to UTF-16.
                let utf16_column: usize = line.chars()
                    .take(location.column())
                    .map(|ch| ch.len_utf16())
                    .sum();

                Position {
                    line: location.line() as _,
                    character: utf16_column as _,
                }
            }
        }
    }

    pub fn convert_location(&self, position: Position) -> FileLocation {
        let offset = 0; // TODO: it would be nice to encode this :)
        let line = position.line as usize;
        let column = position.character as usize;

        let column = match self.encoding {
            TextEncoding::Utf8 => column,
            TextEncoding::Utf16 => {
                let Some(line) = self.source_code.lines().nth(line) else {
                    panic!("Illegal position given, line index={line} while file has {} line(s)", self.source_code.lines().count());
                };

                let mut utf8_column = 0;
                let mut utf16_column = 0;
                for character in line.chars() {
                    if utf16_column == column {
                        break;
                    }

                    utf16_column += character.len_utf16();
                    utf8_column += character.len_utf8();
                }

                debug_assert!(utf16_column == column);
                utf8_column
            }
        };

        FileLocation::new(self.source_code.file_id(), offset, line, column)
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
