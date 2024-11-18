// Copyright (C) 2023 - 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{error::Error, fmt::Display, str::CharIndices};

use strum::AsRefStr;
use thiserror::Error;

use crate::{FileLocation, Keyword, Punctuator, Slice, SourceCode, TemplateStringToken, Token, TokenKind};

pub struct Lexer<'source_code> {
    input: &'source_code SourceCode,
    chars: CharIndices<'source_code>,

    current: Option<(FileLocation, char)>,
    line: usize,
    column: usize,
    errors: Vec<LexerError>,
}

impl<'source_code> Lexer<'source_code> {
    pub fn new(input: &'source_code SourceCode) -> Self {
        Self {
            input,
            chars: input.char_indices(),
            current: None,
            line: 0,
            column: 0,
            errors: Vec::new(),
        }
    }

    pub fn next(&mut self) -> Option<Token> {
        self.skip_whitespace();

        let ch = self.peek_char()?;
        let tok = match ch {
            '"' => self.consume_string(),
            '€' => self.consume_template_string(),
            '\'' => self.consume_character_literal(),

            'a'..='z' | 'A'..='Z' | '_' => self.consume_identifier_or_keyword(),
            '0'..='9' => self.consume_number(),

            '(' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::LeftParenthesis)),
            ')' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::RightParenthesis)),
            '{' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::LeftCurlyBracket)),
            '}' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::RightCurlyBracket)),
            '[' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::LeftSquareBracket)),
            ']' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::RightSquareBracket)),
            ';' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::Semicolon)),
            ',' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::Comma)),
            '=' => self.consume_single_or_double_char_token(Punctuator::Assignment, Punctuator::Equals),
            '+' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::PlusSign)),
            '-' => self.consume_minus_or_arrow(),
            '/' => self.handle_solidus(),
            '*' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::Asterisk)),
            '%' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::PercentageSign)),
            ':' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::Colon)),
            '.' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::Period)),
            '@' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::AtSign)),
            '<' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::LessThan)),
            '>' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::GreaterThan)),
            '&' => self.consume_single_or_double_char_token(Punctuator::BitwiseAnd, Punctuator::LogicalAnd),
            '|' => self.consume_single_or_double_char_token(Punctuator::BitwiseOr, Punctuator::LogicalOr),
            '^' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::BitwiseXor)),

            _ => {
                let (begin, char) = self.current?;
                self.consume_char();
                let end = self.current
                    .map(|(pos, _)| pos)
                    .unwrap_or_else(|| {
                        let length = char.len_utf8();
                        FileLocation::new(self.input.file_id(), begin.offset() + length, begin.line(), begin.column() + length)
                    });
                Some(Token {
                    begin,
                    end,
                    kind: TokenKind::IllegalCharacter(char),
                })
            }
        };

        tok
    }

    fn consume_single_char_token(&mut self, kind: TokenKind) -> Option<Token> {
        let begin = self.current_location();

        self.consume_char();

        let end = self.current_location();

        Some(Token {
            kind,
            begin,
            end,
        })
    }

    fn consume_single_or_double_char_token(&mut self, single: Punctuator, double: Punctuator) -> Option<Token> {
        let begin = self.current_location();

        let char = self.next_char()?;

        let kind = if self.peek_char() == Some(char) {
            self.consume_char();
            TokenKind::Punctuator(double)
        } else {
            TokenKind::Punctuator(single)
        };

        let end = self.current_location();

        Some(Token {
            kind,
            begin,
            end,
        })
    }

    fn consume_character_literal(&mut self) -> Option<Token> {
        let begin = self.current_location();
        assert_eq!(self.next_char().unwrap(), '\'');

        let ch = self.next_char()?;

        if self.peek_char() == Some('\'') {
            self.consume_char();
        } else {
            let location = self.current_location();
            self.errors.push(LexerError {
                location,
                kind: LexerErrorKind::InvalidCharacterLiteral,
            });
        }

        Some(Token {
            kind: TokenKind::CharacterLiteral(ch),
            begin,
            end: self.current_location(),
        })
    }

    fn consume_string(&mut self) -> Option<Token> {
        let begin = self.current_location();

        assert_eq!(self.next_char().unwrap(), '"');

        let offset_begin = self.current_location().offset();

        loop {
            let Some(c) = self.peek_char() else {
                break;
            };

            if c == '"' {
                break;
            }

            self.consume_char();
        }

        let offset_end = self.current_location().offset();
        let str = self.input.slice(offset_begin..offset_end);

        self.consume_char();

        let end = self.current_location();

        Some(Token {
            kind: TokenKind::StringLiteral(str),
            begin,
            end,
        })
    }

    fn consume_template_string(&mut self) -> Option<Token> {
        assert_eq!(self.next_char().unwrap(), '€');
        let begin = self.current_location();

        let location = self.current_location();
        if self.next_char() != Some('"') {
            self.errors.push(LexerError {
                location,
                kind: LexerErrorKind::InvalidTemplateString,
            });

            return Some(Token {
                kind: TokenKind::TemplateString(Vec::new()),
                begin,
                end: begin,
            });
        }

        let mut parts = Vec::new();

        loop {
            let Some(c) = self.peek_char() else {
                break;
            };

            if c == '{' {
                self.consume_char();

                let remaining = &self.input[self.current_location().offset()..];
                if let Some(quote) = remaining.find('"') {
                    let remaining = &remaining[..quote];

                    if !remaining.contains('}') {
                        parts.push(TemplateStringToken::Expression(Vec::new()));
                        continue;
                    }
                }

                let mut tokens = Vec::new();
                loop {
                    let token = self.next()?;

                    if matches!(token.kind, TokenKind::Punctuator(Punctuator::RightCurlyBracket)) {
                        break;
                    }

                    tokens.push(token);
                }

                parts.push(TemplateStringToken::Expression(tokens));
                continue;
            }

            if c == '"' {
                break;
            }

            let begin = self.current_location();
            self.consume_char();
            let end_pos = self.current_location();
            let end = end_pos;

            if let Some(TemplateStringToken::Plain{ begin, end, str }) = parts.last_mut() {
                *end = end_pos;
                *str = self.input.slice(begin.offset()..end_pos.offset());
            } else {
                let str = self.input.slice(begin.offset()..end.offset());
                parts.push(TemplateStringToken::Plain {
                    begin,
                    end,
                    str,
                });
            }
        }

        self.consume_char();
        let end = self.current_location();

        Some(Token {
            kind: TokenKind::TemplateString(parts),
            begin,
            end,
        })
    }

    fn consume_identifier_or_keyword(&mut self) -> Option<Token> {
        let begin = self.current_location();

        loop {
            let Some(c) = self.peek_char() else {
                break;
            };

            if !is_identifier_char(c) {
                break;
            }

            self.consume_char();
        }

        let end = self.current_location();
        let str = self.input.slice(begin.offset()..end.offset());

        let kind = match Keyword::parse(&str) {
            Some(keyword) => TokenKind::Keyword(keyword),
            None => TokenKind::Identifier(str)
        };

        Some(Token {
            kind,
            begin,
            end,
        })
    }

    fn consume_number(&mut self) -> Option<Token> {
        let begin = self.current_location();

        loop {
            let Some(c) = self.peek_char() else {
                break;
            };

            if !('0'..='9').contains(&c) && c != 'x' {
                break;
            }

            self.consume_char();
        }

        let end = self.current_location();
        let mut str = &self.input[begin.offset()..end.offset()];
        let mut radix = 10;

        if str.starts_with("0x") {
            str = &str[2..];
            radix = 16;
        }

        let integer = match i64::from_str_radix(str, radix) {
            Ok(int) => int,
            Err(..) => {
                self.errors.push(LexerError {
                    location: end,
                    kind: LexerErrorKind::InvalidNumber,
                });
                0
            }
        };

        Some(Token {
            kind: TokenKind::Integer(integer),
            begin,
            end,
        })
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek_char() {
            if c == '/' {
                self.consume_char();
                if self.peek_char() != Some('/') {
                    self.consume_single_char_token(TokenKind::Punctuator(Punctuator::Solidus));
                    continue;
                }

                self.consume_until_end_of_line();
                continue;
            }

            if !c.is_whitespace() {
                break;
            }

            self.consume_char();
        }
    }

    fn peek_char(&mut self) -> Option<char> {
        if let Some((_, c)) = self.current {
            return Some(c);
        }

        self.current = self.chars.next()
            .map(|(offset, char)| {
                let location = FileLocation::new(self.input.file_id(), offset, self.line, self.column);

                if char == '\n' {
                    self.line += 1;
                    self.column = 0;
                } else {
                    self.column += 1;
                }

                (location, char)
            });
        Some(self.current?.1)
    }

    fn next_char(&mut self) -> Option<char> {
        let c = self.peek_char()?;
        self.consume_char();
        Some(c)
    }

    fn consume_char(&mut self) {
        self.current = None;
        _ = self.peek_char();
    }

    fn current_location(&mut self) -> FileLocation {
        _ = self.peek_char();
        match self.current {
            Some((location, _)) => location,
            None => FileLocation::new(self.input.file_id(), self.input.len(), self.line, self.column),
        }
    }

    fn handle_solidus(&mut self) -> Option<Token> {
        self.consume_single_char_token(TokenKind::Punctuator(Punctuator::Solidus))
    }

    fn consume_until_end_of_line(&mut self) {
        while let Some(c) = self.next_char() {
            if c == '\n' {
                break;
            }

            if c == '\r' {
                if self.peek_char() == Some('\n') {
                    self.consume_char();
                }

                break;
            }
        }
    }

    pub fn collect_all(mut self) -> (Vec<Token>, Vec<LexerError>) {
        let mut tokens = Vec::new();

        while let Some(token) = self.next() {
            tokens.push(token);
        }

        (tokens, self.errors)
    }

    fn consume_minus_or_arrow(&mut self) -> Option<Token> {
        let begin = self.current_location();

        _ = self.next_char()?;

        let kind = if self.peek_char() == Some('>') {
            self.consume_char();
            TokenKind::Punctuator(Punctuator::Arrow)
        } else {
            TokenKind::Punctuator(Punctuator::HyphenMinus)
        };

        let end = self.current_location();

        Some(Token {
            kind,
            begin,
            end,
        })
    }
}

impl<'source_code> Iterator for Lexer<'source_code> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next()
    }
}

fn is_identifier_char(c: char) -> bool {
    ('a'..='z').contains(&c)
        || ('A'..='Z').contains(&c)
        || ('0'..='9').contains(&c)
        || c == '_'
}

#[derive(Clone, Debug)]
pub struct LexerError {
    pub location: FileLocation,
    pub kind: LexerErrorKind,
}

impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

impl Error for LexerError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Error, AsRefStr)]
pub enum LexerErrorKind {
    #[error("Ongeldige sjabloonslinger")]
    InvalidTemplateString,

    #[error("Ongeldig nummer")]
    InvalidNumber,

    #[error("Ongeldig teken")]
    InvalidCharacterLiteral,
}
impl LexerErrorKind {
    #[must_use]
    pub fn name(&self) -> &str {
        self.as_ref()
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;
    use rstest::rstest;
    use crate::{BabString, FileId};

    #[rstest]
    #[case("h", Token {
        kind: TokenKind::Identifier(BabString::new_static("h")),
        begin: FileLocation::new(FileId::INTERNAL, 0, 0, 0),
        end: FileLocation::new(FileId::INTERNAL, 1, 0, 1),
    })]
    #[case("s ", Token {
        kind: TokenKind::Identifier(BabString::new_static("s")),
        begin: FileLocation::new(FileId::INTERNAL, 0, 0, 0),
        end: FileLocation::new(FileId::INTERNAL, 1, 0, 1),
    })]
    fn next_text(#[case] input: &'static str, #[case] expected: Token) {
        let source_code = SourceCode::new(PathBuf::new(), 0, input.to_string());
        let actual = Lexer::new(&source_code).next();

        assert_eq!(actual, Some(expected));
    }
}
