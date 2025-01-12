// Copyright (C) 2023 - 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{error::Error, fmt::Display, str::CharIndices};

use strum::AsRefStr;
use thiserror::Error;

use crate::{BabString, FileLocation, Keyword, Punctuator, Slice, SourceCode, TemplateStringToken, Token, TokenKind};

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
            '+' => self.consume_assign_or_normal_token(Punctuator::PlusSign, Punctuator::AddAssign),
            '-' => self.consume_minus(),
            '/' => self.handle_solidus(),
            '*' => self.consume_assign_or_normal_token(Punctuator::Asterisk, Punctuator::MultiplyAssign),
            '%' => self.consume_assign_or_normal_token(Punctuator::PercentageSign, Punctuator::ModuloAssign),
            ':' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::Colon)),
            '.' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::Period)),
            '@' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::AtSign)),
            '<' => self.consume_quadruple(Punctuator::LessThan, Punctuator::LessThanOrEqual, Punctuator::LeftShift, Punctuator::LeftShiftAssign),
            '>' => self.consume_quadruple(Punctuator::GreaterThan, Punctuator::GreaterThanOrEqual, Punctuator::RightShift, Punctuator::RightShiftAssign),
            '&' => self.consume_single_or_double_char_token(Punctuator::BitwiseAnd, Punctuator::LogicalAnd),
            '|' => self.consume_single_or_double_char_token(Punctuator::BitwiseOr, Punctuator::LogicalOr),
            '^' => self.consume_assign_or_normal_token(Punctuator::BitwiseXor, Punctuator::BitwiseXorAssign),
            '!' => self.consume_exclamation_mark(),

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

    #[must_use]
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

    fn consume_assign_or_normal_token(&mut self, normal: Punctuator, assign: Punctuator) -> Option<Token> {
        let begin = self.current_location();
        self.consume_char();

        let kind = if self.peek_char() == Some('=') {
            self.consume_char();
            TokenKind::Punctuator(assign)
        } else {
            TokenKind::Punctuator(normal)
        };

        let end = self.current_location();

        Some(Token {
            kind,
            begin,
            end,
        })
    }

    fn consume_exclamation_mark(&mut self) -> Option<Token> {
        let begin = self.current_location();
        self.consume_char();

        let kind = if self.peek_char() == Some('=') {
            self.consume_char();
            TokenKind::Punctuator(Punctuator::NotEquals)
        } else {
            TokenKind::Punctuator(Punctuator::Not)
        };

        let end = self.current_location();

        Some(Token {
            kind,
            begin,
            end,
        })
    }

    fn consume_quadruple(&mut self, normal: Punctuator, normal_assign: Punctuator, double: Punctuator, double_assign: Punctuator) -> Option<Token> {
        let begin = self.current_location();
        let ch = self.next_char()?;

        let punctuator = if self.peek_char() == Some(ch) {
            self.consume_char();

            if self.peek_char() == Some('=') {
                self.consume_char();
                double_assign
            } else {
                double
            }
        } else if self.peek_char() == Some('=') {
            self.consume_char();
            normal_assign
        } else {
            normal
        };

        let end = self.current_location();

        Some(Token {
            kind: TokenKind::Punctuator(punctuator),
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

        let mut str = None;

        loop {
            let Some(c) = self.peek_char() else {
                break;
            };

            if c == '"' {
                break;
            }

            let pos = self.current_location().offset();
            self.consume_char();

            if c == '\\' {
                let mut buf = str.take().unwrap_or_else(|| self.input[offset_begin..pos].to_string());

                match self.peek_char() {
                    Some('"') => buf.push('"'),
                    Some('n') => buf.push('\n'),
                    Some('r') => buf.push('\r'),
                    Some('t') => buf.push('\t'),
                    Some('\\') => buf.push('\\'),

                    Some(invalid) => {
                        buf.push('\\');
                        buf.push(invalid);

                        let location = self.current_location();
                        self.errors.push(LexerError {
                            location,
                            kind: LexerErrorKind::InvalidEscape { invalid },
                        });
                    }

                    None => break,
                }

                self.consume_char();
                str = Some(buf);
            } else if let Some(buf) = &mut str {
                buf.push(c);
            }
        }

        let offset_end = self.current_location().offset();
        let str = match str {
            Some(str) => BabString::new(str),
            None => self.input.slice(offset_begin..offset_end),
        };

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

            if !c.is_ascii_hexdigit() && c != 'x' {
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

        let integer = match u64::from_str_radix(str, radix) {
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
            kind: TokenKind::Integer(integer as i64),
            begin,
            end,
        })
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek_char() {
            if c == '/' {
                break;
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
        let token = self.consume_single_char_token(TokenKind::Punctuator(Punctuator::Solidus))?;

        match self.peek_char() {
            Some('/') => {
                self.consume_until_end_of_line();
                self.next()
            }

            Some('=') => {
                self.consume_char();
                Some(Token {
                    kind: TokenKind::Punctuator(Punctuator::DivideAssign),
                    begin: token.begin,
                    end: self.current_location(),
                })
            }

            _ => Some(token),
        }
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

    fn consume_minus(&mut self) -> Option<Token> {
        let begin = self.current_location();

        _ = self.next_char()?;

        let kind = match self.peek_char() {
            Some('>') => {
                self.consume_char();
                TokenKind::Punctuator(Punctuator::Arrow)
            }

            Some('=') => {
                self.consume_char();
                TokenKind::Punctuator(Punctuator::AddAssign)
            }

            _ => {
                TokenKind::Punctuator(Punctuator::HyphenMinus)
            }
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

#[derive(Clone, Debug, PartialEq, Eq)]
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

    #[error("Ongeldige ontsnappingscode `\\{invalid}`, alleen `\\\"`, `\\\\`, `\\n`, `\\r` en `\\t` zijn toegestaan")]
    InvalidEscape { invalid: char },
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
    #[case("\"Hal\\nlo\" ", Token {
        kind: TokenKind::StringLiteral(BabString::new_static("Hal\nlo")),
        begin: FileLocation::new(FileId::INTERNAL, 0, 0, 0),
        end: FileLocation::new(FileId::INTERNAL, 9, 0, 9),
    })]
    #[case("\"Hal\\tlo\" ", Token {
        kind: TokenKind::StringLiteral(BabString::new_static("Hal\tlo")),
        begin: FileLocation::new(FileId::INTERNAL, 0, 0, 0),
        end: FileLocation::new(FileId::INTERNAL, 9, 0, 9),
    })]
    #[case("\"Hal\\rlo\" ", Token {
        kind: TokenKind::StringLiteral(BabString::new_static("Hal\rlo")),
        begin: FileLocation::new(FileId::INTERNAL, 0, 0, 0),
        end: FileLocation::new(FileId::INTERNAL, 9, 0, 9),
    })]
    #[case("\"Hal\\r\\nlo\" ", Token {
        kind: TokenKind::StringLiteral(BabString::new_static("Hal\r\nlo")),
        begin: FileLocation::new(FileId::INTERNAL, 0, 0, 0),
        end: FileLocation::new(FileId::INTERNAL, 11, 0, 11),
    })]
    #[case("< 1", Token {
        kind: TokenKind::Punctuator(Punctuator::LessThan),
        begin: FileLocation::new(FileId::INTERNAL, 0, 0, 0),
        end: FileLocation::new(FileId::INTERNAL, 1, 0, 1),
    })]
    #[case("> 1", Token {
        kind: TokenKind::Punctuator(Punctuator::GreaterThan),
        begin: FileLocation::new(FileId::INTERNAL, 0, 0, 0),
        end: FileLocation::new(FileId::INTERNAL, 1, 0, 1),
    })]
    #[case("<= 1", Token {
        kind: TokenKind::Punctuator(Punctuator::LessThanOrEqual),
        begin: FileLocation::new(FileId::INTERNAL, 0, 0, 0),
        end: FileLocation::new(FileId::INTERNAL, 2, 0, 2),
    })]
    #[case(">= 1", Token {
        kind: TokenKind::Punctuator(Punctuator::GreaterThanOrEqual),
        begin: FileLocation::new(FileId::INTERNAL, 0, 0, 0),
        end: FileLocation::new(FileId::INTERNAL, 2, 0, 2),
    })]
    #[case("<< 1", Token {
        kind: TokenKind::Punctuator(Punctuator::LeftShift),
        begin: FileLocation::new(FileId::INTERNAL, 0, 0, 0),
        end: FileLocation::new(FileId::INTERNAL, 2, 0, 2),
    })]
    #[case(">> 1", Token {
        kind: TokenKind::Punctuator(Punctuator::RightShift),
        begin: FileLocation::new(FileId::INTERNAL, 0, 0, 0),
        end: FileLocation::new(FileId::INTERNAL, 2, 0, 2),
    })]
    #[case("<<= 1", Token {
        kind: TokenKind::Punctuator(Punctuator::LeftShiftAssign),
        begin: FileLocation::new(FileId::INTERNAL, 0, 0, 0),
        end: FileLocation::new(FileId::INTERNAL, 3, 0, 3),
    })]
    #[case(">>= 1", Token {
        kind: TokenKind::Punctuator(Punctuator::RightShiftAssign),
        begin: FileLocation::new(FileId::INTERNAL, 0, 0, 0),
        end: FileLocation::new(FileId::INTERNAL, 3, 0, 3),
    })]
    #[case("* 1", Token {
        kind: TokenKind::Punctuator(Punctuator::Asterisk),
        begin: FileLocation::new(FileId::INTERNAL, 0, 0, 0),
        end: FileLocation::new(FileId::INTERNAL, 1, 0, 1),
    })]
    #[case("*= 1", Token {
        kind: TokenKind::Punctuator(Punctuator::MultiplyAssign),
        begin: FileLocation::new(FileId::INTERNAL, 0, 0, 0),
        end: FileLocation::new(FileId::INTERNAL, 2, 0, 2),
    })]
    #[case("/ 1", Token {
        kind: TokenKind::Punctuator(Punctuator::Solidus),
        begin: FileLocation::new(FileId::INTERNAL, 0, 0, 0),
        end: FileLocation::new(FileId::INTERNAL, 1, 0, 1),
    })]
    #[case("/= 1", Token {
        kind: TokenKind::Punctuator(Punctuator::DivideAssign),
        begin: FileLocation::new(FileId::INTERNAL, 0, 0, 0),
        end: FileLocation::new(FileId::INTERNAL, 2, 0, 2),
    })]
    #[case("= 1", Token {
        kind: TokenKind::Punctuator(Punctuator::Assignment),
        begin: FileLocation::new(FileId::INTERNAL, 0, 0, 0),
        end: FileLocation::new(FileId::INTERNAL, 1, 0, 1),
    })]
    #[case("== 1", Token {
        kind: TokenKind::Punctuator(Punctuator::Equals),
        begin: FileLocation::new(FileId::INTERNAL, 0, 0, 0),
        end: FileLocation::new(FileId::INTERNAL, 2, 0, 2),
    })]
    #[case("0x80", Token {
        kind: TokenKind::Integer(128),
        begin: FileLocation::new(FileId::INTERNAL, 0, 0, 0),
        end: FileLocation::new(FileId::INTERNAL, 4, 0, 4),
    })]
    fn next_text(#[case] input: &'static str, #[case] expected: Token) {
        let source_code = SourceCode::new(PathBuf::new(), 0, input.to_string());
        let mut lexer = Lexer::new(&source_code);
        let mut actual = lexer.next();

        assert_eq!(lexer.errors, Vec::new());

        if let Some(actual) = &mut actual {
            actual.begin = FileLocation::new(FileId::INTERNAL, actual.begin.offset(), actual.begin.line(), actual.begin.column());
            actual.end = FileLocation::new(FileId::INTERNAL, actual.end.offset(), actual.end.line(), actual.end.column());
        }

        assert_eq!(actual, Some(expected));
    }
}
