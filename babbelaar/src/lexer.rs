// Copyright (C) 2023 - 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::str::CharIndices;

use crate::{FileLocation, Keyword, Punctuator, TemplateStringToken, Token, TokenKind};

pub struct Lexer<'source_code> {
    input: &'source_code str,
    chars: CharIndices<'source_code>,

    current: Option<(FileLocation, char)>,
    line: usize,
    column: usize,
}

impl<'source_code> Lexer<'source_code> {
    pub fn new(input: &'source_code str) -> Self {
        Self {
            input,
            chars: input.char_indices(),
            current: None,
            line: 0,
            column: 0,
        }
    }

    pub fn next(&mut self) -> Option<Token<'source_code>> {
        self.skip_whitespace();

        let ch = self.peek_char()?;
        let tok = match ch {
            '"' => self.consume_string(),
            '€' => self.consume_template_string(),

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
            '-' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::HyphenMinus)),
            '/' => self.handle_solidus(),
            '*' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::Asterisk)),
            '%' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::PercentageSign)),
            ':' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::Colon)),
            '.' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::Period)),
            '@' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::AtSign)),

            _ => {
                let (begin, char) = self.current?;
                self.consume_char();
                let end = self.current
                    .map(|(pos, _)| pos)
                    .unwrap_or_else(|| {
                        let length = char.len_utf8();
                        FileLocation::new(begin.offset() + length, begin.line(), begin.column() + length)
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

    fn consume_single_char_token(&mut self, kind: TokenKind<'source_code>) -> Option<Token<'source_code>> {
        let begin = self.current_location();

        self.consume_char();

        let end = self.current_location();

        Some(Token {
            kind,
            begin,
            end,
        })
    }

    fn consume_single_or_double_char_token(&mut self, single: Punctuator, double: Punctuator) -> Option<Token<'source_code>> {
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

    fn consume_string(&mut self) -> Option<Token<'source_code>> {
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
        let str = &self.input[offset_begin..offset_end];

        self.consume_char();

        let end = self.current_location();

        Some(Token {
            kind: TokenKind::StringLiteral(str),
            begin,
            end,
        })
    }

    fn consume_template_string(&mut self) -> Option<Token<'source_code>> {
        assert_eq!(self.next_char().unwrap(), '€');
        let begin = self.current_location();

        assert_eq!(self.next_char().unwrap(), '"');

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
                *str = &self.input[begin.offset()..end_pos.offset()];
            } else {
                let str = &self.input[begin.offset()..end.offset()];
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

    fn consume_identifier_or_keyword(&mut self) -> Option<Token<'source_code>> {
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
        let str = &self.input[begin.offset()..end.offset()];

        let kind = match Keyword::parse(str) {
            Some(keyword) => TokenKind::Keyword(keyword),
            None => TokenKind::Identifier(str)
        };

        Some(Token {
            kind,
            begin,
            end,
        })
    }

    fn consume_number(&mut self) -> Option<Token<'source_code>> {
        let begin = self.current_location();

        loop {
            let Some(c) = self.peek_char() else {
                break;
            };

            if !('0'..='9').contains(&c) {
                break;
            }

            self.consume_char();
        }

        let end = self.current_location();
        let str = &self.input[begin.offset()..end.offset()];
        let integer = str.parse().unwrap();

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
                let location = FileLocation::new(offset, self.line, self.column);

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
            None => FileLocation::new(self.input.len(), self.line, self.column),
        }
    }

    fn handle_solidus(&mut self) -> Option<Token<'source_code>> {
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
}

impl<'source_code> Iterator for Lexer<'source_code> {
    type Item = Token<'source_code>;

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

#[derive(Clone, Debug, thiserror::Error)]
pub enum LexerError {
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case("h", Token {
        kind: TokenKind::Identifier("h"),
        begin: FileLocation::new(0, 0, 0),
        end: FileLocation::new(1, 0, 1),
    })]
    #[case("s ", Token {
        kind: TokenKind::Identifier("s"),
        begin: FileLocation::new(0, 0, 0),
        end: FileLocation::new(1, 0, 1),
    })]
    fn next_text(#[case] input: &'static str, #[case] expected: Token<'static>) {
        let actual = Lexer::new(input).next();

        assert_eq!(actual, Some(expected));
    }
}
