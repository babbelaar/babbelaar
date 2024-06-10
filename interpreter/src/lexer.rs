// Copyright (C) 2023 Tristan Gerritsen <tristan@thewoosh.org>
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

            'a'..='z' | 'A'..='Z' => self.consume_identifier_or_keyword(),
            '0'..='9' => self.consume_number(),

            '(' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::LeftParenthesis)),
            ')' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::RightParenthesis)),
            '{' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::LeftCurlyBracket)),
            '}' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::RightCurlyBracket)),
            '[' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::LeftSquareBracket)),
            ']' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::RightSquareBracket)),
            ';' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::Semicolon)),
            ',' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::Comma)),
            '=' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::EqualsSign)),
            '+' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::PlusSign)),
            '-' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::HyphenMinus)),
            '/' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::Solidus)),
            '*' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::Asterisk)),
            ':' => self.consume_single_char_token(TokenKind::Punctuator(Punctuator::Colon)),

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
        let begin = self.current?.0;

        self.consume_char();

        let end = self.current?.0;

        Some(Token {
            kind,
            begin,
            end,
        })
    }

    fn consume_string(&mut self) -> Option<Token<'source_code>> {
        assert_eq!(self.next_char().unwrap(), '"');

        let begin = self.current?.0;

        loop {
            let Some(c) = self.peek_char() else {
                break;
            };

            if c == '"' {
                break;
            }

            self.consume_char();
        }

        let end = self.current?.0;
        let str = &self.input[begin.offset()..end.offset()];

        self.consume_char();

        Some(Token {
            kind: TokenKind::StringLiteral(str),
            begin,
            end,
        })
    }

    fn consume_template_string(&mut self) -> Option<Token<'source_code>> {
        assert_eq!(self.next_char().unwrap(), '€');
        let begin = self.current?.0;

        assert_eq!(self.next_char().unwrap(), '"');

        let mut parts = Vec::new();

        loop {
            let Some(c) = self.peek_char() else {
                break;
            };

            if c == '{' {
                self.consume_char();
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

            let begin = self.current?.0;
            self.consume_char();
            let end_pos = self.current?.0;
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
        let end = self.current?.0;

        Some(Token {
            kind: TokenKind::TemplateString(parts),
            begin,
            end,
        })
    }

    fn consume_identifier_or_keyword(&mut self) -> Option<Token<'source_code>> {
        let begin = self.current?.0;

        loop {
            let Some(c) = self.peek_char() else {
                break;
            };

            if !is_identifier_char(c) {
                break;
            }

            self.consume_char();
        }

        let end = self.current?.0;
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
        let begin = self.current?.0;

        loop {
            let Some(c) = self.peek_char() else {
                break;
            };

            if !('0'..='9').contains(&c) {
                break;
            }

            self.consume_char();
        }

        let end = self.current?.0;
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
