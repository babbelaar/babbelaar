// Copyright (C) 2023 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::str::CharIndices;

use crate::{Token, TokenKind, Keyword};

pub struct Lexer<'source_code> {
    input: &'source_code str,
    chars: CharIndices<'source_code>,

    current: Option<(usize, char)>,
}

impl<'source_code> Lexer<'source_code> {
    pub fn new(input: &'source_code str) -> Self {
        Self {
            input,
            chars: input.char_indices(),
            current: None,
        }
    }

    pub fn next(&mut self) -> Option<Token<'source_code>> {
        self.skip_whitespace();

        match self.peek_char()? {
            '"' => self.consume_string(),
            'a'..='z' | 'A'..='Z' => self.consume_identifier_or_keyword(),
            '0'..='9' => self.consume_number(),

            '(' => self.consume_single_char_token(TokenKind::LeftParenthesis),
            ')' => self.consume_single_char_token(TokenKind::RightParenthesis),
            ';' => self.consume_single_char_token(TokenKind::Semicolon),
            ',' => self.consume_single_char_token(TokenKind::Comma),
            '=' => self.consume_single_char_token(TokenKind::EqualsSign),
            '+' => self.consume_single_char_token(TokenKind::PlusSign),
            '-' => self.consume_single_char_token(TokenKind::HyphenMinus),
            '/' => self.consume_single_char_token(TokenKind::Solidus),
            '*' => self.consume_single_char_token(TokenKind::Asterisk),

            unknown_char => panic!("Invalid char: '{unknown_char}' (U+{:X}", unknown_char as u32),
        }
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
            let c = self.peek_char().unwrap();

            if c == '"' {
                break;
            }

            self.consume_char();
        }

        let end = self.current?.0;
        let str = &self.input[begin..end];

        self.consume_char();

        Some(Token {
            kind: TokenKind::StringLiteral(str),
            begin,
            end,
        })
    }

    fn consume_identifier_or_keyword(&mut self) -> Option<Token<'source_code>> {
        let begin = self.current?.0;

        loop {
            let c = self.peek_char().unwrap();
            if !is_identifier_char(c) {
                break;
            }

            self.consume_char();
        }

        let end = self.current?.0;
        let str = &self.input[begin..end];

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
            let c = self.peek_char().unwrap();
            if !('0'..='9').contains(&c) {
                break;
            }

            self.consume_char();
        }

        let end = self.current?.0;
        let str = &self.input[begin..end];
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

        self.current = self.chars.next();
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
