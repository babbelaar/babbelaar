// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::collections::HashMap;

use babbelaar::{Expression, FileRange, ForStatement, Statement, Token, TokenKind};
use strum::EnumIter;
use tower_lsp::lsp_types::{DocumentSymbolResponse, SemanticToken, SemanticTokenType, SymbolInformation, SymbolKind, Url};

use crate::conversion::convert_file_range_to_location;

pub struct Symbolizer {
    uri: Url,
    symbols: HashMap<FileRange, LspSymbol>,
}

impl Symbolizer {
    pub fn new(uri: Url) -> Self {
        Self {
            uri,
            symbols: HashMap::new(),
        }
    }

    pub fn add_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::Expression(expression) => self.add_expression(expression),
            Statement::For(statement) => self.add_statement_for(statement),
            _ => (),
        }
    }

    pub fn add_token(&mut self, token: &Token) {
        self.symbols.insert(token.range(), LspSymbol {
            name: token.kind.name().to_string(),
            kind: (&token.kind).into(),
            range: token.range(),
        });
    }

    pub fn to_response(self) -> DocumentSymbolResponse {
        let mut values: Vec<_> = self.symbols.into_iter().collect();
        values.sort_by(|(range_a, _), (range_b, _)| range_a.cmp(range_b));

        let values = values.into_iter()
            .map(|(_, info)| info)
            .map(|info| info.to_symbol(self.uri.clone()))
            .collect();
        DocumentSymbolResponse::Flat(values)
    }

    pub fn to_tokens(self) -> Vec<SemanticToken> {
        let mut values: Vec<_> = self.symbols.into_iter().collect();
        values.sort_by(|(range_a, _), (range_b, _)| range_a.cmp(range_b));


        let mut tokens = Vec::new();
        let mut previous_range = FileRange::default();

        for (range, symbol) in values {
            tokens.push(symbol.to_semantic(previous_range));
            previous_range = range;
        }

        tokens
    }

    fn add_statement_for(&mut self, statement: &ForStatement) {
        for statement in &statement.body {
            self.add_statement(statement);
        }
    }

    fn add_expression(&mut self, expression: &Expression) {
        match expression {
            Expression::Function(function) => {
                self.symbols.insert(function.function_identifier.range(), LspSymbol {
                    name: function.function_identifier.value().to_string(),
                    kind: LspTokenType::Method,
                    range: function.function_identifier.range(),
                });
            }

            _ => (),
        }
    }
}

struct LspSymbol {
    name: String,
    kind: LspTokenType,
    range: FileRange,
}

impl LspSymbol {
    fn to_semantic(self, previous_range: FileRange) -> SemanticToken {
        let delta_line = (self.range.start().line() - previous_range.start().line()) as u32;

        let mut delta_start = self.range.start().column() as u32;
        if delta_line == 0 {
            delta_start -= previous_range.end().column() as u32;
        }

        let length = (self.range.end().column() - self.range.start().column()) as u32;

        SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type: self.kind.into(),
            token_modifiers_bitset: 0,
        }
    }

    fn to_symbol(self, uri: Url) -> SymbolInformation {
        #[allow(deprecated)]
        SymbolInformation {
            name: self.name,
            kind: self.kind.into(),
            tags: None,
            deprecated: None,
            location: convert_file_range_to_location(uri, self.range),
            container_name: None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumIter)]
#[allow(unused)]
pub enum LspTokenType {
    Function,
    Keyword,
    Number,
    Method,
    Operator,
    String,
    Variable,
}

impl LspTokenType {
    pub fn legend() -> Vec<SemanticTokenType> {
        use strum::IntoEnumIterator;
        Self::iter().map(|x| x.into()).collect()
    }
}

impl<'source_code> From<&TokenKind<'source_code>> for LspTokenType {
    fn from(value: &TokenKind<'source_code>) -> Self {
        match value {
            TokenKind::Keyword(..) => LspTokenType::Keyword,
            TokenKind::StringLiteral(..) => LspTokenType::String,
            TokenKind::TemplateString(..) => LspTokenType::String,
            TokenKind::Identifier(..) => LspTokenType::Variable,
            TokenKind::Integer(..) => LspTokenType::Number,

            TokenKind::Comma => LspTokenType::Operator,
            TokenKind::LeftParenthesis => LspTokenType::Operator,
            TokenKind::RightParenthesis => LspTokenType::Operator,
            TokenKind::LeftCurlyBracket => LspTokenType::Operator,
            TokenKind::RightCurlyBracket => LspTokenType::Operator,
            TokenKind::LeftSquareBracket => LspTokenType::Operator,
            TokenKind::RightSquareBracket => LspTokenType::Operator,
            TokenKind::Semicolon => LspTokenType::Operator,
            TokenKind::PlusSign => LspTokenType::Operator,
            TokenKind::EqualsSign => LspTokenType::Operator,
            TokenKind::HyphenMinus => LspTokenType::Operator,
            TokenKind::Solidus => LspTokenType::Operator,
            TokenKind::Asterisk => LspTokenType::Operator,
            TokenKind::PercentageSign => LspTokenType::Operator,
        }
    }
}

impl From<LspTokenType> for SemanticTokenType {
    fn from(value: LspTokenType) -> Self {
        match value {
            LspTokenType::Function => SemanticTokenType::FUNCTION,
            LspTokenType::Keyword => SemanticTokenType::KEYWORD,
            LspTokenType::Method => SemanticTokenType::METHOD,
            LspTokenType::Number => SemanticTokenType::NUMBER,
            LspTokenType::Operator => SemanticTokenType::OPERATOR,
            LspTokenType::String => SemanticTokenType::STRING,
            LspTokenType::Variable => SemanticTokenType::VARIABLE,
        }
    }
}

impl From<LspTokenType> for u32 {
    fn from(value: LspTokenType) -> Self {
        value as u32
    }
}

impl From<LspTokenType> for SymbolKind {
    fn from(value: LspTokenType) -> Self {
        match value {
            LspTokenType::Function => SymbolKind::FUNCTION,
            LspTokenType::Keyword => SymbolKind::KEY,
            LspTokenType::Method => SymbolKind::METHOD,
            LspTokenType::Number => SymbolKind::NUMBER,
            LspTokenType::Operator => SymbolKind::OPERATOR,
            LspTokenType::String => SymbolKind::STRING,
            LspTokenType::Variable => SymbolKind::VARIABLE,
        }
    }
}
