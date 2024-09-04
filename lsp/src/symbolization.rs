// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::collections::HashMap;

use babbelaar::{Expression, FileRange, ForStatement, FunctionStatement, IfStatement, OptionExt, Parameter, PostfixExpression, PostfixExpressionKind, PrimaryExpression, ReturnStatement, SemanticAnalyzer, SemanticLocalKind, Statement, StatementKind, Structure, TemplateStringExpressionPart, TemplateStringToken, Token, TokenKind, VariableStatement};
use log::error;
use strum::EnumIter;
use tower_lsp::lsp_types::{DocumentSymbolResponse, SemanticToken, SemanticTokenType, SymbolInformation, SymbolKind, Url};

use crate::conversion::convert_file_range_to_location;

pub struct Symbolizer<'source_code> {
    uri: Url,
    symbols: SymbolMap,
    semantic_analyzer: SemanticAnalyzer<'source_code>,
}

impl<'source_code> Symbolizer<'source_code> {
    pub fn new(uri: Url) -> Self {
        Self {
            uri,
            symbols: SymbolMap::default(),
            semantic_analyzer: SemanticAnalyzer::new(),
        }
    }

    pub fn add_statement(&mut self, statement: &'source_code Statement<'source_code>) {
        match &statement.kind {
            StatementKind::Expression(expression) => self.add_expression(expression),
            StatementKind::For(statement) => self.add_statement_for(statement),
            StatementKind::Function(statement) => self.add_statement_function(statement),
            StatementKind::If(statement) => self.add_statement_if(statement),
            StatementKind::Return(statement) => self.add_statement_return(statement),
            StatementKind::Structure(statement) => self.add_statement_structure(statement),
            StatementKind::Variable(statement) => self.add_statement_variable(statement),
        }

        self.semantic_analyzer.analyze_statement(statement);
    }

    pub fn add_token(&mut self, token: &Token) {
        self.symbols.insert(LspSymbol {
            name: token.kind.name().to_string(),
            kind: (&token.kind).into(),
            range: token.range(),
        });

        match &token.kind {
            TokenKind::TemplateString(ts) => {
                for part in ts.iter() {
                    if let TemplateStringToken::Expression(expr) = part {
                        for token in expr.iter() {
                            self.add_token(token);
                        }
                    }
                }
            }

            _ => (),
        }
    }

    pub fn to_response(self) -> DocumentSymbolResponse {
        let mut values: Vec<_> = self.symbols.to_vec();
        values.sort_by(|(range_a, _), (range_b, _)| range_a.start().offset().cmp(&range_b.start().offset()));

        let values = values.into_iter()
            .map(|(_, info)| info)
            .map(|info| info.to_symbol(self.uri.clone()))
            .collect();
        DocumentSymbolResponse::Flat(values)
    }

    pub fn to_tokens(self) -> Vec<SemanticToken> {
        let mut values: Vec<_> = self.symbols.to_vec();
        values.sort_by(|(range_a, _), (range_b, _)| range_a.cmp(range_b));


        let mut tokens = Vec::new();
        let mut previous_range = FileRange::default();

        for (range, symbol) in values {
            tokens.push(symbol.to_semantic(previous_range));
            previous_range = range;
        }

        tokens
    }

    fn add_statement_for(&mut self, statement: &'source_code ForStatement<'source_code>) {
        for statement in &statement.body {
            self.add_statement(statement);
        }
    }

    fn add_statement_function(&mut self, statement: &'source_code FunctionStatement<'source_code>) {
        self.symbols.insert(LspSymbol {
            name: statement.name.value().to_string(),
            kind: LspTokenType::Method,
            range: statement.name.range(),
        });

        for parameter in &statement.parameters {
            self.add_parameter(parameter);
        }

        for statement in statement.body.as_inner_slice() {
            self.add_statement(statement);
        }
    }

    fn add_statement_if(&mut self, statement: &'source_code IfStatement<'source_code>) {
        self.add_expression(&statement.condition);

        for statement in &statement.body {
            self.add_statement(statement);
        }
    }

    fn add_statement_return(&mut self, statement: &'source_code ReturnStatement<'source_code>) {
        if let Some(expr) = &statement.expression {
            self.add_expression(expr);
        }
    }

    fn add_statement_structure(&mut self, statement: &'source_code Structure<'source_code>) {
        _ = statement;
    }

    fn add_statement_variable(&mut self, statement: &'source_code VariableStatement<'source_code>) {
        self.add_expression(&statement.expression);
    }

    fn add_parameter(&mut self, parameter: &'source_code Parameter<'source_code>) {
        self.symbols.insert(LspSymbol {
            name: parameter.name.value().to_string(),
            kind: LspTokenType::ParameterName,
            range: parameter.name.range(),
        });

        self.symbols.insert(LspSymbol {
            name: "Type".to_string(),
            kind: LspTokenType::Class,
            range: parameter.ty.range(),
        });
    }

    fn add_expression(&mut self, expression: &'source_code Expression<'source_code>) {
        match expression {
            Expression::Postfix(expression) => self.add_expression_postfix(expression),

            Expression::Primary(PrimaryExpression::Reference(identifier)) => {
                if let Some(reference) = self.semantic_analyzer.find_reference(identifier.range()) {
                    self.symbols.insert(LspSymbol {
                        name: identifier.value().to_string(),
                        kind: match reference.local_kind {
                            SemanticLocalKind::Iterator => LspTokenType::Variable,
                            SemanticLocalKind::Parameter => LspTokenType::ParameterName,
                            SemanticLocalKind::Function => LspTokenType::Function,
                            SemanticLocalKind::FunctionReference => LspTokenType::Function,
                            SemanticLocalKind::Variable => LspTokenType::Variable,
                        },
                        range: identifier.range(),
                    });
                }
            }

            Expression::BiExpression(expr) => {
                self.add_expression(&expr.lhs);
                self.add_expression(&expr.rhs);
            }

            Expression::Primary(PrimaryExpression::TemplateString { parts }) => {
                for part in parts {
                    if let TemplateStringExpressionPart::Expression(expression) = part {
                        self.add_expression(expression);
                    }
                }
            }

            Expression::Primary(..) => (),
        }
    }

    fn add_expression_postfix(&mut self, expression: &PostfixExpression) {
        match &expression.kind {
            PostfixExpressionKind::Call(..) => {
                if let Expression::Primary(PrimaryExpression::Reference(ident)) = expression.lhs.value() {
                    self.symbols.insert(LspSymbol {
                        name: ident.value().to_string(),
                        kind: LspTokenType::Function,
                        range: ident.range(),
                    });
                }
            }

            PostfixExpressionKind::MethodCall(method) => {
                self.symbols.insert(LspSymbol {
                    name: method.method_name.value().to_string(),
                    kind: LspTokenType::Method,
                    range: method.method_name.range(),
                });
            }

            PostfixExpressionKind::Member(member) => {
                self.symbols.insert(LspSymbol {
                    name: member.value().to_string(),
                    kind: LspTokenType::Property,
                    range: member.range(),
                });
            }
        }
    }
}

#[derive(Debug, Default)]
struct SymbolMap {
    map: HashMap<FileRange, LspSymbol>
}

impl SymbolMap {
    pub fn insert(&mut self, sym: LspSymbol) {
        let range = sym.range;
        self.map.insert(sym.range, sym);

        let Some(key) = self.map.keys().find(|x| {
                    let x = x.start().offset()..x.end().offset();
                    x.contains(&range.start().offset()) || x.contains(&range.end().offset().saturating_sub(1))
                }).copied() else {
            return;
        };

        if key == range {
            return;
        }

        let Some(mut sym_begin) = self.map.remove(&key) else {
            error!("Kon zojuist gevonden symbool niet verwijderen op index?");
            debug_assert!(false);
            return;
        };
        let mut sym_end = sym_begin.clone();

        eprintln!("sym_begin={:?} range={range:?}", sym_begin.range);

        sym_begin.range = FileRange::new(sym_begin.range.start(), range.start());
        sym_end.range = FileRange::new(range.end(), sym_end.range.end());

        let is_begin_empty = sym_begin.range.start() == sym_begin.range.end();
        if !is_begin_empty {
            self.map.insert(sym_begin.range, sym_begin);
        }

        let is_end_empty = sym_end.range.start() == sym_end.range.end();
        if !is_end_empty {
            self.map.insert(sym_end.range, sym_end);
        }
    }

    pub fn to_vec(self) -> Vec<(FileRange, LspSymbol)> {
        self.map.into_iter().collect()
    }
}

#[derive(Debug, Clone, PartialEq)]
struct LspSymbol {
    name: String,
    kind: LspTokenType,
    range: FileRange,
}

impl LspSymbol {
    #[must_use]
    fn to_semantic(self, previous_range: FileRange) -> SemanticToken {
        let delta_line = (self.range.start().line() - previous_range.start().line()) as u32;

        let mut delta_start = self.range.start().column() as u32;
        if delta_line == 0 {
            delta_start -= previous_range.start().column() as u32;
        }

        let length = self.range.len() as u32;

        SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type: self.kind.into(),
            token_modifiers_bitset: 0,
        }
    }

    #[must_use]
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
    ParameterName,
    Class,
    Property,
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

            TokenKind::Punctuator(..) => LspTokenType::Operator,
            TokenKind::IllegalCharacter(..) => LspTokenType::Operator,
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
            LspTokenType::ParameterName => SemanticTokenType::PARAMETER,
            LspTokenType::Class => SemanticTokenType::CLASS,
            LspTokenType::Property => SemanticTokenType::PROPERTY,
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
            LspTokenType::ParameterName => SymbolKind::PROPERTY,
            LspTokenType::Class => SymbolKind::CLASS,
            LspTokenType::Property => SymbolKind::PROPERTY,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use babbelaar::Lexer;
    use rstest::rstest;

    #[rstest]
    #[case(
        "€\"Hallo {naam}\""
    )]
    fn test_add_token(#[case] input: &'static str) {
        let tokens: Vec<Token<'static>> = Lexer::new(input).collect();

        let mut symbolizer = Symbolizer::new(Url::parse("file:///test.h").unwrap());
        for token in &tokens {
            symbolizer.add_token(token);
        }

        for (range, symbol) in symbolizer.symbols.map {
            eprintln!("Range={range:?}    symbol={symbol:#?}");
        }
    }
}
