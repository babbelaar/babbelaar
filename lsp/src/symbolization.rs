// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::collections::HashMap;

use babbelaar::{AssignStatement, Attribute, Expression, Field, FileRange, ForStatement, FunctionStatement, IfStatement, Method, OptionExt, Parameter, ParseTree, PostfixExpression, PostfixExpressionKind, PrimaryExpression, ReturnStatement, SemanticAnalyzer, SemanticLocalKind, SourceCode, Statement, StatementKind, Structure, StructureInstantiationExpression, TemplateStringExpressionPart, TemplateStringToken, Token, TokenKind, VariableStatement};
use log::error;
use strum::EnumIter;
use tower_lsp::lsp_types::{DocumentSymbolResponse, SemanticToken, SemanticTokenType, SymbolInformation, SymbolKind, Uri};

use crate::conversion::convert_file_range_to_location;

pub struct Symbolizer {
    uri: Uri,
    symbols: SymbolMap,
    semantic_analyzer: SemanticAnalyzer,
}

impl Symbolizer {
    pub fn new(uri: Uri, source_code: &SourceCode) -> Self {
        Self {
            uri,
            symbols: SymbolMap::default(),
            semantic_analyzer: SemanticAnalyzer::new_single(source_code),
        }
    }

    pub fn add_tree(&mut self, tree: &ParseTree) {
        self.semantic_analyzer.analyze_tree_phase_1(tree);
        self.semantic_analyzer.analyze_tree_phase_2(tree);

        for statement in tree.all() {
            self.add_statement(statement);
        }
    }

    fn add_statement(&mut self, statement: &Statement) {
        self.add_attributes(&statement.attributes);

        match &statement.kind {
            StatementKind::Assignment(statement) => self.add_statement_assign(statement),
            StatementKind::Expression(expression) => self.add_expression(expression),
            StatementKind::For(statement) => self.add_statement_for(statement),
            StatementKind::Function(statement) => self.add_statement_function(statement),
            StatementKind::If(statement) => self.add_statement_if(statement),
            StatementKind::Return(statement) => self.add_statement_return(statement),
            StatementKind::Structure(statement) => self.add_statement_structure(statement),
            StatementKind::Variable(statement) => self.add_statement_variable(statement),
        }
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

    fn add_statement_assign(&mut self, statement: &AssignStatement) {
        self.add_expression(&statement.expression);
    }

    fn add_statement_for(&mut self, statement: &ForStatement) {
        for statement in &statement.body {
            self.add_statement(statement);
        }
    }

    fn add_statement_function(&mut self, statement: &FunctionStatement) {
        self.symbols.insert(LspSymbol {
            name: statement.name.value().to_string(),
            kind: LspTokenType::Method,
            range: statement.name.range(),
        });

        if let Some(return_type) = &statement.return_type {
            self.symbols.insert(LspSymbol {
                name: return_type.specifier.name().to_string(),
                kind: LspTokenType::Class,
                range: return_type.range(),
            });
        }

        for parameter in &statement.parameters {
            self.add_parameter(parameter);
        }

        for statement in statement.body.as_inner_slice() {
            self.add_statement(statement);
        }
    }

    fn add_statement_if(&mut self, statement: &IfStatement) {
        self.add_expression(&statement.condition);

        for statement in &statement.body {
            self.add_statement(statement);
        }
    }

    fn add_statement_return(&mut self, statement: &ReturnStatement) {
        if let Some(expr) = &statement.expression {
            self.add_expression(expr);
        }
    }

    fn add_statement_structure(&mut self, statement: &Structure) {
        self.symbols.insert(LspSymbol {
            name: statement.name.value().to_string(),
            kind: LspTokenType::Class,
            range: statement.name.range(),
        });

        for field in &statement.fields {
            self.add_structure_field(field);
        }

        for field in &statement.methods {
            self.add_structure_method(field);
        }
    }

    fn add_structure_field(&mut self, field: &Field) {
        self.add_attributes(&field.attributes);

        self.symbols.insert(LspSymbol {
            name: field.name.value().to_string(),
            kind: LspTokenType::Property,
            range: field.name.range(),
        });

        self.symbols.insert(LspSymbol {
            name: field.ty.specifier.name().to_string(),
            kind: LspTokenType::Class,
            range: field.ty.range(),
        });
    }

    fn add_structure_method(&mut self, method: &Method) {
        self.add_statement_function(&method.function);
    }

    fn add_statement_variable(&mut self, statement: &VariableStatement) {
        self.add_expression(&statement.expression);
    }

    fn add_parameter(&mut self, parameter: &Parameter) {
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

    fn add_attribute(&mut self, attribute: &Attribute) {
        self.symbols.remove(attribute.at_range);
        self.symbols.remove(attribute.name.range());
    }

    fn add_attributes(&mut self, attributes: &[Attribute]) {
        for attribute in attributes {
            self.add_attribute(attribute);
        }
    }

    fn add_expression(&mut self, expression: &Expression) {
        match expression {
            Expression::Postfix(expression) => self.add_expression_postfix(expression),

            Expression::Primary(PrimaryExpression::Reference(identifier)) => {
                if let Some(reference) = self.semantic_analyzer.find_reference(identifier.range()) {
                    self.symbols.insert(LspSymbol {
                        name: identifier.value().to_string(),
                        kind: match reference.local_kind {
                            SemanticLocalKind::FieldReference => LspTokenType::Property,
                            SemanticLocalKind::Iterator => LspTokenType::Variable,
                            SemanticLocalKind::StructureReference => LspTokenType::Class,
                            SemanticLocalKind::Parameter => LspTokenType::ParameterName,
                            SemanticLocalKind::Function => LspTokenType::Function,
                            SemanticLocalKind::FunctionReference => LspTokenType::Function,
                            SemanticLocalKind::Method => LspTokenType::Method,
                            SemanticLocalKind::Variable => LspTokenType::Variable,
                            SemanticLocalKind::ReferenceThis => LspTokenType::ParameterName,
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

            Expression::Primary(PrimaryExpression::StructureInstantiation(instantiation)) => {
                self.add_expression_structure_instantiation(instantiation);
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

    fn add_expression_structure_instantiation(&mut self, expression: &StructureInstantiationExpression) {
        self.symbols.insert(LspSymbol {
            name: expression.name.value().to_string(),
            kind: LspTokenType::Class,
            range: expression.name.range(),
        });
    }
}

#[derive(Debug, Default)]
struct SymbolMap {
    map: HashMap<FileRange, LspSymbol>
}

impl SymbolMap {
    pub fn remove(&mut self, range: FileRange) {
        self.map.remove(&range);
    }

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

        // eprintln!("sym_begin={:?} range={range:?}", sym_begin.range);

        if sym_begin.range.start() < range.start() {
            sym_begin.range = FileRange::new(sym_begin.range.start(), range.start());
        } else {
            sym_begin.range = FileRange::new(range.start(), sym_begin.range.start());
        }

        if sym_end.range.end() < range.end() {
            sym_end.range = FileRange::new(sym_end.range.end(), range.end());
        } else {
            sym_end.range = FileRange::new(range.end(), sym_end.range.end());
        }

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
    fn to_symbol(self, uri: Uri) -> SymbolInformation {
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
    Attribute,
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

impl From<&TokenKind> for LspTokenType {
    fn from(value: &TokenKind) -> Self {
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
            LspTokenType::Attribute => SemanticTokenType::MACRO,
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
            LspTokenType::Attribute => SymbolKind::KEY,
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
    use std::path::PathBuf;

    use super::*;
    use babbelaar::{BabString, Lexer, SourceCode};
    use rstest::rstest;

    #[rstest]
    #[case(
        "€\"Hallo {naam}\""
    )]
    fn test_add_token(#[case] input: &'static str) {
        let input = SourceCode::new(PathBuf::new(), 0, BabString::new_static(input));
        let tokens: Vec<Token> = Lexer::new(&input).collect();

        let mut symbolizer = Symbolizer::new("file:///test.h".parse().unwrap(), &input);
        for token in &tokens {
            symbolizer.add_token(token);
        }

        for (range, symbol) in symbolizer.symbols.map {
            eprintln!("Range={range:?}    symbol={symbol:#?}");
        }
    }
}
