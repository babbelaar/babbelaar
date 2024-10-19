// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::collections::HashMap;

use babbelaar::{AssignStatement, Attribute, BabString, Expression, ExtensionStatement, Field, FileRange, ForIterableKind, ForStatement, FunctionStatement, IfStatement, InterfaceSpecifier, InterfaceStatement, Keyword, Method, OptionExt, Parameter, ParseTree, PostfixExpression, PostfixExpressionKind, PrimaryExpression, Ranged, ReturnStatement, SemanticAnalysisPhase, SemanticAnalyzer, SemanticLocalKind, SourceCode, Statement, StatementKind, Structure, StructureInstantiationExpression, TemplateStringExpressionPart, TemplateStringToken, Token, TokenKind, Type, TypeSpecifier, VariableStatement};
use log::error;
use strum::EnumIter;
use tower_lsp::lsp_types::{DocumentSymbolResponse, SemanticToken, SemanticTokenModifier, SemanticTokenType, SymbolInformation, SymbolKind, Uri};

use crate::Converter;

pub struct Symbolizer {
    uri: Uri,
    converter: Converter,
    symbols: SymbolMap,
    semantic_analyzer: SemanticAnalyzer,
}

impl Symbolizer {
    pub fn new(uri: Uri, source_code: &SourceCode, converter: Converter) -> Self {
        Self {
            uri,
            converter,
            symbols: SymbolMap::default(),
            semantic_analyzer: SemanticAnalyzer::new_single(source_code, false),
        }
    }

    pub fn add_tree(&mut self, tree: &ParseTree) {
        for phase in SemanticAnalysisPhase::iter() {
            self.semantic_analyzer.analyze_tree(tree, phase);
        }

        for statement in tree.all() {
            self.add_statement(statement);
        }
    }

    fn add_statement(&mut self, statement: &Statement) {
        self.add_attributes(&statement.attributes);

        match &statement.kind {
            StatementKind::Assignment(statement) => self.add_statement_assign(statement),
            StatementKind::Expression(expression) => self.add_expression(expression),
            StatementKind::Extension(extension) => self.add_extension(extension),
            StatementKind::For(statement) => self.add_statement_for(statement),
            StatementKind::Function(statement) => self.add_statement_function(statement),
            StatementKind::If(statement) => self.add_statement_if(statement),
            StatementKind::Interface(statement) => self.add_statement_interface(statement),
            StatementKind::Return(statement) => self.add_statement_return(statement),
            StatementKind::Structure(statement) => self.add_statement_structure(statement),
            StatementKind::Variable(statement) => self.add_statement_variable(statement),
        }
    }

    pub fn add_token(&mut self, token: &Token) {
        self.symbols.insert(LspSymbol {
            name: BabString::new_static(token.kind.name()),
            kind: (&token.kind).into(),
            range: token.range(),
            modifier: match token.kind {
                TokenKind::Keyword(Keyword::Als) => LspSymbolModifier::ControlFlow,
                TokenKind::Keyword(Keyword::Bekeer) => LspSymbolModifier::ControlFlow,
                TokenKind::Keyword(Keyword::In) => LspSymbolModifier::ControlFlow,
                TokenKind::Keyword(Keyword::Reeks) => LspSymbolModifier::ControlFlow,
                TokenKind::Keyword(Keyword::Volg) => LspSymbolModifier::ControlFlow,
                _ => LspSymbolModifier::None,
            },
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

    pub fn to_response(mut self) -> DocumentSymbolResponse {
        let mut values: Vec<_> = std::mem::take(&mut self.symbols).to_vec();
        values.sort_by(|(range_a, _), (range_b, _)| range_a.start().offset().cmp(&range_b.start().offset()));

        let values = values.into_iter()
            .map(|(_, info)| info)
            .map(|info| info.to_symbol(&self))
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
        self.add_expression(&statement.destination);
        self.add_expression(&statement.source);
    }

    fn add_statement_for(&mut self, statement: &ForStatement) {
        match statement.iterable.value() {
            ForIterableKind::Expression(expr) => {
                self.add_expression(&expr);
            }
            ForIterableKind::Range(range) => {
                self.add_expression(&range.start);
                self.add_expression(&range.end);
            }
        }

        for statement in &statement.body {
            self.add_statement(statement);
        }
    }

    fn add_statement_function(&mut self, statement: &FunctionStatement) {
        self.symbols.insert(LspSymbol {
            name: statement.name.value().clone(),
            kind: LspTokenType::Method,
            range: statement.name.range(),
            modifier: LspSymbolModifier::default(),
        });

        if let Some(return_type) = &statement.return_type {
            self.add_type(&return_type);
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

    fn add_statement_interface(&mut self, interface: &InterfaceStatement) {
        self.symbols.insert(LspSymbol {
            name: interface.name.value().clone(),
            kind: LspTokenType::Class,
            range: interface.name.range(),
            modifier: LspSymbolModifier::default(),
        });

        for ty_param in &interface.generic_types {
            self.symbols.insert(LspSymbol {
                name: ty_param.value().clone(),
                kind: LspTokenType::Class,
                range: ty_param.range(),
                modifier: LspSymbolModifier::default(),
            });
        }

        for field in &interface.methods {
            self.add_method(field);
        }
    }

    fn add_statement_return(&mut self, statement: &ReturnStatement) {
        if let Some(expr) = &statement.expression {
            self.add_expression(expr);
        }
    }

    fn add_statement_structure(&mut self, statement: &Structure) {
        self.symbols.insert(LspSymbol {
            name: statement.name.value().clone(),
            kind: LspTokenType::Class,
            range: statement.name.range(),
            modifier: LspSymbolModifier::default(),
        });

        for ty_param in &statement.generic_types {
            self.symbols.insert(LspSymbol {
                name: ty_param.value().clone(),
                kind: LspTokenType::Class,
                range: ty_param.range(),
                modifier: LspSymbolModifier::default(),
            });
        }

        for field in &statement.fields {
            self.add_structure_field(field);
        }

        for field in &statement.methods {
            self.add_method(field);
        }
    }

    fn add_structure_field(&mut self, field: &Field) {
        self.add_attributes(&field.attributes);

        self.symbols.insert(LspSymbol {
            name: field.name.value().clone(),
            kind: LspTokenType::Property,
            range: field.name.range(),
            modifier: LspSymbolModifier::default(),
        });

        self.symbols.insert(LspSymbol {
            name: field.ty.specifier.fully_qualified_name().clone(),
            kind: LspTokenType::Class,
            range: field.ty.range(),
            modifier: LspSymbolModifier::default(),
        });

        if let Some(default_value) = &field.default_value {
            self.add_expression(&default_value);
        }
    }

    fn add_method(&mut self, method: &Method) {
        self.add_statement_function(&method.function);
    }

    fn add_statement_variable(&mut self, statement: &VariableStatement) {
        self.add_expression(&statement.expression);
    }

    fn add_parameter(&mut self, parameter: &Parameter) {
        self.add_attributes(&parameter.attributes);

        self.symbols.insert(LspSymbol {
            name: parameter.name.value().clone(),
            kind: LspTokenType::ParameterName,
            range: parameter.name.range(),
            modifier: LspSymbolModifier::default(),
        });

        self.add_type(&parameter.ty);
    }

    fn add_attribute(&mut self, attribute: &Attribute) {
        self.symbols.remove(attribute.at_range);
        self.symbols.remove(attribute.name.range());
    }

    fn add_attributes(&mut self, attributes: &[Ranged<Attribute>]) {
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
                        name: identifier.value().clone(),
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
                        modifier: LspSymbolModifier::default(),
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

            Expression::Primary(PrimaryExpression::SizedArrayInitializer{ typ, size }) => {
                self.symbols.insert(LspSymbol {
                    name: typ.specifier.fully_qualified_name(),
                    kind: LspTokenType::Class,
                    range: typ.specifier.range(),
                    modifier: LspSymbolModifier::default(),
                });

                self.add_expression(&size);
            }

            Expression::Primary(..) => (),
        }
    }

    fn add_extension(&mut self, extension: &ExtensionStatement) {
        for ty_param in &extension.generic_types {
            self.symbols.insert(LspSymbol {
                name: ty_param.value().clone(),
                kind: LspTokenType::Class,
                range: ty_param.range(),
                modifier: LspSymbolModifier::default(),
            });
        }

        if let Some(interface) = &extension.interface_specifier {
            self.add_interface_specifier(interface);
        }

        self.add_type_specifier(&extension.type_specifier);

        for method in &extension.methods {
            self.add_method(method);
        }
    }

    fn add_expression_postfix(&mut self, expression: &PostfixExpression) {
        match expression.kind.value() {
            PostfixExpressionKind::Call(..) => {
                if let Expression::Primary(PrimaryExpression::Reference(ident)) = expression.lhs.value() {
                    self.symbols.insert(LspSymbol {
                        name: ident.value().clone(),
                        kind: LspTokenType::Function,
                        range: ident.range(),
                        modifier: LspSymbolModifier::default(),
                    });
                }
            }

            PostfixExpressionKind::MethodCall(method) => {
                self.symbols.insert(LspSymbol {
                    name: method.method_name.value().clone(),
                    kind: LspTokenType::Method,
                    range: method.method_name.range(),
                    modifier: LspSymbolModifier::default(),
                });
            }

            PostfixExpressionKind::Member(member) => {
                self.symbols.insert(LspSymbol {
                    name: member.value().clone(),
                    kind: LspTokenType::Property,
                    range: member.range(),
                    modifier: LspSymbolModifier::default(),
                });
            }

            PostfixExpressionKind::Subscript(ranged) => {
                self.add_expression(&ranged);
            }
        }
    }

    fn add_expression_structure_instantiation(&mut self, expression: &StructureInstantiationExpression) {
        self.symbols.insert(LspSymbol {
            name: expression.name.value().clone(),
            kind: LspTokenType::Class,
            range: expression.name.range(),
            modifier: LspSymbolModifier::default(),
        });

        for ty in expression.type_parameters.value() {
            self.add_type(ty);
        }

        for field_instantiation in &expression.fields {
            self.symbols.insert(LspSymbol {
                name: field_instantiation.name.value().clone(),
                kind: LspTokenType::Variable,
                range: field_instantiation.name.range(),
                modifier: LspSymbolModifier::default(),
            });
            self.add_expression(&field_instantiation.value);
        }
    }

    fn add_type(&mut self, ty: &Type) {
        self.add_type_specifier(&ty.specifier);
    }

    fn add_type_specifier(&mut self, specifier: &TypeSpecifier) {
        match specifier {
            TypeSpecifier::BuiltIn(bt) => {
                self.symbols.insert(LspSymbol {
                    name: bt.name().clone(),
                    kind: LspTokenType::Class,
                    range: bt.range(),
                    modifier: LspSymbolModifier::default(),
                });
            }

            TypeSpecifier::Custom { name, type_parameters } => {
                self.symbols.insert(LspSymbol {
                    name: name.value().clone(),
                    kind: LspTokenType::Class,
                    range: name.range(),
                    modifier: LspSymbolModifier::default(),
                });

                for param in type_parameters.value() {
                    self.add_type(param);
                }
            }
        }

    }

    fn add_interface_specifier(&mut self, specifier: &InterfaceSpecifier) {
        self.symbols.insert(LspSymbol {
            name: specifier.name.value().clone(),
            kind: LspTokenType::Class,
            range: specifier.name.range(),
            modifier: LspSymbolModifier::default(),
        });

        for param in specifier.type_parameters.value() {
            self.add_type(param);
        }
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
    name: BabString,
    kind: LspTokenType,
    range: FileRange,
    modifier: LspSymbolModifier,
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
            token_modifiers_bitset: self.modifier as u32,
        }
    }

    #[must_use]
    fn to_symbol(self, symbolizer: &Symbolizer) -> SymbolInformation {
        #[allow(deprecated)]
        SymbolInformation {
            name: self.name.to_string(),
            kind: self.kind.into(),
            tags: None,
            deprecated: None,
            location: symbolizer.converter.convert_file_range_to_location(symbolizer.uri.clone(), self.range),
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
    Interface,
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
            TokenKind::CharacterLiteral(..) => LspTokenType::String,
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
            LspTokenType::Interface => SemanticTokenType::INTERFACE,
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
            LspTokenType::Interface => SymbolKind::INTERFACE,
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, EnumIter)]
#[repr(u32)]
pub enum LspSymbolModifier {
    #[default]
    None,
    ControlFlow,
}

impl LspSymbolModifier {
    pub fn legend() -> Vec<SemanticTokenModifier> {
        use strum::IntoEnumIterator;
        Self::iter().filter_map(|x| x.into()).collect()
    }
}

impl From<LspSymbolModifier> for Option<SemanticTokenModifier> {
    fn from(value: LspSymbolModifier) -> Self {
        match value {
            LspSymbolModifier::None => None,
            LspSymbolModifier::ControlFlow => Some(SemanticTokenModifier::new("controlFlow")),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::TextEncoding;

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

        let mut symbolizer = Symbolizer::new("file:///test.h".parse().unwrap(), &input, Converter::new(input.clone(), TextEncoding::Utf8));
        for token in &tokens {
            symbolizer.add_token(token);
        }

        for (range, symbol) in symbolizer.symbols.map {
            eprintln!("Range={range:?}    symbol={symbol:#?}");
        }
    }
}
