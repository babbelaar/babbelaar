// Copyright (C) 2023 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::Display;

use strum::AsRefStr;

use crate::{
    statement::ReturnStatement, BiExpression, BiOperator, Builtin, Comparison, Expression, FileLocation, FileRange, ForStatement, FunctionCallExpression, FunctionStatement, IfStatement, Keyword, Parameter, PrimaryExpression, Punctuator, RangeExpression, Ranged, Statement, StatementKind, TemplateStringExpressionPart, TemplateStringToken, Token, TokenKind, Type, TypeSpecifier
};

pub type ParseResult<'source_code, T> = Result<T, ParseError<'source_code>>;

#[derive(Clone)]
pub struct Parser<'tokens, 'source_code> {
    tokens: &'tokens [Token<'source_code>],
    pub cursor: usize,
    pub token_begin: FileLocation,
    pub token_end: FileLocation,
    pub errors: Vec<ParseError<'source_code>>,
    error_behavior: ParserErrorBehavior,
}

impl<'tokens, 'source_code> Parser<'tokens, 'source_code> {
    pub fn new(tokens: &'tokens [Token<'source_code>]) -> Self {
        Self {
            error_behavior: ParserErrorBehavior::Propagate,
            token_begin: Default::default(),
            token_end: Default::default(),
            errors: Vec::new(),
            tokens,
            cursor: 0,
        }
    }

    #[must_use]
    pub fn attempt_to_ignore_errors(self) -> Self {
        Self {
            error_behavior: ParserErrorBehavior::AttemptToIgnore,
            ..self
        }
    }

    pub fn parse_statement(&mut self) -> Result<Statement<'source_code>, ParseError<'source_code>> {
        let first_token = self.peek_token()?;
        let start = first_token.begin;

        let kind = match first_token.kind {
            TokenKind::Keyword(Keyword::Als) => {
                _ = self.consume_token().ok();
                StatementKind::If(self.parse_if_statement()?)
            }

            TokenKind::Keyword(Keyword::Bekeer) => {
                _ = self.consume_token().ok();
                StatementKind::Return(self.parse_return_statement()?)
            }

            TokenKind::Keyword(Keyword::Functie) => {
                _ = self.consume_token().ok();
                StatementKind::Function(self.parse_function()?)
            }

            TokenKind::Keyword(Keyword::Volg) => {
                StatementKind::For(self.parse_for_statement()?)
            }

            _ => {
                let expression = self.parse_function_call_expression()?;
                self.expect_semicolon_after_statement()?;
                StatementKind::Expression(expression)
            }
        };

        let range = FileRange::new(start, self.token_end);
        Ok(Statement {
            range,
            kind,
        })
    }

    pub fn parse_function(&mut self) -> Result<FunctionStatement<'source_code>, ParseError<'source_code>> {
        let name = self.consume_token()?;
        let name_range = name.range();
        let TokenKind::Identifier(name) = name.kind else {
            return Err(ParseError::FunctionStatementExpectedName { token: name });
        };
        let name = Ranged::new(name_range, name);

        self.expect_left_paren("functienaam")?;

        let mut parameters = Vec::new();
        while self.peek_punctuator() != Some(Punctuator::RightParenthesis) {
            match self.parse_parameter() {
                Ok(parameter) => parameters.push(parameter),
                Err(error) => self.handle_error(error)?,
            }

            match self.peek_punctuator() {
                Some(Punctuator::Comma) => {
                    _ = self.consume_token()?;
                    continue;
                }

                Some(Punctuator::RightParenthesis) => break,

                _ => match self.error_behavior {
                    ParserErrorBehavior::Propagate => return Err(ParseError::ParameterExpectedComma{
                        token: self.peek_token().ok().cloned(),
                    }),
                    ParserErrorBehavior::AttemptToIgnore => continue,
                }
            }
        }

        self.expect_right_paren("functienaam")?;
        self.expect_left_curly_bracket("functieargumentenlijst")?;

        let mut body = Vec::new();
        loop {
            if self.peek_token()?.kind == TokenKind::Punctuator(Punctuator::RightCurlyBracket) {
                _ = self.consume_token()?;
                break;
            }

            match self.parse_statement() {
                Ok(statement) => body.push(statement),
                Err(error) => self.handle_error(error)?,
            }
        }

        Ok(FunctionStatement { name, body, parameters })
    }

    fn handle_error(&mut self, error: ParseError<'source_code>) -> Result<(), ParseError<'source_code>> {
        match self.error_behavior {
            ParserErrorBehavior::AttemptToIgnore => {
                self.errors.push(error);
                Ok(())
            }
            ParserErrorBehavior::Propagate => return Err(error),
        }
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement<'source_code>, ParseError<'source_code>> {
        match self.peek_punctuator() {
            Some(Punctuator::Semicolon) => {
                _ = self.consume_token()?;
                return Ok(ReturnStatement { expression: None });
            }

            Some(Punctuator::RightCurlyBracket) => return Ok(ReturnStatement { expression: None }),

            _ => (),
        }

        match self.parse_expression() {
            Ok(expression) => {
                self.expect_semicolon_after_statement()?;
                Ok(ReturnStatement {
                    expression: Some(expression),
                })
            }

            Err(error) => self.handle_error(error)
                .map(|()| {
                    ReturnStatement {
                        expression: None,
                    }
                }),
        }
    }

    fn parse_parameter(&mut self) -> Result<Parameter<'source_code>, ParseError<'source_code>> {
        let name = self.parse_parameter_name()?;

        self.expect_colon("parameternaam")?;

        let ty = self.parse_type()?;

        Ok(Parameter {
            name,
            ty,
        })
    }

    fn parse_parameter_name(&mut self) -> Result<Ranged<String>, ParseError<'source_code>> {
        let name = self.consume_token()?;
        let name_range = name.range();
        let TokenKind::Identifier(name) = name.kind else {
            return Err(ParseError::ParameterExpectedName { token: name });
        };

        Ok(Ranged::new(name_range, name.to_string()))
    }

    fn parse_type(&mut self) -> Result<Ranged<Type<'source_code>>, ParseError<'source_code>> {
        let name_token = self.consume_token()?;
        let TokenKind::Identifier(name) = name_token.kind else {
            return Err(ParseError::TypeExpectedSpecifierName { token: name_token });
        };

        let specifier = match Builtin::TYPES.iter().find(|x| x.name == name) {
            Some(builtin) => TypeSpecifier::BuiltIn(builtin),
            None => TypeSpecifier::Custom { name },
        };

        let ty = Type {
            specifier: Ranged::new(name_token.range(), specifier),
        };
        Ok(Ranged::new(name_token.range(), ty))
    }

    fn parse_for_statement(&mut self) -> Result<ForStatement<'source_code>, ParseError<'source_code>> {
        let keyword = self.consume_token()?.range();
        let iterator = self.consume_token()?;
        let TokenKind::Identifier(iterator_name) = iterator.kind else {
            return Err(ParseError::ForStatementExpectedIteratorName { token: iterator });
        };

        let iterator_name = Ranged::new(iterator.range(), iterator_name);

        let in_keyword = self.consume_token()?;
        if in_keyword.kind != TokenKind::Keyword(Keyword::In) {
            return Err(ParseError::ForStatementExpectedInKeyword { token: in_keyword, iterator_name });
        }

        let range = self.parse_range()?;

        self.expect_left_curly_bracket("reeks van volg-lus")?;

        let mut body = Vec::new();
        loop {
            if self.peek_token()?.kind == TokenKind::Punctuator(Punctuator::RightCurlyBracket) {
                _ = self.consume_token()?;
                break;
            }

            match self.parse_statement() {
                Ok(statement) => body.push(statement),
                Err(error) => self.handle_error(error)?,
            }
        }


        Ok(ForStatement { keyword, iterator_name, range, body })
    }

    fn parse_if_statement(&mut self) -> Result<IfStatement<'source_code>, ParseError<'source_code>> {
        let condition = self.parse_expression()?;

        self.expect_left_curly_bracket("als-conditie")?;

        let mut body = Vec::new();
        loop {
            if self.peek_token()?.kind == TokenKind::Punctuator(Punctuator::RightCurlyBracket) {
                _ = self.consume_token()?;
                break;
            }

            match self.parse_statement() {
                Ok(statement) => body.push(statement),
                Err(error) => self.handle_error(error)?,
            }
        }

        Ok(IfStatement { condition, body })
    }

    fn parse_range(&mut self) -> Result<RangeExpression<'source_code>, ParseError<'source_code>> {
        let range_keyword = self.consume_token()?;
        if range_keyword.kind != TokenKind::Keyword(Keyword::Reeks) {
            return Err(ParseError::RangeExpectedKeyword { token: range_keyword });
        }

        self.expect_left_paren("reeks")?;

        let start = self.parse_ranged(Self::parse_primary_expression)?;

        self.expect_comma("reeks")?;

        let end = self.parse_ranged(Self::parse_primary_expression)?;

        self.expect_right_paren("reeks")?;

        Ok(RangeExpression { start, end })
    }

    fn parse_primary_expression(&mut self) -> Result<PrimaryExpression<'source_code>, ParseError<'source_code>> {
        let token = self.consume_token()?;

        match token.kind {
            TokenKind::StringLiteral(literal) => Ok(PrimaryExpression::StringLiteral(literal)),
            TokenKind::Integer(integer) => Ok(PrimaryExpression::IntegerLiteral(integer)),
            TokenKind::Identifier(identifier) => Ok(PrimaryExpression::Reference(Ranged::new(token.range(), identifier))),
            TokenKind::TemplateString(template_string) => self.parse_template_string(template_string),
            TokenKind::Keyword(Keyword::Waar) => Ok(PrimaryExpression::Boolean(true)),
            TokenKind::Keyword(Keyword::Onwaar) => Ok(PrimaryExpression::Boolean(false)),

            _ => Err(ParseError::UnknownStartOfExpression { token }),
        }
    }

    fn parse_ranged<F, T>(&mut self, f: F) -> Result<Ranged<T>, ParseError<'source_code>>
            where F: FnOnce(&mut Self) -> Result<T, ParseError<'source_code>> {
        let start = self.token_begin;
        let value = f(self)?;
        let end = self.token_end;
        Ok(Ranged::new(FileRange::new(start, end), value))
    }

    fn parse_expression(&mut self) -> Result<Ranged<Expression<'source_code>>, ParseError<'source_code>> {
        let start = self.next_start();

        let expr = self.parse_relational_expression()?;

        let end = self.token_end;
        let range = FileRange::new(start, end);

        Ok(Ranged::new(range, expr))
    }

    fn parse_relational_expression(&mut self) -> Result<Expression<'source_code>, ParseError<'source_code>> {
        self.parse_bi_expression(Self::parse_multiplicative_expression, &[
            (Punctuator::Equals, BiOperator::Comparison(Comparison::Equality)),
            // TODO the rest
        ])
    }

    fn parse_multiplicative_expression(&mut self) -> Result<Expression<'source_code>, ParseError<'source_code>> {
        self.parse_bi_expression(Self::parse_additive_expression, &[
            (Punctuator::Asterisk, BiOperator::Multiply),
            (Punctuator::PercentageSign, BiOperator::Modulo),
            (Punctuator::Solidus, BiOperator::Divide),
        ])
    }

    fn parse_additive_expression(&mut self) -> Result<Expression<'source_code>, ParseError<'source_code>> {
        self.parse_bi_expression(Self::parse_postfix_expression, &[
            (Punctuator::PlusSign, BiOperator::Add),
            (Punctuator::HyphenMinus, BiOperator::Subtract),
        ])
    }

    fn parse_bi_expression<F>(&mut self, mut operand: F, operators: &[(Punctuator, BiOperator)]) -> Result<Expression<'source_code>, ParseError<'source_code>>
            where F: FnMut(&mut Self) -> Result<Expression<'source_code>, ParseError<'source_code>> {
        let lhs = operand(self)?;

        let Ok(next) = self.peek_token() else {
            return Ok(lhs);
        };

        let Some((_, operator)) = operators.into_iter().find(|(p, _)| next.kind == TokenKind::Punctuator(*p)).cloned() else {
            return Ok(lhs);
        };

        let operator_range = self.consume_token()?.range();
        let operator = Ranged::new(operator_range, operator);

        let rhs = Expression::Primary(self.parse_primary_expression()?);

        Ok(Expression::BiExpression(BiExpression {
            operator,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }))
    }

    fn parse_postfix_expression(&mut self) -> Result<Expression<'source_code>, ParseError<'source_code>> {
        let mut parser = self.clone();
        if let Ok(func) = parser.parse_function_call_expression() {
            *self = parser;
            Ok(func)
        } else {
            Ok(Expression::Primary(self.parse_primary_expression()?))
        }
    }

    fn parse_function_call_expression(&mut self) -> Result<Expression<'source_code>, ParseError<'source_code>> {
        let token = self.consume_token()?;

        let TokenKind::Identifier(identifier) = token.kind else {
            return Err(ParseError::ExpressionFunctionCallNotStartingWithIdentifier { token });
        };
        let function_identifier = Ranged::new(token.range(), identifier.to_string());

        let token_left_paren = self.expect_left_paren("aan te roepen functienaam")?;

        let mut arguments = Vec::new();
        while let Ok(token) = self.peek_token() {
            if token.kind == TokenKind::Punctuator(Punctuator::RightParenthesis) {
                self.consume_token()?;
                break;
            }

            if !arguments.is_empty() {
                if let Err(error) = self.expect_comma("argument in functieaanroep") {
                    self.handle_error(error)?;
                    break;
                }
            }

            match self.parse_expression() {
                Ok(expr) => arguments.push(expr),
                Err(error) => {
                    self.handle_error(error)?;
                }
            }
        }

        let token_right_paren = self.tokens[self.cursor - 1].range();

        Ok(Expression::Function(FunctionCallExpression {
            function_identifier,
            arguments,
            token_left_paren,
            token_right_paren,
        }))
    }

    fn parse_template_string(&self, template_string: Vec<TemplateStringToken<'source_code>>) -> ParseResult<'source_code, PrimaryExpression<'source_code>> {
        let mut parts = Vec::new();

        for token in template_string {
            let part = match token {
                TemplateStringToken::Plain { str, .. } => TemplateStringExpressionPart::String(str),
                TemplateStringToken::Expression(tokens) => {
                    let mut parser = Parser::new(&tokens);
                    let expr = parser.parse_expression()?;

                    if parser.cursor < tokens.len() {
                        return Err(ParseError::ResidualTokensInTemplateString {
                            token: tokens[parser.cursor].clone(),
                        });
                    }

                    TemplateStringExpressionPart::Expression(expr)
                }
            };

            parts.push(part);
        }

        Ok(PrimaryExpression::TemplateString { parts })
    }

    fn peek_token(&self) -> Result<&Token<'source_code>, ParseError<'source_code>> {
        match self.tokens.get(self.cursor) {
            Some(token) => Ok(token),
            None => Err(ParseError::EndOfFile),
        }
    }

    fn peek_punctuator(&self) -> Option<Punctuator> {
        let Ok(token) = self.peek_token() else { return None };
        match token.kind {
            TokenKind::Punctuator(punctuator) => Some(punctuator),
            _ => None,
        }
    }

    fn consume_token(&mut self) -> Result<Token<'source_code>, ParseError<'source_code>> {
        let token = self.peek_token()?.clone();
        self.token_begin = token.begin;
        self.token_end = token.end;
        self.cursor += 1;
        Ok(token)
    }

    fn expect_left_paren(&mut self, context: &'static str) -> Result<FileRange, ParseError<'source_code>> {
        let token = self.consume_token()?;

        if token.kind != TokenKind::Punctuator(Punctuator::LeftParenthesis) {
            return Err(ParseError::ExpectedLeftParen { token, context });
        }

        Ok(token.range())
    }

    fn expect_right_paren(&mut self, context: &'static str) -> Result<FileRange, ParseError<'source_code>> {
        let token = self.consume_token()?;

        if token.kind != TokenKind::Punctuator(Punctuator::RightParenthesis) {
            return Err(ParseError::ExpectedRightParen { token, context });
        }

        Ok(token.range())
    }

    fn expect_left_curly_bracket(&mut self, context: &'static str) -> Result<(), ParseError<'source_code>> {
        let token = self.consume_token()?;

        if token.kind != TokenKind::Punctuator(Punctuator::LeftCurlyBracket) {
            return Err(ParseError::ExpectedLeftCurlyBracket { token, context });
        }

        Ok(())
    }

    fn expect_comma(&mut self, context: &'static str) -> Result<(), ParseError<'source_code>> {
        let token = self.consume_token()?;

        if token.kind != TokenKind::Punctuator(Punctuator::Comma) {
            return Err(ParseError::ExpectedComma { token, context });
        }

        Ok(())
    }

    fn expect_colon(&mut self, context: &'static str) -> Result<(), ParseError<'source_code>> {
        let token = self.consume_token()?;

        if token.kind != TokenKind::Punctuator(Punctuator::Colon) {
            return Err(ParseError::ExpectedColon { token, context });
        }

        Ok(())
    }

    fn expect_semicolon_after_statement(&mut self) -> Result<(), ParseError<'source_code>> {
        let token = match self.consume_token() {
            Ok(token) => token,
            Err(ParseError::EndOfFile) => return self.handle_error(ParseError::ExpectedSemicolonAfterStatement { token: self.tokens[self.cursor - 1].clone() }),
            Err(e) => return Err(e),
        };

        if token.kind != TokenKind::Punctuator(Punctuator::Semicolon) {
            self.handle_error(ParseError::ExpectedSemicolonAfterStatement { token })?;
        }

        Ok(())
    }

    fn next_start(&self) -> FileLocation {
        match self.tokens.get(self.cursor) {
            Some(token) => token.end,
            None => self.token_end,
        }
    }
}

#[derive(Clone, Debug, thiserror::Error, AsRefStr)]
pub enum ParseError<'source_code> {
    #[error("Onverwacht einde van het bestand")]
    EndOfFile,

    #[error("Ongeldig start van een statement: {token}")]
    StatementInvalidStart{ token: Token<'source_code> },

    #[error("Ongeldig start van een functieaanroepstatement: {token}")]
    ExpressionFunctionCallNotStartingWithIdentifier { token: Token<'source_code> },

    #[error("Open accolade verwacht `{{` na {context}, maar kreeg: {token}")]
    ExpectedLeftCurlyBracket { token: Token<'source_code>, context: &'static str },

    #[error("Open rond haakje verwacht '(' na {context}, maar kreeg: {token}")]
    ExpectedLeftParen { token: Token<'source_code>, context: &'static str },

    #[error("Gesloten rond haakje verwacht ')' na {context}, maar kreeg: {token}")]
    ExpectedRightParen { token: Token<'source_code>, context: &'static str },

    #[error("Dubbele punt verwacht ':' na {context}, maar kreeg: {token}")]
    ExpectedColon { token: Token<'source_code>, context: &'static str },

    #[error("Komma verwacht ',' na {context}, maar kreeg: {token}")]
    ExpectedComma { token: Token<'source_code>, context: &'static str },

    #[error("Puntkomma verwacht ';' na statement: {token}")]
    ExpectedSemicolonAfterStatement { token: Token<'source_code> },

    #[error("Functienaam verwacht na `functie`, maar kreeg: {token}")]
    FunctionStatementExpectedName { token: Token<'source_code> },

    #[error("Iteratornaam verwacht na `volg`, maar kreeg: {token}")]
    ForStatementExpectedIteratorName { token: Token<'source_code> },

    #[error("Sleutelwoord 'in' verwacht na `volg {}`, maar kreeg: {token}", iterator_name.value())]
    ForStatementExpectedInKeyword { token: Token<'source_code>, iterator_name: Ranged<&'source_code str> },

    #[error("Parameternaam verwacht, maar kreeg: {token}")]
    ParameterExpectedName { token: Token<'source_code> },

    #[error("Komma `,` of gesloten rond haakje `)` verwacht binnen parameterlijst, maar kreeg: {}", token.as_ref().map(|x| x as &dyn Display).unwrap_or_else(|| &"(niets)" as &'static dyn Display))]
    ParameterExpectedComma { token: Option<Token<'source_code>> },

    #[error("Sleutelwoord `reeks` verwacht, maar kreeg: {token}")]
    RangeExpectedKeyword { token: Token<'source_code> },

    #[error("Resterende token na expressie binnen sjabloonslinger: {token}")]
    ResidualTokensInTemplateString { token: Token<'source_code> },

    #[error("Parametertype verwacht, maar kreeg: {token}")]
    TypeExpectedSpecifierName { token: Token<'source_code> },

    #[error("Onbekend start van een expressie: {token}")]
    UnknownStartOfExpression { token: Token<'source_code> },
}

impl<'source_code> ParseError<'source_code> {
    pub fn token(&self) -> Option<&Token<'source_code>> {
        match self {
            Self::EndOfFile => None,
            Self::StatementInvalidStart { token } => Some(token),
            Self::ExpressionFunctionCallNotStartingWithIdentifier { token } => Some(token),
            Self::ExpectedLeftCurlyBracket { token, .. } => Some(token),
            Self::ExpectedLeftParen { token, .. } => Some(token),
            Self::ExpectedRightParen { token, .. } => Some(token),
            Self::ExpectedColon { token, .. } => Some(token),
            Self::ExpectedComma { token, .. } => Some(token),
            Self::ExpectedSemicolonAfterStatement { token, .. } => Some(token),
            Self::FunctionStatementExpectedName { token } => Some(token),
            Self::ForStatementExpectedIteratorName { token } => Some(token),
            Self::ForStatementExpectedInKeyword { token, .. } => Some(token),
            Self::ParameterExpectedName { token } => Some(token),
            Self::ParameterExpectedComma { token } => token.as_ref(),
            Self::RangeExpectedKeyword { token } => Some(token),
            Self::ResidualTokensInTemplateString { token } => Some(token),
            Self::TypeExpectedSpecifierName { token } => Some(token),
            Self::UnknownStartOfExpression { token } => Some(token),
        }
    }

    #[must_use]
    pub fn name(&self) -> &str {
        self.as_ref()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParserErrorBehavior {
    Propagate,
    AttemptToIgnore,
}

#[cfg(test)]
mod tests {
    use crate::Lexer;

    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(" schrijf(\"Hallo\") ")]
    #[case("schrijf(\"Hallo\")")]
    fn parse_function_call_expression(#[case] input: &str) {
        let tokens: Vec<Token<'_>> = Lexer::new(input).collect();
        let mut parser = Parser::new(&tokens);
        let expression = parser.parse_expression().unwrap();
        assert!(matches!(expression.value(), Expression::Function(..)), "Invalid parse: {expression:?}");
    }
}