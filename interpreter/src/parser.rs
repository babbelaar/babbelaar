// Copyright (C) 2023 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{
    statement::ReturnStatement, BiExpression, BiOperator, Builtin, Expression, FileLocation, FileRange, ForStatement, FunctionCallExpression, FunctionStatement, Keyword, Parameter, PrimaryExpression, Punctuator, RangeExpression, Ranged, Statement, TemplateStringExpressionPart, TemplateStringToken, Token, TokenKind, Type, TypeSpecifier
};

pub type ParseResult<'source_code, T> = Result<T, ParseError<'source_code>>;

#[derive(Copy, Clone)]
pub struct Parser<'tokens, 'source_code> {
    tokens: &'tokens [Token<'source_code>],
    pub cursor: usize,
    pub token_begin: FileLocation,
    pub token_end: FileLocation,
    error_behavior: ParserErrorBehavior,
}

impl<'tokens, 'source_code> Parser<'tokens, 'source_code> {
    pub fn new(tokens: &'tokens [Token<'source_code>]) -> Self {
        Self {
            error_behavior: ParserErrorBehavior::Propagate,
            token_begin: Default::default(),
            token_end: Default::default(),
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
        if self.peek_token()?.kind == TokenKind::Keyword(Keyword::Bekeer) {
            _ = self.consume_token().ok();
            return Ok(Statement::Return(self.parse_return_statement()?));
        }

        if self.peek_token()?.kind == TokenKind::Keyword(Keyword::Functie) {
            _ = self.consume_token().ok();
            return Ok(Statement::Function(self.parse_function()?));
        }

        if self.peek_token()?.kind == TokenKind::Keyword(Keyword::Volg) {
            return Ok(Statement::For(self.parse_for_statement()?));
        }

        let expr = self.parse_function_call_expression()?;
        Ok(Statement::Expression(expr))
    }

    pub fn parse_function(&mut self) -> Result<FunctionStatement<'source_code>, ParseError<'source_code>> {
        let name = self.consume_token()?;
        let TokenKind::Identifier(name) = name.kind else {
            return Err(ParseError::FunctionStatementExpectedName { token: name });
        };

        self.expect_left_paren("function name")?;

        let mut parameters = Vec::new();
        while self.peek_punctuator() != Some(Punctuator::RightParenthesis) {
            match (self.parse_parameter(), self.error_behavior) {
                (Ok(parameter), _) => parameters.push(parameter),
                (Err(e), ParserErrorBehavior::Propagate) => return Err(e),
                (Err(_), ParserErrorBehavior::AttemptToIgnore) => continue,
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

        self.expect_right_paren("function name")?;
        self.expect_left_curly_bracket("function argument list")?;

        let mut body = Vec::new();
        loop {
            if self.peek_token()?.kind == TokenKind::Punctuator(Punctuator::RightCurlyBracket) {
                _ = self.consume_token()?;
                break;
            }

            match (self.parse_statement(), self.error_behavior) {
                (Ok(statement), _) => body.push(statement),
                (Err(e), ParserErrorBehavior::Propagate) => return Err(e),
                (Err(_), ParserErrorBehavior::AttemptToIgnore) => continue,
            }
        }


        Ok(FunctionStatement { name, body, parameters })
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
                Ok(ReturnStatement {
                    expression: Some(expression),
                })
            }

            Err(e) => {
                match self.error_behavior {
                    ParserErrorBehavior::Propagate => Err(e),
                    ParserErrorBehavior::AttemptToIgnore => Ok(ReturnStatement {
                        expression: None,
                    }),
                }
            }
        }
    }

    fn parse_parameter(&mut self) -> Result<Parameter<'source_code>, ParseError<'source_code>> {
        let name = self.parse_parameter_name()?;

        self.expect_colon("parameter name")?;

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
            return Err(ParseError::ForStatementExpectedInKeyword { token: in_keyword });
        }

        let range = self.parse_range()?;

        self.expect_left_curly_bracket("for-loop range")?;

        let mut body = Vec::new();
        loop {
            if self.peek_token()?.kind == TokenKind::Punctuator(Punctuator::RightCurlyBracket) {
                _ = self.consume_token()?;
                break;
            }

            match (self.parse_statement(), self.error_behavior) {
                (Ok(statement), _) => body.push(statement),
                (Err(e), ParserErrorBehavior::Propagate) => return Err(e),
                (Err(_), ParserErrorBehavior::AttemptToIgnore) => continue,
            }
        }


        Ok(ForStatement { keyword, iterator_name, range, body })
    }

    fn parse_range(&mut self) -> Result<RangeExpression<'source_code>, ParseError<'source_code>> {
        let range_keyword = self.consume_token()?;
        if range_keyword.kind != TokenKind::Keyword(Keyword::Reeks) {
            return Err(ParseError::RangeExpectedKeyword { token: range_keyword });
        }

        self.expect_left_paren("range")?;

        let start = self.parse_ranged(Self::parse_primary_expression)?;

        self.expect_comma("range")?;

        let end = self.parse_ranged(Self::parse_primary_expression)?;

        self.expect_right_paren("range")?;

        Ok(RangeExpression { start, end })
    }

    fn parse_primary_expression(&mut self) -> Result<PrimaryExpression<'source_code>, ParseError<'source_code>> {
        let token = self.consume_token()?;

        match token.kind {
            TokenKind::StringLiteral(literal) => Ok(PrimaryExpression::StringLiteral(literal)),
            TokenKind::Integer(integer) => Ok(PrimaryExpression::IntegerLiteral(integer)),
            TokenKind::Identifier(identifier) => Ok(PrimaryExpression::Reference(identifier)),
            TokenKind::TemplateString(template_string) => self.parse_template_string(template_string),

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

    fn parse_expression(&mut self) -> Result<Expression<'source_code>, ParseError<'source_code>> {
        let primary = {
            let mut parser = *self;
            if let Ok(func) = parser.parse_function_call_expression() {
                *self = parser;
                func
            } else {
                Expression::Primary(self.parse_primary_expression()?)
            }
        };

        let Ok(next) = self.peek_token() else {
            return Ok(primary);
        };

        let operator = match next.kind {
            TokenKind::Punctuator(Punctuator::PlusSign) => BiOperator::Add,
            TokenKind::Punctuator(Punctuator::HyphenMinus) => BiOperator::Subtract,
            TokenKind::Punctuator(Punctuator::Asterisk) => BiOperator::Multiply,
            _ => return Ok(primary),
        };

        self.consume_token()?;

        let lhs = primary;
        let rhs = Expression::Primary(self.parse_primary_expression()?);

        Ok(Expression::BiExpression(BiExpression {
            operator,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }))
    }

    fn parse_function_call_expression(&mut self) -> Result<Expression<'source_code>, ParseError<'source_code>> {
        let token = self.consume_token()?;

        let TokenKind::Identifier(identifier) = token.kind else {
            return Err(ParseError::ExpressionFunctionCallNotStartingWithIdentifier { token });
        };
        let function_identifier = Ranged::new(token.range(), identifier.to_string());

        self.expect_left_paren("function call identifier")?;

        let mut arguments = Vec::new();
        while let Ok(token) = self.peek_token() {
            if token.kind == TokenKind::Punctuator(Punctuator::RightParenthesis) {
                self.consume_token()?;
                break;
            }

            if !arguments.is_empty() {
                self.expect_comma("argument in function call")?;
            }

            arguments.push(self.parse_expression()?);
        }

        Ok(Expression::Function(FunctionCallExpression {
            function_identifier,
            arguments,
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
        self.cursor += 1;
        Ok(token)
    }

    fn expect_left_paren(&mut self, context: &'static str) -> Result<(), ParseError<'source_code>> {
        let token = self.consume_token()?;

        if token.kind != TokenKind::Punctuator(Punctuator::LeftParenthesis) {
            return Err(ParseError::ExpectedLeftParen { token, context });
        }

        Ok(())
    }

    fn expect_right_paren(&mut self, context: &'static str) -> Result<(), ParseError<'source_code>> {
        let token = self.consume_token()?;

        if token.kind != TokenKind::Punctuator(Punctuator::RightParenthesis) {
            return Err(ParseError::ExpectedRightParen { token, context });
        }

        Ok(())
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
}

#[derive(Clone, Debug, thiserror::Error)]
pub enum ParseError<'source_code> {
    #[error("End of file (EOF) reached")]
    EndOfFile,

    #[error("Invalid start of statement: {token:?}")]
    StatementInvalidStart{ token: Token<'source_code> },

    #[error("Invalid start of function-call statement: {token:?}")]
    ExpressionFunctionCallNotStartingWithIdentifier { token: Token<'source_code> },

    #[error("Expected left curly bracket '{{' after {context}, but got: {token:?}")]
    ExpectedLeftCurlyBracket { token: Token<'source_code>, context: &'static str },

    #[error("Expected left parenthesis '(' after {context}, but got: {token:?}")]
    ExpectedLeftParen { token: Token<'source_code>, context: &'static str },

    #[error("Expected right parenthesis ')' after {context}, but got: {token:?}")]
    ExpectedRightParen { token: Token<'source_code>, context: &'static str },

    #[error("Expected colon ':' after {context}, but got: {token:?}")]
    ExpectedColon { token: Token<'source_code>, context: &'static str },

    #[error("Expected comma ',' after {context}, but got: {token:?}")]
    ExpectedComma { token: Token<'source_code>, context: &'static str },

    #[error("Expected function name after 'functie', but got: {token:?}")]
    FunctionStatementExpectedName { token: Token<'source_code> },

    #[error("Expected iterator name after 'volg', but got: {token:?}")]
    ForStatementExpectedIteratorName { token: Token<'source_code> },

    #[error("Expected 'in' keyword 'volg' iterator name, but got: {token:?}")]
    ForStatementExpectedInKeyword { token: Token<'source_code> },

    #[error("Expected name of parameter, but got: {token:?}")]
    ParameterExpectedName { token: Token<'source_code> },

    #[error("Expected comma ',' or right parenthesis ')' inside parameter list, but got: {token:?}")]
    ParameterExpectedComma { token: Option<Token<'source_code>> },

    #[error("Expected 'reeks' keyword, but got: {token:?}")]
    RangeExpectedKeyword { token: Token<'source_code> },

    #[error("Residual token after template string expression: {token:?}")]
    ResidualTokensInTemplateString { token: Token<'source_code> },

    #[error("Expected the type of the parameter, but got: {token:?}")]
    TypeExpectedSpecifierName { token: Token<'source_code> },

    #[error("Unknown start of expression: {token:?}")]
    UnknownStartOfExpression { token: Token<'source_code> },
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
        assert!(matches!(expression, Expression::Function(..)), "Invalid parse: {expression:?}");
    }
}
