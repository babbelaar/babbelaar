// Copyright (C) 2023 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{
    BiOperator,
    BiExpression,
    Expression,
    FunctionCallExpression,
    FunctionStatement,
    Keyword,
    PrimaryExpression,
    Statement,
    Token,
    TokenKind, ForStatement, RangeExpression,
};

#[derive(Copy, Clone)]
pub struct Parser<'source_code> {
    tokens: &'source_code [Token<'source_code>],
    cursor: usize,
}

impl<'source_code> Parser<'source_code> {
    pub fn new(tokens: &'source_code [Token<'source_code>]) -> Self {
        Self {
            tokens,
            cursor: 0,
        }
    }

    pub fn parse_statement(&mut self) -> Result<Statement<'source_code>, ParseError<'source_code>> {
        if self.peek_token()?.kind == TokenKind::Keyword(Keyword::Functie) {
            _ = self.consume_token().ok();
            return Ok(Statement::Function(self.parse_function()?));
        }

        if self.peek_token()?.kind == TokenKind::Keyword(Keyword::Volg) {
            _ = self.consume_token().ok();
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
        self.expect_right_paren("function name")?;
        self.expect_left_curly_bracket("function argument list")?;

        let mut body = Vec::new();
        loop {
            if self.peek_token()?.kind == TokenKind::RightCurlyBracket {
                _ = self.consume_token()?;
                break;
            }

            body.push(self.parse_statement()?);
        }


        Ok(FunctionStatement { name, body })
    }

    fn parse_for_statement(&mut self) -> Result<ForStatement<'source_code>, ParseError<'source_code>> {
        let iterator = self.consume_token()?;
        let TokenKind::Identifier(iterator_name) = iterator.kind else {
            return Err(ParseError::ForStatementExpectedIteratorName { token: iterator });
        };

        let in_keyword = self.consume_token()?;
        if in_keyword.kind != TokenKind::Keyword(Keyword::In) {
            return Err(ParseError::ForStatementExpectedInKeyword { token: in_keyword });
        }

        let range = self.parse_range()?;

        self.expect_left_curly_bracket("for-loop range")?;

        let mut body = Vec::new();
        loop {
            if self.peek_token()?.kind == TokenKind::RightCurlyBracket {
                _ = self.consume_token()?;
                break;
            }

            body.push(self.parse_statement()?);
        }


        Ok(ForStatement { iterator_name, range, body })
    }

    fn parse_range(&mut self) -> Result<RangeExpression<'source_code>, ParseError<'source_code>> {
        let range_keyword = self.consume_token()?;
        if range_keyword.kind != TokenKind::Keyword(Keyword::Reeks) {
            return Err(ParseError::RangeExpectedKeyword { token: range_keyword });
        }

        self.expect_left_paren("range")?;

        let start = self.parse_primary_expression()?;

        self.expect_comma("range")?;

        let end = self.parse_primary_expression()?;

        self.expect_right_paren("range")?;

        Ok(RangeExpression { start, end })
    }

    fn parse_primary_expression(&mut self) -> Result<PrimaryExpression<'source_code>, ParseError<'source_code>> {
        let token = self.consume_token()?;

        match token.kind {
            TokenKind::StringLiteral(literal) => Ok(PrimaryExpression::StringLiteral(literal)),
            TokenKind::Integer(integer) => Ok(PrimaryExpression::IntegerLiteral(integer)),
            TokenKind::Identifier(identifier) => Ok(PrimaryExpression::Reference(identifier)),

            _ => Err(ParseError::UnknownStartOfExpression { token }),
        }
    }

    fn parse_expression(&mut self) -> Result<Expression<'source_code>, ParseError<'source_code>> {
        let primary = Expression::Primary(self.parse_primary_expression()?);

        let Ok(next) = self.peek_token() else {
            return Ok(primary);
        };

        let operator = match next.kind {
            TokenKind::PlusSign => BiOperator::Add,
            TokenKind::HyphenMinus => BiOperator::Subtract,
            TokenKind::Asterisk => BiOperator::Multiply,
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

        self.expect_left_paren("function call identifier")?;

        let mut arguments = Vec::new();
        loop {
            if self.peek_token()?.kind == TokenKind::RightParenthesis {
                self.consume_token()?;
                break;
            }

            if !arguments.is_empty() {
                self.expect_comma("argument in function call")?;
            }

            arguments.push(self.parse_expression()?);
        }

        Ok(Expression::Function(FunctionCallExpression {
            function_identifier: identifier.to_string(),
            arguments,
        }))
    }

    fn peek_token(&self) -> Result<&Token<'source_code>, ParseError<'source_code>> {
        match self.tokens.get(self.cursor) {
            Some(token) => Ok(token),
            None => Err(ParseError::EndOfFile),
        }
    }

    fn consume_token(&mut self) -> Result<Token<'source_code>, ParseError<'source_code>> {
        let token = self.peek_token()?.clone();
        self.cursor += 1;
        Ok(token)
    }

    fn expect_left_paren(&mut self, context: &'static str) -> Result<(), ParseError<'source_code>> {
        let token = self.consume_token()?;

        if token.kind != TokenKind::LeftParenthesis {
            return Err(ParseError::ExpectedLeftParen { token, context });
        }

        Ok(())
    }

    fn expect_right_paren(&mut self, context: &'static str) -> Result<(), ParseError<'source_code>> {
        let token = self.consume_token()?;

        if token.kind != TokenKind::RightParenthesis {
            return Err(ParseError::ExpectedRightParen { token, context });
        }

        Ok(())
    }

    fn expect_left_curly_bracket(&mut self, context: &'static str) -> Result<(), ParseError<'source_code>> {
        let token = self.consume_token()?;

        if token.kind != TokenKind::LeftCurlyBracket {
            return Err(ParseError::ExpectedLeftCurlyBracket { token, context });
        }

        Ok(())
    }

    fn expect_comma(&mut self, context: &'static str) -> Result<(), ParseError<'source_code>> {
        let token = self.consume_token()?;

        if token.kind != TokenKind::Comma {
            return Err(ParseError::ExpectedComma { token, context });
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

    #[error("Expected comma ',' after {context}, but got: {token:?}")]
    ExpectedComma { token: Token<'source_code>, context: &'static str },

    #[error("Expected function name after 'functie', but got: {token:?}")]
    FunctionStatementExpectedName { token: Token<'source_code> },

    #[error("Expected iterator name after 'volg', but got: {token:?}")]
    ForStatementExpectedIteratorName { token: Token<'source_code> },

    #[error("Expected 'in' keyword 'volg' iterator name, but got: {token:?}")]
    ForStatementExpectedInKeyword { token: Token<'source_code> },

    #[error("Expected 'reeks' keyword, but got: {token:?}")]
    RangeExpectedKeyword { token: Token<'source_code> },

    #[error("Unknown start of expression: {token:?}")]
    UnknownStartOfExpression { token: Token<'source_code> },
}
