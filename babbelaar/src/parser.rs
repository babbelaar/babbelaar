// Copyright (C) 2023 - 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{fmt::Display, path::PathBuf};

use strum::AsRefStr;

use crate::{
    statement::ReturnStatement, AssignStatement, Attribute, AttributeArgument, BiExpression, BiOperator, Builtin, Comparison, Expression, Field, FieldInstantiation, FileLocation, FileRange, ForStatement, FunctionCallExpression, FunctionStatement, IfStatement, Keyword, MethodCallExpression, Parameter, ParseTree, PostfixExpression, PostfixExpressionKind, PrimaryExpression, Punctuator, RangeExpression, Ranged, Statement, StatementKind, Structure, StructureInstantiationExpression, TemplateStringExpressionPart, TemplateStringToken, Token, TokenKind, Type, TypeSpecifier, VariableStatement
};

pub type ParseResult<'source_code, T> = Result<T, ParseDiagnostic<'source_code>>;

#[derive(Clone)]
pub struct Parser<'tokens, 'source_code> {
    pub path: PathBuf,
    tokens: &'tokens [Token<'source_code>],
    pub cursor: usize,
    pub token_begin: FileLocation,
    pub token_end: FileLocation,
    pub errors: Vec<ParseDiagnostic<'source_code>>,
    error_behavior: ParserErrorBehavior,
}

impl<'tokens, 'source_code> Parser<'tokens, 'source_code> {
    pub fn new(path: PathBuf, tokens: &'tokens [Token<'source_code>]) -> Self {
        Self {
            path,
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

    pub fn parse_tree(&mut self) -> Result<ParseTree<'source_code>, ParseDiagnostic<'source_code>> {
        let mut tree = ParseTree::new(self.path.clone());

        while !self.is_at_end() {
            match self.parse_statement() {
                Ok(statement) => tree.push(statement),
                Err(ParseDiagnostic::EndOfFile) => break,
                Err(e) => return Err(e),
            }
        }

        Ok(tree)
    }

    pub fn parse_statement(&mut self) -> Result<Statement<'source_code>, ParseDiagnostic<'source_code>> {
        let start = self.peek_token()?.begin;
        let mut attributes = Vec::new();

        while let Some(Punctuator::AtSign) = self.peek_punctuator() {
            _ = self.consume_token()?;

            match self.parse_attribute() {
                Ok(attr) => attributes.push(attr),
                Err(e) => self.handle_error(e)?,
            }
        }

        let first_token = self.peek_token()?;
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

            TokenKind::Keyword(Keyword::Stel) => {
                _ = self.consume_token().ok();
                StatementKind::Variable(self.parse_variable_statement()?)
            }

            TokenKind::Keyword(Keyword::Structuur) => {
                _ = self.consume_token().ok();
                StatementKind::Structure(self.parse_structure_statement()?)
            }

            TokenKind::Keyword(Keyword::Volg) => {
                StatementKind::For(self.parse_for_statement()?)
            }

            _ => {
                let reset = self.cursor;

                match self.parse_assign_statement()? {
                    Some(statement) => {
                        StatementKind::Assignment(Ranged::new(statement.range, statement))
                    }
                    None => {
                        self.cursor = reset;

                        let expression = self.parse_expression()?;
                        self.expect_semicolon_after_statement()?;
                        StatementKind::Expression(expression)
                    }
                }
            }
        };

        let range = FileRange::new(start, self.token_end);
        Ok(Statement {
            range,
            attributes,
            kind,
        })
    }

    fn parse_assign_statement(&mut self) -> Result<Option<AssignStatement<'source_code>>, ParseDiagnostic<'source_code>> {
        let Ok(dest) = self.parse_postfix_expression() else {
            return Ok(None);
        };

        let Ok(equals) = self.consume_token() else {
            return Ok(None);
        };

        if equals.kind != TokenKind::Punctuator(Punctuator::Assignment) {
            return Ok(None);
        }

        let expression = self.parse_expression()?;
        self.expect_semicolon_after_statement()?;

        Ok(Some(AssignStatement {
            range: FileRange::new(dest.range().start(), expression.range().end()),
            dest,
            expression,
        }))
    }

    pub fn parse_function(&mut self) -> Result<FunctionStatement<'source_code>, ParseDiagnostic<'source_code>> {
        let name = self.consume_token()?;
        let name_range = name.range();
        let TokenKind::Identifier(name) = name.kind else {
            return Err(ParseDiagnostic::FunctionStatementExpectedName { token: name });
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
                    ParserErrorBehavior::Propagate => return Err(ParseDiagnostic::ParameterExpectedComma{
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
            if self.peek_punctuator() == Some(Punctuator::RightCurlyBracket) {
                _ = self.consume_token()?;
                break;
            }

            match self.parse_statement() {
                Ok(statement) => body.push(statement),
                Err(ParseDiagnostic::EndOfFile) => break,
                Err(error) => {
                    self.handle_error(error)?;
                    break;
                }
            }
        }

        let range = FileRange::new(name_range.start(), self.previous_end());
        let body = Some(body);

        Ok(FunctionStatement { name, body, parameters, range })
    }

    fn consume_identifier(&mut self, ident_purpose: &'static str, previous: &'source_code str) -> Result<Ranged<&'source_code str>, ParseDiagnostic<'source_code>> {
        let token = self.consume_token()?;
        let range = token.range();
        if let TokenKind::Identifier(name) = token.kind {
            Ok(Ranged::new(range, name))
        } else {
            return Err(ParseDiagnostic::ExpectedIdentifier { token, previous, ident_purpose });
        }
    }

    fn handle_error(&mut self, error: ParseDiagnostic<'source_code>) -> Result<(), ParseDiagnostic<'source_code>> {
        match self.error_behavior {
            ParserErrorBehavior::AttemptToIgnore => {
                self.errors.push(error);
                Ok(())
            }
            ParserErrorBehavior::Propagate => return Err(error),
        }
    }

    #[must_use]
    pub const fn is_at_end(&self) -> bool {
        self.cursor >= self.tokens.len()
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement<'source_code>, ParseDiagnostic<'source_code>> {
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

    fn parse_structure_statement(&mut self) -> Result<Structure<'source_code>, ParseDiagnostic<'source_code>> {
        let name_token = self.consume_token()?;

        let name = Ranged::new(name_token.range(), match name_token.kind {
            TokenKind::Identifier(ident) => ident,
            _ => {
                self.handle_error(ParseDiagnostic::ExpectedNameOfStructuur { token: name_token })?;
                ""
            }
        });

        let mut structure = Structure {
            name,
            fields: Vec::new(),
        };

        self.expect_left_curly_bracket("structuurnaam")?;

        loop {
            if self.peek_punctuator() == Some(Punctuator::RightCurlyBracket) {
                _ = self.consume_token()?;
                break;
            }

            match self.peek_token()?.kind {
                TokenKind::Keyword(Keyword::Veld) => {
                    _ = self.consume_token();
                    structure.fields.push(self.parse_structure_field()?);
                }

                _ => {
                    let token = self.consume_token()?;
                    self.handle_error(ParseDiagnostic::UnexpectedTokenAtStartOfStructureMember { token })?;
                    break;
                }
            }

            if self.peek_punctuator() == Some(Punctuator::Comma) {
                _ = self.consume_token()?;
                continue;
            }

            if self.peek_punctuator() != Some(Punctuator::RightCurlyBracket) {
                self.handle_error(ParseDiagnostic::UnexpectedTokenInsideStructure { token: self.peek_token()?.clone() })?;
                break;
            }
        }

        Ok(structure)
    }

    fn parse_structure_field(&mut self) -> Result<Field<'source_code>, ParseDiagnostic<'source_code>> {
        let name_token = self.consume_token()?;

        let name = Ranged::new(name_token.range(), match name_token.kind {
            TokenKind::Identifier(ident) => ident,
            _ => {
                self.handle_error(ParseDiagnostic::ExpectedNameOfField { token: name_token })?;
                ""
            }
        });

        self.expect_colon("veldnaam")?;
        let ty = self.parse_type()?;
        Ok(Field {
            attributes: Vec::new(),
            name,
            ty,
        })
    }

    fn parse_variable_statement(&mut self) -> Result<VariableStatement<'source_code>, ParseDiagnostic<'source_code>> {
        let name_token = self.consume_token()?;
        let name_range = name_token.range();

        let name = match name_token.kind {
            TokenKind::Identifier(ident) => ident,
            _ => {
                self.handle_error(ParseDiagnostic::ExpectedNameOfVariable { token: name_token })?;
                ""
            }
        };

        let name = Ranged::new(name_range, name);

        let equals = self.consume_token()?;
        if equals.kind != TokenKind::Punctuator(Punctuator::Assignment) {
            self.handle_error(ParseDiagnostic::ExpectedEqualsInsideVariable { token: equals })?;
        }

        let expression = self.parse_expression()?;
        self.expect_semicolon_after_statement()?;

        Ok(VariableStatement {
            range: FileRange::new(name.range().start(), expression.range().end()),
            name,
            expression,
        })
    }

    fn parse_parameter(&mut self) -> Result<Parameter<'source_code>, ParseDiagnostic<'source_code>> {
        let name = self.parse_parameter_name()?;

        self.expect_colon("parameternaam")?;

        let ty = self.parse_type()?;

        Ok(Parameter {
            name,
            ty,
        })
    }

    fn parse_parameter_name(&mut self) -> Result<Ranged<String>, ParseDiagnostic<'source_code>> {
        let name = self.consume_token()?;
        let name_range = name.range();
        let TokenKind::Identifier(name) = name.kind else {
            return Err(ParseDiagnostic::ParameterExpectedName { token: name });
        };

        Ok(Ranged::new(name_range, name.to_string()))
    }

    fn parse_type(&mut self) -> Result<Ranged<Type<'source_code>>, ParseDiagnostic<'source_code>> {
        let name_token = self.consume_token()?;
        let TokenKind::Identifier(name) = name_token.kind else {
            return Err(ParseDiagnostic::TypeExpectedSpecifierName { token: name_token });
        };
        let name = Ranged::new(name_token.range(), name);

        let specifier = match Builtin::type_by_name(name.value()) {
            Some(builtin) => TypeSpecifier::BuiltIn(builtin),
            None => TypeSpecifier::Custom { name },
        };

        let ty = Type {
            specifier: Ranged::new(name_token.range(), specifier),
        };
        Ok(Ranged::new(name_token.range(), ty))
    }

    fn parse_for_statement(&mut self) -> Result<ForStatement<'source_code>, ParseDiagnostic<'source_code>> {
        let keyword = self.consume_token()?.range();
        let iterator = self.consume_token()?;
        let TokenKind::Identifier(iterator_name) = iterator.kind else {
            return Err(ParseDiagnostic::ForStatementExpectedIteratorName { token: iterator });
        };

        let iterator_name = Ranged::new(iterator.range(), iterator_name);

        let in_keyword = self.consume_token()?;
        if in_keyword.kind != TokenKind::Keyword(Keyword::In) {
            return Err(ParseDiagnostic::ForStatementExpectedInKeyword { token: in_keyword, iterator_name });
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

        let file_range = FileRange::new(keyword.start(), self.previous_end());

        Ok(ForStatement { keyword, iterator_name, range, body, file_range })
    }

    fn parse_if_statement(&mut self) -> Result<IfStatement<'source_code>, ParseDiagnostic<'source_code>> {
        let start = self.previous_end();
        let condition = match self.parse_expression() {
            Ok(expression) => expression,
            Err(error) => {
                self.handle_error(error)?;

                Ranged::new(
                    FileRange::new(self.token_end, self.token_end),
                    Expression::Primary(PrimaryExpression::Boolean(true))
                )
            }
        };

        if self.tokens[self.cursor - 1].kind != TokenKind::Punctuator(Punctuator::LeftCurlyBracket) {
            self.expect_left_curly_bracket("als-conditie")?;
        }

        let mut body = Vec::new();
        loop {
            if self.peek_punctuator() == Some(Punctuator::RightCurlyBracket) {
                _ = self.consume_token()?;
                break;
            }

            match self.parse_statement() {
                Ok(statement) => body.push(statement),
                Err(error) => {
                    self.handle_error(error)?;
                    break;
                }
            }
        }

        let range = FileRange::new(start, self.previous_end());

        Ok(IfStatement { condition, body, range })
    }

    fn parse_range(&mut self) -> Result<RangeExpression<'source_code>, ParseDiagnostic<'source_code>> {
        let range_keyword = self.consume_token()?;
        if range_keyword.kind != TokenKind::Keyword(Keyword::Reeks) {
            return Err(ParseDiagnostic::RangeExpectedKeyword { token: range_keyword });
        }

        self.expect_left_paren("reeks")?;

        let start = self.parse_primary_expression()?;

        self.expect_comma("reeks")?;

        let end = self.parse_primary_expression()?;

        self.expect_right_paren("reeks")?;

        Ok(RangeExpression { start, end })
    }

    fn parse_primary_expression(&mut self) -> Result<Ranged<PrimaryExpression<'source_code>>, ParseDiagnostic<'source_code>> {
        let reset = (self.cursor, self.token_begin, self.token_end);
        let token = self.consume_token()?;
        let range = token.range();

        let expression = match token.kind {
            TokenKind::StringLiteral(literal) => Ok(PrimaryExpression::StringLiteral(literal)),
            TokenKind::Integer(integer) => Ok(PrimaryExpression::IntegerLiteral(integer)),
            TokenKind::Identifier(identifier) => Ok(PrimaryExpression::Reference(Ranged::new(token.range(), identifier))),
            TokenKind::TemplateString(template_string) => self.parse_template_string(template_string),
            TokenKind::Keyword(Keyword::Waar) => Ok(PrimaryExpression::Boolean(true)),
            TokenKind::Keyword(Keyword::Onwaar) => Ok(PrimaryExpression::Boolean(false)),
            TokenKind::Keyword(Keyword::Nieuw) => self.parse_structure_instantiation(),

            TokenKind::Punctuator(Punctuator::LeftParenthesis) => {
                let expression = self.parse_expression()?;
                self.expect_right_paren("expressie binnen haakjes")?;
                Ok(PrimaryExpression::Parenthesized(Box::new(expression)))
            }

            _ => {
                (self.cursor, self.token_begin, self.token_end) = reset;
                let replacement_token = PrimaryExpression::Reference(Ranged::new(token.range(), ""));
                self.handle_error(ParseDiagnostic::UnknownStartOfExpression { token })?;
                Ok(replacement_token)
            }
        }?;

        Ok(Ranged::new(range, expression))
    }

    #[allow(unused)] // TODO evaluate further need of this function
    fn parse_ranged<F, T>(&mut self, f: F) -> Result<Ranged<T>, ParseDiagnostic<'source_code>>
            where F: FnOnce(&mut Self) -> Result<T, ParseDiagnostic<'source_code>> {
        let start = self.token_begin;
        let value = f(self)?;
        let end = self.token_end;
        Ok(Ranged::new(FileRange::new(start, end), value))
    }

    pub fn parse_expression(&mut self) -> Result<Ranged<Expression<'source_code>>, ParseDiagnostic<'source_code>> {
        self.parse_relational_expression()
    }

    fn parse_relational_expression(&mut self) -> Result<Ranged<Expression<'source_code>>, ParseDiagnostic<'source_code>> {
        self.parse_bi_expression(Self::parse_additive_expression, &[
            (Punctuator::Equals, BiOperator::Comparison(Comparison::Equality)),
            // TODO the rest
        ])
    }

    fn parse_additive_expression(&mut self) -> Result<Ranged<Expression<'source_code>>, ParseDiagnostic<'source_code>> {
        self.parse_bi_expression(Self::parse_multiplicative_expression, &[
            (Punctuator::PlusSign, BiOperator::Add),
            (Punctuator::HyphenMinus, BiOperator::Subtract),
        ])
    }

    fn parse_multiplicative_expression(&mut self) -> Result<Ranged<Expression<'source_code>>, ParseDiagnostic<'source_code>> {
        self.parse_bi_expression(Self::parse_postfix_expression, &[
            (Punctuator::Asterisk, BiOperator::Multiply),
            (Punctuator::PercentageSign, BiOperator::Modulo),
            (Punctuator::Solidus, BiOperator::Divide),
        ])
    }

    fn parse_bi_expression<F>(&mut self, mut operand: F, operators: &[(Punctuator, BiOperator)]) -> Result<Ranged<Expression<'source_code>>, ParseDiagnostic<'source_code>>
            where F: FnMut(&mut Self) -> Result<Ranged<Expression<'source_code>>, ParseDiagnostic<'source_code>> {
        let mut expr = operand(self)?;

        loop {
            let Ok(next) = self.peek_token() else {
                break;
            };

            let Some((_, operator)) = operators.into_iter().find(|(p, _)| next.kind == TokenKind::Punctuator(*p)).cloned() else {
                break;
            };

            let operator_range = self.consume_token()?.range();
            let operator = Ranged::new(operator_range, operator);

            let lhs = expr;
            let rhs = operand(self)?;
            let range = FileRange::new(lhs.range().start(), rhs.range().end());

            let expression = Expression::BiExpression(BiExpression {
                operator,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            });
            expr = Ranged::new(range, expression);
        }

        Ok(expr)
    }

    fn parse_postfix_expression(&mut self) -> Result<Ranged<Expression<'source_code>>, ParseDiagnostic<'source_code>> {
        let start = self.next_start();
        let mut expression = self.parse_primary_expression()?.map(|x| Expression::Primary(x));

        loop {
            let expr = match self.peek_punctuator() {
                Some(Punctuator::LeftParenthesis) => {
                    let token_left_paren = self.consume_token()?.range();
                    Expression::Postfix(PostfixExpression {
                        lhs: Box::new(expression),
                        kind: PostfixExpressionKind::Call(self.parse_function_call_expression(token_left_paren)?)
                    })
                }

                Some(Punctuator::Period) => {
                    let period = self.consume_token()?;
                    let reset = (self.token_begin, self.token_end, self.cursor);

                    let ident_token = self.consume_token()?;
                    let TokenKind::Identifier(name) = ident_token.kind else {
                        self.handle_error(ParseDiagnostic::PostfixMemberOrReferenceExpectedIdentifier { token: ident_token, period })?;
                        (self.token_begin, self.token_end, self.cursor) = reset;
                        break;
                    };

                    let name = Ranged::new(ident_token.range(), name);

                    if self.peek_punctuator() == Some(Punctuator::LeftParenthesis) {
                        let token_left_paren = self.consume_token()?.range();
                        Expression::Postfix(PostfixExpression {
                            lhs: Box::new(expression),
                            kind: PostfixExpressionKind::MethodCall(MethodCallExpression {
                                method_name: name,
                                call: self.parse_function_call_expression(token_left_paren)?,
                            })
                        })
                    } else {
                        Expression::Postfix(PostfixExpression {
                            lhs: Box::new(expression),
                            kind: PostfixExpressionKind::Member(name),
                        })
                    }
                }

                _ => break,
            };

            let range = FileRange::new(start, self.previous_end());
            expression = Ranged::new(range, expr);
        }

        Ok(expression)
    }

    fn parse_function_call_expression(&mut self, token_left_paren: FileRange) -> Result<FunctionCallExpression<'source_code>, ParseDiagnostic<'source_code>> {
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

        Ok(FunctionCallExpression {
            arguments,
            token_left_paren,
            token_right_paren,
        })
    }

    fn parse_template_string(&self, template_string: Vec<TemplateStringToken<'source_code>>) -> ParseResult<'source_code, PrimaryExpression<'source_code>> {
        let mut parts = Vec::new();

        for token in template_string {
            let part = match token {
                TemplateStringToken::Plain { str, .. } => TemplateStringExpressionPart::String(str),
                TemplateStringToken::Expression(tokens) => {
                    let mut parser = Parser::new(self.path.clone(), &tokens);
                    let expr = parser.parse_expression()?;

                    if parser.cursor < tokens.len() {
                        return Err(ParseDiagnostic::ResidualTokensInTemplateString {
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

    fn peek_token(&self) -> Result<&Token<'source_code>, ParseDiagnostic<'source_code>> {
        match self.tokens.get(self.cursor) {
            Some(token) => Ok(token),
            None => Err(ParseDiagnostic::EndOfFile),
        }
    }

    fn peek_punctuator(&self) -> Option<Punctuator> {
        let Ok(token) = self.peek_token() else { return None };
        match token.kind {
            TokenKind::Punctuator(punctuator) => Some(punctuator),
            _ => None,
        }
    }

    fn consume_token(&mut self) -> Result<Token<'source_code>, ParseDiagnostic<'source_code>> {
        let token = self.peek_token()?.clone();
        self.token_begin = token.begin;
        self.token_end = token.end;
        self.cursor += 1;
        Ok(token)
    }

    fn expect_left_paren(&mut self, context: &'static str) -> Result<FileRange, ParseDiagnostic<'source_code>> {
        let token = self.consume_token()?;

        if token.kind != TokenKind::Punctuator(Punctuator::LeftParenthesis) {
            return Err(ParseDiagnostic::ExpectedLeftParen { token, context });
        }

        Ok(token.range())
    }

    fn expect_right_paren(&mut self, context: &'static str) -> Result<FileRange, ParseDiagnostic<'source_code>> {
        let token = self.consume_token()?;
        let range = token.range();

        if token.kind != TokenKind::Punctuator(Punctuator::RightParenthesis) {
            self.handle_error(ParseDiagnostic::ExpectedRightParen { token, context })?;
        }

        Ok(range)
    }

    fn expect_left_curly_bracket(&mut self, context: &'static str) -> Result<(), ParseDiagnostic<'source_code>> {
        let token = self.consume_token()?;

        if token.kind != TokenKind::Punctuator(Punctuator::LeftCurlyBracket) {
            self.handle_error(ParseDiagnostic::ExpectedLeftCurlyBracket { token, context })?;
        }

        Ok(())
    }

    fn expect_comma(&mut self, context: &'static str) -> Result<(), ParseDiagnostic<'source_code>> {
        let token = self.consume_token()?;

        if token.kind != TokenKind::Punctuator(Punctuator::Comma) {
            return Err(ParseDiagnostic::ExpectedComma { token, context });
        }

        Ok(())
    }

    fn expect_colon(&mut self, context: &'static str) -> Result<(), ParseDiagnostic<'source_code>> {
        let token = self.consume_token()?;

        if token.kind != TokenKind::Punctuator(Punctuator::Colon) {
            return Err(ParseDiagnostic::ExpectedColon { token, context });
        }

        Ok(())
    }

    fn expect_semicolon_after_statement(&mut self) -> Result<(), ParseDiagnostic<'source_code>> {
        let token = match self.consume_token() {
            Ok(token) => token,
            Err(ParseDiagnostic::EndOfFile) => return self.handle_error(ParseDiagnostic::ExpectedSemicolonAfterStatement { token: self.tokens[self.cursor - 1].clone() }),
            Err(e) => return Err(e),
        };

        if token.kind != TokenKind::Punctuator(Punctuator::Semicolon) {
            self.handle_error(ParseDiagnostic::ExpectedSemicolonAfterStatement { token })?;
        }

        Ok(())
    }

    fn previous_end(&self) -> FileLocation {
        match self.tokens.get(self.cursor - 1) {
            Some(token) => token.end,
            None => self.token_begin,
        }
    }

    fn next_start(&self) -> FileLocation {
        match self.tokens.get(self.cursor) {
            Some(token) => token.end,
            None => self.token_end,
        }
    }

    fn parse_attribute(&mut self) -> Result<Attribute<'source_code>, ParseDiagnostic<'source_code>> {
        let name = match self.consume_identifier("Attribuutnaam", "@") {
            Ok(name) => name,
            Err(e) => {
                self.handle_error(e)?;
                Ranged::new(FileRange::default(), "")
            }
        };

        let arguments = if let Some(Punctuator::LeftParenthesis) = self.peek_punctuator() {
            _ = self.consume_token();
            match self.parse_attribute_argument_list() {
                Ok(arguments) => arguments,
                Err(e) => {
                    self.handle_error(e)?;
                    Vec::new()
                }
            }
        } else {
            Vec::new()
        };

        Ok(Attribute {
            name,
            arguments,
        })
    }

    fn parse_attribute_argument_list(&mut self) -> Result<Vec<AttributeArgument<'source_code>>, ParseDiagnostic<'source_code>> {
        let mut arguments = Vec::new();

        loop {
            if self.peek_punctuator() == Some(Punctuator::RightParenthesis) {
                _ = self.consume_token();
                break;
            }

            match self.parse_attribute_argument() {
                Ok(argument) => arguments.push(argument),
                Err(e) => {
                    self.handle_error(e)?;
                    break;
                }
            }

            let token = self.consume_token()?;
            match &token.kind {
                TokenKind::Punctuator(Punctuator::RightParenthesis) => {
                    _ = self.consume_token();
                    break;
                }

                TokenKind::Punctuator(Punctuator::Comma) => {
                    _ = self.consume_token();
                    continue;
                }

                _ => {
                    self.handle_error(ParseDiagnostic::AttributeArgumentExpectedComma { token })?;
                    break;
                }
            }
        }

        Ok(arguments)
    }

    fn parse_attribute_argument(&mut self) -> Result<AttributeArgument<'source_code>, ParseDiagnostic<'source_code>> {
        let name = match self.consume_identifier("Argumentnaam", "(") {
            Ok(name) => name,
            Err(e) => {
                self.handle_error(e)?;
                Ranged::new(FileRange::default(), "")
            }
        };

        self.expect_colon("argumentnaam")?;
        let value = self.parse_primary_expression()?;

        Ok(AttributeArgument {
            name,
            value,
        })
    }

    fn parse_structure_instantiation(&mut self) -> Result<PrimaryExpression<'source_code>, ParseDiagnostic<'source_code>> {
        let name_token = self.peek_token()?;
        let TokenKind::Identifier(name) = name_token.kind else {
            return Err(ParseDiagnostic::ExpectedNameAfterNieuw { token: name_token.clone() });
        };

        let name = Ranged::new(name_token.range(), name);
        _ = self.consume_token();

        self.expect_left_curly_bracket("nieuw")?;

        let mut expr = StructureInstantiationExpression {
            name,
            fields: Vec::new(),
        };

        loop {
            if self.peek_punctuator() == Some(Punctuator::RightCurlyBracket) {
                _ = self.consume_token()?;
                break;
            }

            let token = self.peek_token()?;
            match token.kind {
                TokenKind::Identifier(name) => {
                    let name = Ranged::new(token.range(), name);
                    _ = self.consume_token();

                    self.expect_colon("structuurveldverwijzing")?;
                    let expression = match self.parse_expression() {
                        Ok(expr) => expr,
                        Err(e) => {
                            self.handle_error(e)?;
                            break;
                        }
                    };

                    expr.fields.push(FieldInstantiation {
                        name,
                        value: Box::new(expression),
                    })
                }

                _ => {
                    let token = self.consume_token()?;
                    self.handle_error(ParseDiagnostic::UnexpectedTokenAtStartOfStructureMember { token })?;
                    break;
                }
            }

            if self.peek_punctuator() == Some(Punctuator::Comma) {
                _ = self.consume_token()?;
                continue;
            }

            if self.peek_punctuator() != Some(Punctuator::RightCurlyBracket) {
                self.handle_error(ParseDiagnostic::UnexpectedTokenInsideStructureInstantiation { token: self.peek_token()?.clone() })?;
                break;
            }
        }

        Ok(PrimaryExpression::StructureInstantiation(expr))
    }
}

#[derive(Clone, Debug, thiserror::Error, AsRefStr)]
pub enum ParseDiagnostic<'source_code> {
    #[error("Onverwacht einde van het bestand")]
    EndOfFile,

    #[error("Komma `,` of gesloten rond haakje `)` verwacht binnen attribuutargumentlijst, maar kreeg: {token}")]
    AttributeArgumentExpectedComma { token: Token<'source_code> },

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

    #[error("Naam van structuur verwacht, maar kreeg: {token}")]
    ExpectedNameAfterNieuw { token: Token<'source_code> },

    #[error("Naam van structuurveld verwacht, maar kreeg: {token}")]
    ExpectedNameOfField { token: Token<'source_code> },

    #[error("Naam van structuur verwacht, maar kreeg: {token}")]
    ExpectedNameOfStructuur { token: Token<'source_code> },

    #[error("Naam van stelling verwacht, maar kreeg: {token}")]
    ExpectedNameOfVariable { token: Token<'source_code> },

    #[error("Is-teken `=` verwacht tussen naam van stelling en de toewijzing, maar kreeg: {token}")]
    ExpectedEqualsInsideVariable { token: Token<'source_code> },

    #[error("Puntkomma verwacht ';' na statement, maar kreeg: {token}")]
    ExpectedSemicolonAfterStatement { token: Token<'source_code> },

    #[error("Functienaam verwacht na `functie`, maar kreeg: {token}")]
    FunctionStatementExpectedName { token: Token<'source_code> },

    #[error("{ident_purpose} verwacht na `{previous}`, maar kreeg: {token}")]
    ExpectedIdentifier { token: Token<'source_code>, previous: &'source_code str, ident_purpose: &'static str },

    #[error("Iteratornaam verwacht na `volg`, maar kreeg: {token}")]
    ForStatementExpectedIteratorName { token: Token<'source_code> },

    #[error("Sleutelwoord 'in' verwacht na `volg {}`, maar kreeg: {token}", iterator_name.value())]
    ForStatementExpectedInKeyword { token: Token<'source_code>, iterator_name: Ranged<&'source_code str> },

    #[error("Parameternaam verwacht, maar kreeg: {token}")]
    ParameterExpectedName { token: Token<'source_code> },

    #[error("Komma `,` of gesloten rond haakje `)` verwacht binnen parameterlijst, maar kreeg: {}", token.as_ref().map(|x| x as &dyn Display).unwrap_or_else(|| &"(niets)" as &'static dyn Display))]
    ParameterExpectedComma { token: Option<Token<'source_code>> },

    #[error("Methode of structuurlid verwacht na punt `.`, maar kreeg: {token}")]
    PostfixMemberOrReferenceExpectedIdentifier { token: Token<'source_code>, period: Token<'source_code> },

    #[error("Sleutelwoord `reeks` verwacht, maar kreeg: {token}")]
    RangeExpectedKeyword { token: Token<'source_code> },

    #[error("Resterende token na expressie binnen sjabloonslinger: {token}")]
    ResidualTokensInTemplateString { token: Token<'source_code> },

    #[error("Parametertype verwacht, maar kreeg: {token}")]
    TypeExpectedSpecifierName { token: Token<'source_code> },

    #[error("Onbekend start van een expressie: {token}")]
    UnknownStartOfExpression { token: Token<'source_code> },

    #[error("Onbekend start van een structuurlid: {token}")]
    UnexpectedTokenAtStartOfStructureMember { token: Token<'source_code> },

    #[error("Onverwachte token binnen structuur: `{token}`")]
    UnexpectedTokenInsideStructure { token: Token<'source_code> },

    #[error("Verwijzing naar structuurveld verwacht, maar kreeg: {token}")]
    UnexpectedTokenAtStartOfStructureInstantiation { token: Token<'source_code> },

    #[error("Onverwachte token binnen structuur: `{token}`")]
    UnexpectedTokenInsideStructureInstantiation { token: Token<'source_code> },
}

impl<'source_code> ParseDiagnostic<'source_code> {
    pub fn token(&self) -> Option<&Token<'source_code>> {
        match self {
            Self::EndOfFile => None,
            Self::AttributeArgumentExpectedComma { token } => Some(token),
            Self::StatementInvalidStart { token } => Some(token),
            Self::ExpressionFunctionCallNotStartingWithIdentifier { token } => Some(token),
            Self::ExpectedLeftCurlyBracket { token, .. } => Some(token),
            Self::ExpectedLeftParen { token, .. } => Some(token),
            Self::ExpectedRightParen { token, .. } => Some(token),
            Self::ExpectedColon { token, .. } => Some(token),
            Self::ExpectedComma { token, .. } => Some(token),
            Self::ExpectedSemicolonAfterStatement { token, .. } => Some(token),
            Self::ExpectedNameAfterNieuw { token } => Some(token),
            Self::ExpectedNameOfField { token } => Some(token),
            Self::ExpectedNameOfStructuur { token } => Some(token),
            Self::ExpectedNameOfVariable { token } => Some(token),
            Self::ExpectedIdentifier { token, .. } => Some(token),
            Self::ExpectedEqualsInsideVariable { token } => Some(token),
            Self::FunctionStatementExpectedName { token } => Some(token),
            Self::ForStatementExpectedIteratorName { token } => Some(token),
            Self::ForStatementExpectedInKeyword { token, .. } => Some(token),
            Self::ParameterExpectedName { token } => Some(token),
            Self::ParameterExpectedComma { token } => token.as_ref(),
            Self::PostfixMemberOrReferenceExpectedIdentifier { token, .. } => Some(token),
            Self::RangeExpectedKeyword { token } => Some(token),
            Self::ResidualTokensInTemplateString { token } => Some(token),
            Self::TypeExpectedSpecifierName { token } => Some(token),
            Self::UnknownStartOfExpression { token } => Some(token),
            Self::UnexpectedTokenAtStartOfStructureMember { token } => Some(token),
            Self::UnexpectedTokenInsideStructure { token } => Some(token),
            Self::UnexpectedTokenAtStartOfStructureInstantiation { token } => Some(token),
            Self::UnexpectedTokenInsideStructureInstantiation { token } => Some(token),
        }
    }

    pub fn range(&self) -> Option<FileRange> {
        match self {
            Self::PostfixMemberOrReferenceExpectedIdentifier { period, .. } => Some(period.range()),
            _ => Some(self.token()?.range()),
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
        let mut parser = Parser::new(PathBuf::new(), &tokens);
        let expression = parser.parse_expression().unwrap();
        assert!(
            matches!(
                expression.value(),
                Expression::Postfix(..)
            ),
            "Invalid parse: {expression:?}"
        );
    }
}
