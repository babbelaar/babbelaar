// Copyright (C) 2023 - 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::path::PathBuf;

use strum::AsRefStr;

use crate::{
    statement::ReturnStatement, AssignStatement, Attribute, AttributeArgument, BabString, BiExpression, BiOperator, Builtin, BuiltinType, Comparison, Expression, Field, FieldInstantiation, FileLocation, FileRange, ForStatement, FunctionCallExpression, FunctionStatement, IfStatement, Keyword, Method, MethodCallExpression, Parameter, ParseTree, PostfixExpression, PostfixExpressionKind, PrimaryExpression, Punctuator, RangeExpression, Ranged, Statement, StatementKind, Structure, StructureInstantiationExpression, TemplateStringExpressionPart, TemplateStringToken, Token, TokenKind, Type, TypeSpecifier, VariableStatement
};

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Clone)]
pub struct Parser<'tokens> {
    pub path: PathBuf,
    tokens: &'tokens [Token],
    pub cursor: usize,
    pub token_begin: FileLocation,
    pub token_end: FileLocation,
    diagnostics: Vec<ParseDiagnostic>,
    end_of_file_token: Token,
}

impl<'tokens> Parser<'tokens> {
    pub fn new(path: PathBuf, tokens: &'tokens [Token]) -> Self {
        let end = tokens.last().map(|x| x.end).unwrap_or_default();

        Self {
            path,
            token_begin: Default::default(),
            token_end: Default::default(),
            diagnostics: Vec::new(),
            tokens,
            cursor: 0,
            end_of_file_token: Token {
                kind: TokenKind::Identifier(BabString::empty()),
                begin: end,
                end,
            }
        }
    }

    pub fn parse_tree(&mut self) -> ParseTree {
        let mut tree = ParseTree::new(self.path.clone());

        while !self.is_at_end() {
            match self.parse_statement() {
                Ok(statement) => tree.push(statement),
                Err(ParseError::EndOfFile) => break,
            }
        }

        tree
    }

    #[must_use]
    pub fn diagnostics(&self) -> &[ParseDiagnostic] {
        &self.diagnostics
    }

    #[must_use]
    pub fn into_diagnostics(self) -> Vec<ParseDiagnostic> {
        self.diagnostics
    }

    pub fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        let start = self.peek_token()?.begin;
        let mut attributes = Vec::new();

        while let Some(Punctuator::AtSign) = self.peek_punctuator() {
            match self.parse_attribute() {
                Ok(attr) => attributes.push(attr),
                Err(e) => self.handle_error(e),
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

            TokenKind::Keyword(Keyword::Werkwijze) => {
                _ = self.consume_token().ok();
                StatementKind::Function(self.parse_function(FunctionParsingContext::Function)?)
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

    fn parse_assign_statement(&mut self) -> Result<Option<AssignStatement>, ParseError> {
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

    pub fn parse_function(&mut self, ctx: FunctionParsingContext) -> Result<FunctionStatement, ParseError> {
        let name = self.consume_token()?;
        let name_range = name.range();
        let name = match name.kind {
            TokenKind::Identifier(name) => name,
            _ => {
                self.emit_diagnostic(ParseDiagnostic::FunctionStatementExpectedName { token: name.clone() });
                BabString::empty()
            }
        };
        let name = Ranged::new(name_range, name);

        self.expect_left_paren("werkwijzenaam")?;

        let mut parameters = Vec::new();
        while self.peek_punctuator() != Some(Punctuator::RightParenthesis) {
            match self.parse_parameter() {
                Ok(parameter) => parameters.push(parameter),
                Err(error) => self.handle_error(error),
            }

            match self.peek_punctuator() {
                Some(Punctuator::Comma) => {
                    _ = self.consume_token()?;
                    continue;
                }

                Some(Punctuator::RightParenthesis) => break,

                _ => {
                    self.emit_diagnostic(ParseDiagnostic::ParameterExpectedComma{
                        token: self.peek_token().ok().cloned().unwrap_or_else(|| self.tokens[self.tokens.len() - 1].clone()),
                    });
                    continue;
                }
            }
        }

        let parameters_right_paren_range = self.expect_right_paren("werkwijzenaam")?;

        let token = self.consume_token();
        let has_body = match token.as_ref().map(|x| &x.kind) {
            Ok(TokenKind::Punctuator(Punctuator::LeftCurlyBracket)) => {
                true
            }

            Ok(TokenKind::Punctuator(Punctuator::Semicolon)) | Ok(TokenKind::Punctuator(Punctuator::Comma)) => {
                if ctx.require_body() {
                    let range = parameters_right_paren_range.end().as_zero_range();
                    self.emit_diagnostic(ParseDiagnostic::FunctionMustHaveDefinition { semicolon: token.unwrap(), range });
                }

                false
            }

            _ => {
                let range = parameters_right_paren_range.end().as_zero_range();
                let token = token.unwrap_or_else(|_| self.end_of_file_token.clone());
                self.emit_diagnostic(ParseDiagnostic::ExpectedSemicolonOrCurlyBracketForFunction { token, range });
                false
            }
        };

        let body = if has_body {
            let mut body = Vec::new();
            loop {
                if self.peek_punctuator() == Some(Punctuator::RightCurlyBracket) {
                    _ = self.consume_token()?;
                    break;
                }

                match self.parse_statement() {
                    Ok(statement) => body.push(statement),
                    Err(ParseError::EndOfFile) => break,
                }
            }

            Some(body)
        } else {
            None
        };

        let range = FileRange::new(name_range.start(), self.previous_end());

        Ok(FunctionStatement { name, body, parameters, parameters_right_paren_range, range })
    }

    fn consume_identifier(&mut self, ident_purpose: &'static str, previous: BabString) -> Result<Ranged<BabString>, ParseError> {
        let token = self.consume_token()?;
        let range = token.range();
        if let TokenKind::Identifier(name) = token.kind {
            Ok(Ranged::new(range, name))
        } else {
            self.emit_diagnostic(ParseDiagnostic::ExpectedIdentifier { token, previous, ident_purpose });
            Ok(Ranged::new(FileRange::new(range.start(), range.start()), BabString::empty()))
        }
    }

    fn emit_diagnostic(&mut self, error: ParseDiagnostic){
        self.diagnostics.push(error);
    }

    fn handle_error(&mut self, _: ParseError) {}

    #[must_use]
    pub const fn is_at_end(&self) -> bool {
        self.cursor >= self.tokens.len()
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement, ParseError> {
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

            Err(error) => {
                self.handle_error(error);
                Ok(ReturnStatement {
                    expression: None,
                })
            }
        }
    }

    fn parse_structure_statement(&mut self) -> Result<Structure, ParseError> {
        let name_token = self.consume_token()?;

        let name = Ranged::new(name_token.range(), match name_token.kind {
            TokenKind::Identifier(ident) => ident,
            _ => {
                self.emit_diagnostic(ParseDiagnostic::ExpectedNameOfStructuur { token: name_token });
                BabString::empty()
            }
        });

        let left_curly_range = self.expect_left_curly_bracket("structuurnaam")?;

        let mut structure = Structure {
            name,
            left_curly_range,
            fields: Vec::new(),
            methods: Vec::new(),
        };

        loop {
            if self.peek_punctuator() == Some(Punctuator::RightCurlyBracket) {
                _ = self.consume_token()?;
                break;
            }

            let mut require_comma = true;

            let peeked_token = self.peek_token()?;
            match peeked_token.kind {
                TokenKind::Keyword(Keyword::Veld) => {
                    _ = self.consume_token();
                    structure.fields.push(self.parse_structure_field()?);
                }

                TokenKind::Keyword(Keyword::Werkwijze) => {
                    let start = self.consume_token()?.begin;

                    let function = self.parse_function(FunctionParsingContext::Method)?;

                    let range = FileRange::new(start, self.token_end);

                    structure.methods.push(Method {
                        range,
                        function,
                    });

                    require_comma = false;
                }

                TokenKind::Identifier(..) => {
                    let token = self.consume_token()?;
                    let next_token = self.peek_token();
                    match next_token.map(|x| &x.kind) {
                        Ok(TokenKind::Punctuator(Punctuator::Colon)) => {
                            self.emit_diagnostic(ParseDiagnostic::ExpectedStructureMemberPrefixVeld { token });
                        }
                        Ok(TokenKind::Punctuator(Punctuator::LeftParenthesis)) => {
                            self.emit_diagnostic(ParseDiagnostic::ExpectedStructureMethodPrefixWerkwijze { token });
                        }
                        _ => {
                            self.emit_diagnostic(ParseDiagnostic::UnexpectedTokenAtStartOfStructureMember { token });
                            break;
                        }
                    }
                    break;
                }

                _ => {
                    let token = self.consume_token()?;
                    self.emit_diagnostic(ParseDiagnostic::UnexpectedTokenAtStartOfStructureMember { token });
                    break;
                }
            }

            if self.peek_punctuator() == Some(Punctuator::Comma) {
                _ = self.consume_token()?;
                continue;
            }

            if require_comma && self.peek_punctuator() != Some(Punctuator::RightCurlyBracket) {
                self.emit_diagnostic(ParseDiagnostic::ExpectedCommaAfterStructureMember { token: self.peek_token()?.clone(), location: self.token_end });
                break;
            }
        }

        Ok(structure)
    }

    fn parse_structure_field(&mut self) -> Result<Field, ParseError> {
        let name_token = self.consume_token()?;

        let name = Ranged::new(name_token.range(), match name_token.kind {
            TokenKind::Identifier(ident) => ident,
            _ => {
                self.emit_diagnostic(ParseDiagnostic::ExpectedNameOfField { token: name_token });
                BabString::empty()
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

    fn parse_variable_statement(&mut self) -> Result<VariableStatement, ParseError> {
        let name_token = self.consume_token()?;
        let name_range = name_token.range();

        let name = match name_token.kind {
            TokenKind::Identifier(ident) => ident,
            _ => {
                self.emit_diagnostic(ParseDiagnostic::ExpectedNameOfVariable { token: name_token });
                BabString::empty()
            }
        };

        let name = Ranged::new(name_range, name);

        let equals = self.consume_token()?;
        if equals.kind != TokenKind::Punctuator(Punctuator::Assignment) {
            self.emit_diagnostic(ParseDiagnostic::ExpectedEqualsInsideVariable { token: equals });
        }

        let expression = self.parse_expression()?;
        self.expect_semicolon_after_statement()?;

        Ok(VariableStatement {
            range: FileRange::new(name.range().start(), expression.range().end()),
            name,
            expression,
        })
    }

    fn parse_parameter(&mut self) -> Result<Parameter, ParseError> {
        let name = self.parse_parameter_name()?;

        self.expect_colon("parameternaam")?;

        let ty = self.parse_type()?;

        Ok(Parameter {
            name,
            ty,
        })
    }

    fn parse_parameter_name(&mut self) -> Result<Ranged<BabString>, ParseError> {
        let name = self.consume_token()?;
        let name_range = name.range();
        let TokenKind::Identifier(name) = name.kind else {
            self.emit_diagnostic(ParseDiagnostic::ParameterExpectedName { token: name });
            return Ok(Ranged::new(name_range.start().as_zero_range(), BabString::empty()))
        };

        Ok(Ranged::new(name_range, name))
    }

    fn parse_type(&mut self) -> Result<Ranged<Type>, ParseError> {
        let name_token = self.consume_token()?;
        let name_range = name_token.range();
        let TokenKind::Identifier(ref name) = name_token.kind else {
            self.emit_diagnostic(ParseDiagnostic::TypeExpectedSpecifierName { token: name_token });
            return Ok(Ranged::new(name_range, Type {
                specifier: Ranged::new(name_range, TypeSpecifier::BuiltIn(BuiltinType::Null)),
            }))
        };
        let name = Ranged::new(name_token.range(), name.clone());

        let specifier = match Builtin::type_by_name(name.value()) {
            Some(builtin) => TypeSpecifier::BuiltIn(builtin),
            None => TypeSpecifier::Custom { name },
        };

        let ty = Type {
            specifier: Ranged::new(name_token.range(), specifier),
        };
        Ok(Ranged::new(name_token.range(), ty))
    }

    fn parse_for_statement(&mut self) -> Result<ForStatement, ParseError> {
        let keyword = self.consume_token()?.range();
        let iterator = self.consume_token()?;
        let iterator_name = match &iterator.kind {
            TokenKind::Identifier(iterator_name) => iterator_name.clone(),
            _ => {
                self.emit_diagnostic(ParseDiagnostic::ForStatementExpectedIteratorName { token: iterator.clone() });
                BabString::empty()
            }
        };

        let iterator_name = Ranged::new(iterator.range(), iterator_name);

        let in_keyword = self.consume_token()?;
        if in_keyword.kind != TokenKind::Keyword(Keyword::In) {
            self.emit_diagnostic(ParseDiagnostic::ForStatementExpectedInKeyword { token: in_keyword, iterator_name: iterator_name.clone() });
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
                Err(error) => self.handle_error(error),
            }
        }

        let file_range = FileRange::new(keyword.start(), self.previous_end());

        Ok(ForStatement { keyword, iterator_name, range, body, file_range })
    }

    fn parse_if_statement(&mut self) -> Result<IfStatement, ParseError> {
        let start = self.previous_end();
        let condition = match self.parse_expression() {
            Ok(expression) => expression,
            Err(error) => {
                self.handle_error(error);

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
                    self.handle_error(error);
                    break;
                }
            }
        }

        let range = FileRange::new(start, self.previous_end());

        Ok(IfStatement { condition, body, range })
    }

    fn parse_range(&mut self) -> Result<RangeExpression, ParseError> {
        let range_keyword = self.consume_token()?;
        if range_keyword.kind != TokenKind::Keyword(Keyword::Reeks) {
            self.emit_diagnostic(ParseDiagnostic::RangeExpectedKeyword { token: range_keyword });
        }

        self.expect_left_paren("reeks")?;

        let start = self.parse_primary_expression()?;

        self.expect_comma("reeks")?;

        let end = self.parse_primary_expression()?;

        self.expect_right_paren("reeks")?;

        Ok(RangeExpression { start, end })
    }

    fn parse_primary_expression(&mut self) -> ParseResult<Ranged<PrimaryExpression>> {
        let reset = (self.cursor, self.token_begin, self.token_end);
        let token = self.consume_token()?;
        let range = token.range();

        let expression = match token.kind {
            TokenKind::StringLiteral(literal) => Ok(PrimaryExpression::StringLiteral(literal)),
            TokenKind::Integer(integer) => Ok(PrimaryExpression::IntegerLiteral(integer)),
            TokenKind::Identifier(ref identifier) => Ok(PrimaryExpression::Reference(Ranged::new(token.range(), identifier.clone()))),
            TokenKind::TemplateString(template_string) => self.parse_template_string(template_string),
            TokenKind::Keyword(Keyword::Waar) => Ok(PrimaryExpression::Boolean(true)),
            TokenKind::Keyword(Keyword::Onwaar) => Ok(PrimaryExpression::Boolean(false)),
            TokenKind::Keyword(Keyword::Nieuw) => self.parse_structure_instantiation(range.start()),
            TokenKind::Keyword(Keyword::Dit) => Ok(PrimaryExpression::ReferenceThis),

            TokenKind::Punctuator(Punctuator::LeftParenthesis) => {
                let expression = self.parse_expression()?;
                self.expect_right_paren("expressie binnen haakjes")?;
                Ok(PrimaryExpression::Parenthesized(Box::new(expression)))
            }

            _ => {
                (self.cursor, self.token_begin, self.token_end) = reset;
                let replacement_token = PrimaryExpression::Reference(Ranged::new(token.range(), BabString::empty()));
                self.emit_diagnostic(ParseDiagnostic::UnknownStartOfExpression { token });
                return Ok(Ranged::new(FileRange::new(self.token_begin, self.token_end), replacement_token));
            }
        }?;

        let range = FileRange::new(range.start(), self.token_end);

        Ok(Ranged::new(range, expression))
    }

    #[allow(unused)] // TODO evaluate further need of this function
    fn parse_ranged<F, T>(&mut self, f: F) -> Result<Ranged<T>, ParseError>
            where F: FnOnce(&mut Self) -> Result<T, ParseError> {
        let start = self.token_begin;
        let value = f(self)?;
        let end = self.token_end;
        Ok(Ranged::new(FileRange::new(start, end), value))
    }

    pub fn parse_expression(&mut self) -> Result<Ranged<Expression>, ParseError> {
        self.parse_relational_expression()
    }

    fn parse_relational_expression(&mut self) -> Result<Ranged<Expression>, ParseError> {
        self.parse_bi_expression(Self::parse_additive_expression, &[
            (Punctuator::Equals, BiOperator::Comparison(Comparison::Equality)),
            // TODO the rest
        ])
    }

    fn parse_additive_expression(&mut self) -> Result<Ranged<Expression>, ParseError> {
        self.parse_bi_expression(Self::parse_multiplicative_expression, &[
            (Punctuator::PlusSign, BiOperator::Add),
            (Punctuator::HyphenMinus, BiOperator::Subtract),
        ])
    }

    fn parse_multiplicative_expression(&mut self) -> Result<Ranged<Expression>, ParseError> {
        self.parse_bi_expression(Self::parse_postfix_expression, &[
            (Punctuator::Asterisk, BiOperator::Multiply),
            (Punctuator::PercentageSign, BiOperator::Modulo),
            (Punctuator::Solidus, BiOperator::Divide),
        ])
    }

    fn parse_bi_expression<F>(&mut self, mut operand: F, operators: &[(Punctuator, BiOperator)]) -> Result<Ranged<Expression>, ParseError>
            where F: FnMut(&mut Self) -> Result<Ranged<Expression>, ParseError> {
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

    fn parse_postfix_expression(&mut self) -> Result<Ranged<Expression>, ParseError> {
        let mut expression = self.parse_primary_expression()?.map(|x| Expression::Primary(x));
        let start = expression.range().start();

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
                    let Ok(period) = self.consume_token() else {
                        break;
                    };

                    let reset = (self.token_begin, self.token_end, self.cursor);

                    let Ok(ident_token) = self.consume_token() else {
                        (self.token_begin, self.token_end, self.cursor) = reset;
                        break;
                    };

                    let TokenKind::Identifier(name) = &ident_token.kind else {
                        self.emit_diagnostic(ParseDiagnostic::PostfixMemberOrReferenceExpectedIdentifier { token: ident_token, period });
                        (self.token_begin, self.token_end, self.cursor) = reset;
                        break;
                    };

                    let name = Ranged::new(ident_token.range(), name.clone());

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

    fn parse_function_call_expression(&mut self, token_left_paren: FileRange) -> Result<FunctionCallExpression, ParseError> {
        let mut arguments = Vec::new();
        while let Ok(token) = self.peek_token() {
            if token.kind == TokenKind::Punctuator(Punctuator::RightParenthesis) {
                self.consume_token()?;
                break;
            }

            if !arguments.is_empty() {
                if let Err(error) = self.expect_comma("argument in werkwijzeaanroeping") {
                    self.handle_error(error);
                    break;
                }
            }

            match self.parse_expression() {
                Ok(expr) => arguments.push(expr),
                Err(error) => {
                    self.handle_error(error);
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

    fn parse_template_string(&mut self, template_string: Vec<TemplateStringToken>) -> ParseResult<PrimaryExpression> {
        let mut parts = Vec::new();

        for token in template_string {
            let part = match token {
                TemplateStringToken::Plain { str, .. } => TemplateStringExpressionPart::String(str),
                TemplateStringToken::Expression(tokens) => {
                    let mut parser = Parser::new(self.path.clone(), &tokens);
                    match parser.parse_expression() {
                        Ok(expr) => {
                            if parser.cursor < tokens.len() {
                                self.emit_diagnostic(ParseDiagnostic::ResidualTokensInTemplateString {
                                    token: tokens[parser.cursor].clone(),
                                    range: FileRange::new(tokens[parser.cursor].begin, tokens.last().unwrap().end),
                                });
                            }

                            TemplateStringExpressionPart::Expression(expr)
                        }
                        Err(e) => {
                            self.handle_error(e);
                            TemplateStringExpressionPart::String(BabString::empty())
                        }
                    }
                }
            };

            parts.push(part);
        }

        Ok(PrimaryExpression::TemplateString { parts })
    }

    fn peek_token(&self) -> ParseResult<&Token> {
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

    fn consume_token(&mut self) -> Result<Token, ParseError> {
        let token = self.peek_token()?.clone();
        self.token_begin = token.begin;
        self.token_end = token.end;
        self.cursor += 1;
        Ok(token)
    }

    fn expect_left_paren(&mut self, context: &'static str) -> Result<FileRange, ParseError> {
        let token = self.consume_token()?;

        if token.kind != TokenKind::Punctuator(Punctuator::LeftParenthesis) {
            self.emit_diagnostic(ParseDiagnostic::ExpectedLeftParen { token: token.clone(), context });
        }

        Ok(token.range())
    }

    fn expect_right_paren(&mut self, context: &'static str) -> Result<FileRange, ParseError> {
        let token = self.consume_token()?;
        let range = token.range();

        if token.kind != TokenKind::Punctuator(Punctuator::RightParenthesis) {
            self.emit_diagnostic(ParseDiagnostic::ExpectedRightParen { token, context });
        }

        Ok(range)
    }

    fn expect_left_curly_bracket(&mut self, context: &'static str) -> Result<FileRange, ParseError> {
        let token = self.consume_token()?;
        let range = token.range();

        if token.kind != TokenKind::Punctuator(Punctuator::LeftCurlyBracket) {
            self.emit_diagnostic(ParseDiagnostic::ExpectedLeftCurlyBracket { token, context });
        }

        Ok(range)
    }

    fn expect_comma(&mut self, context: &'static str) -> Result<(), ParseError> {
        let token = self.consume_token()?;

        if token.kind != TokenKind::Punctuator(Punctuator::Comma) {
            self.emit_diagnostic(ParseDiagnostic::ExpectedComma { token, context });
        }

        Ok(())
    }

    fn expect_colon(&mut self, context: &'static str) -> Result<(), ParseError> {
        let token = self.consume_token()?;

        if token.kind != TokenKind::Punctuator(Punctuator::Colon) {
            let range = FileRange::new(self.tokens[self.cursor.saturating_sub(2)].end, token.begin);
            self.emit_diagnostic(ParseDiagnostic::ExpectedColon { token, range, context });
        }

        Ok(())
    }

    fn expect_semicolon_after_statement(&mut self) -> Result<(), ParseError> {
        let token = match self.consume_token() {
            Ok(token) => token,
            Err(ParseError::EndOfFile) => {
                self.emit_diagnostic(ParseDiagnostic::ExpectedSemicolonAfterStatement { token: self.tokens[self.cursor - 1].clone() });
                return Ok(())
            },
        };

        if token.kind != TokenKind::Punctuator(Punctuator::Semicolon) {
            self.emit_diagnostic(ParseDiagnostic::ExpectedSemicolonAfterStatement { token });
        }

        Ok(())
    }

    fn previous_end(&self) -> FileLocation {
        match self.tokens.get(self.cursor - 1) {
            Some(token) => token.end,
            None => self.token_begin,
        }
    }

    fn parse_attribute(&mut self) -> Result<Attribute, ParseError> {
        let at_sign = self.consume_token()?;

        let name = match self.consume_identifier("Attribuutnaam", BabString::new_static("@")) {
            Ok(name) => name,
            Err(e) => {
                self.handle_error(e);
                Ranged::new(FileRange::default(), BabString::empty())
            }
        };

        let arguments = if let Some(Punctuator::LeftParenthesis) = self.peek_punctuator() {
            _ = self.consume_token();
            match self.parse_attribute_argument_list() {
                Ok(arguments) => arguments,
                Err(e) => {
                    self.handle_error(e);
                    Vec::new()
                }
            }
        } else {
            Vec::new()
        };

        Ok(Attribute {
            at_range: at_sign.range(),
            name,
            arguments,
        })
    }

    fn parse_attribute_argument_list(&mut self) -> Result<Vec<AttributeArgument>, ParseError> {
        let mut arguments = Vec::new();

        loop {
            if self.peek_punctuator() == Some(Punctuator::RightParenthesis) {
                _ = self.consume_token();
                break;
            }

            match self.parse_attribute_argument() {
                Ok(argument) => arguments.push(argument),
                Err(e) => {
                    self.handle_error(e);
                    break;
                }
            }

            let token = self.consume_token()?;
            match &token.kind {
                TokenKind::Punctuator(Punctuator::RightParenthesis) => break,

                TokenKind::Punctuator(Punctuator::Comma) => continue,

                _ => {
                    self.emit_diagnostic(ParseDiagnostic::AttributeArgumentExpectedComma { token });
                    break;
                }
            }
        }

        Ok(arguments)
    }

    fn parse_attribute_argument(&mut self) -> Result<AttributeArgument, ParseError> {
        let name = match self.consume_identifier("Argumentnaam", BabString::new_static("(")) {
            Ok(name) => name,
            Err(e) => {
                self.handle_error(e);
                Ranged::new(FileRange::default(), BabString::empty())
            }
        };

        self.expect_colon("argumentnaam")?;
        let value = self.parse_primary_expression()?;

        Ok(AttributeArgument {
            name,
            value,
        })
    }

    fn parse_structure_instantiation(&mut self, start: FileLocation) -> ParseResult<PrimaryExpression> {
        let name_token = self.peek_token()?;
        let name = match &name_token.kind {
            TokenKind::Identifier(name) => Ranged::new(name_token.range(), name.clone()),

            _ => {
                self.emit_diagnostic(ParseDiagnostic::ExpectedNameAfterNieuw { token: name_token.clone() });
                Ranged::new(FileRange::new(self.token_begin, self.token_begin), BabString::empty())
            }
        };

        _ = self.consume_token();

        let left_curly_bracket = self.expect_left_curly_bracket("nieuw")?;

        let mut expr = StructureInstantiationExpression {
            name,
            fields: Vec::new(),
            range: FileRange::new(start, start),
            left_curly_bracket,
            right_curly_bracket: FileRange::default(),
        };

        loop {
            if self.peek_punctuator() == Some(Punctuator::RightCurlyBracket) {
                expr.right_curly_bracket = self.consume_token()?.range();
                break;
            }

            let token = self.peek_token()?;
            match &token.kind {
                TokenKind::Identifier(name) => {
                    let name = Ranged::new(token.range(), name.clone());
                    _ = self.consume_token();

                    self.expect_colon("structuurveldverwijzing")?;
                    let expression = match self.parse_expression() {
                        Ok(expr) => expr,
                        Err(e) => {
                            self.handle_error(e);
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
                    self.emit_diagnostic(ParseDiagnostic::UnexpectedTokenAtStartOfStructureMember { token });
                    break;
                }
            }

            if self.peek_punctuator() == Some(Punctuator::Comma) {
                _ = self.consume_token()?;
                continue;
            }

            if self.peek_punctuator() != Some(Punctuator::RightCurlyBracket) {
                self.emit_diagnostic(ParseDiagnostic::UnexpectedTokenInsideStructureInstantiation { token: self.peek_token()?.clone() });
                break;
            }
        }

        if expr.right_curly_bracket == FileRange::default() {
            expr.right_curly_bracket = FileRange::new(self.token_begin, self.token_end);
        }

        expr.range = FileRange::new(start, self.token_end);

        Ok(PrimaryExpression::StructureInstantiation(expr))
    }
}

#[derive(Clone, Debug, thiserror::Error, AsRefStr)]
pub enum ParseDiagnostic {
    #[error("Komma `,` of gesloten rond haakje `)` verwacht binnen attribuutargumentlijst, maar kreeg: {token}")]
    AttributeArgumentExpectedComma { token: Token },

    #[error("Ongeldig start van een statement: {token}")]
    StatementInvalidStart{ token: Token },

    #[error("Open accolade verwacht `{{` na {context}, maar kreeg: {token}")]
    ExpectedLeftCurlyBracket { token: Token, context: &'static str },

    #[error("Open rond haakje verwacht '(' na {context}, maar kreeg: {token}")]
    ExpectedLeftParen { token: Token, context: &'static str },

    #[error("Gesloten rond haakje verwacht ')' na {context}, maar kreeg: {token}")]
    ExpectedRightParen { token: Token, context: &'static str },

    #[error("Dubbele punt verwacht ':' na {context}, maar kreeg: {token}")]
    ExpectedColon { token: Token, range: FileRange, context: &'static str },

    #[error("Komma verwacht ',' na {context}, maar kreeg: {token}")]
    ExpectedComma { token: Token, context: &'static str },

    #[error("Na een structuurlid hoort een komma `;`")]
    ExpectedCommaAfterStructureMember { token: Token, location: FileLocation },

    #[error("Naam van structuur verwacht, maar kreeg: {token}")]
    ExpectedNameAfterNieuw { token: Token },

    #[error("Naam van structuurveld verwacht, maar kreeg: {token}")]
    ExpectedNameOfField { token: Token },

    #[error("Naam van structuur verwacht, maar kreeg: {token}")]
    ExpectedNameOfStructuur { token: Token },

    #[error("Naam van stelling verwacht, maar kreeg: {token}")]
    ExpectedNameOfVariable { token: Token },

    #[error("Is-teken `=` verwacht tussen naam van stelling en de toewijzing, maar kreeg: {token}")]
    ExpectedEqualsInsideVariable { token: Token },

    #[error("Puntkomma verwacht ';' na statement, maar kreeg: {token}")]
    ExpectedSemicolonAfterStatement { token: Token },

    #[error("Accolade `{{` of puntkomma `;` verwacht, maar kreeg: {token}")]
    ExpectedSemicolonOrCurlyBracketForFunction { token: Token, range: FileRange },

    #[error("Onjuiste velddeclaratie, velden moeten starten met `veld`")]
    ExpectedStructureMemberPrefixVeld { token: Token },

    #[error("Onjuiste structuurwerkwijze, deze hoort te starten met `werkwijze`")]
    ExpectedStructureMethodPrefixWerkwijze { token: Token },

    #[error("Werkwijzenaam verwacht na `werkwijze`, maar kreeg: {token}")]
    FunctionStatementExpectedName { token: Token },

    #[error("{ident_purpose} verwacht na `{previous}`, maar kreeg: {token}")]
    ExpectedIdentifier { token: Token, previous: BabString, ident_purpose: &'static str },

    #[error("Iteratornaam verwacht na `volg`, maar kreeg: {token}")]
    ForStatementExpectedIteratorName { token: Token },

    #[error("Sleutelwoord 'in' verwacht na `volg {}`, maar kreeg: {token}", iterator_name.value())]
    ForStatementExpectedInKeyword { token: Token, iterator_name: Ranged<BabString> },

    #[error("Parameternaam verwacht, maar kreeg: {token}")]
    ParameterExpectedName { token: Token },

    #[error("Komma `,` of gesloten rond haakje `)` verwacht binnen parameterlijst, maar kreeg: {token}")]
    ParameterExpectedComma { token: Token },

    #[error("Methode of structuurlid verwacht na punt `.`, maar kreeg: {token}")]
    PostfixMemberOrReferenceExpectedIdentifier { token: Token, period: Token },

    #[error("Sleutelwoord `reeks` verwacht, maar kreeg: {token}")]
    RangeExpectedKeyword { token: Token },

    #[error("Resterende token na expressie binnen sjabloonslinger: {token}")]
    ResidualTokensInTemplateString { token: Token, range: FileRange },

    #[error("Parametertype verwacht, maar kreeg: {token}")]
    TypeExpectedSpecifierName { token: Token },

    #[error("Onbekend start van een expressie: {token}")]
    UnknownStartOfExpression { token: Token },

    #[error("Onbekend start van een structuurlid: {token}")]
    UnexpectedTokenAtStartOfStructureMember { token: Token },

    #[error("Verwijzing naar structuurveld verwacht, maar kreeg: {token}")]
    UnexpectedTokenAtStartOfStructureInstantiation { token: Token },

    #[error("Onverwachte token binnen structuur: `{token}`")]
    UnexpectedTokenInsideStructureInstantiation { token: Token },

    #[error("Functie moet een definitie bevatten")]
    FunctionMustHaveDefinition { semicolon: Token, range: FileRange },
}

impl ParseDiagnostic {
    pub fn token(&self) -> &Token {
        match self {
            Self::AttributeArgumentExpectedComma { token } => token,
            Self::StatementInvalidStart { token } => token,
            Self::ExpectedLeftCurlyBracket { token, .. } => token,
            Self::ExpectedLeftParen { token, .. } => token,
            Self::ExpectedRightParen { token, .. } => token,
            Self::ExpectedColon { token, .. } => token,
            Self::ExpectedComma { token, .. } => token,
            Self::ExpectedCommaAfterStructureMember { token, .. } => token,
            Self::ExpectedStructureMethodPrefixWerkwijze { token } => token,
            Self::ExpectedSemicolonAfterStatement { token, .. } => token,
            Self::ExpectedSemicolonOrCurlyBracketForFunction { token, .. } => token,
            Self::ExpectedNameAfterNieuw { token } => token,
            Self::ExpectedNameOfField { token } => token,
            Self::ExpectedNameOfStructuur { token } => token,
            Self::ExpectedNameOfVariable { token } => token,
            Self::ExpectedIdentifier { token, .. } => token,
            Self::ExpectedEqualsInsideVariable { token } => token,
            Self::ExpectedStructureMemberPrefixVeld { token } => token,
            Self::FunctionStatementExpectedName { token } => token,
            Self::ForStatementExpectedIteratorName { token } => token,
            Self::ForStatementExpectedInKeyword { token, .. } => token,
            Self::FunctionMustHaveDefinition { semicolon, .. } => semicolon,
            Self::ParameterExpectedName { token } => token,
            Self::ParameterExpectedComma { token } => token,
            Self::PostfixMemberOrReferenceExpectedIdentifier { token, .. } => token,
            Self::RangeExpectedKeyword { token } => token,
            Self::ResidualTokensInTemplateString { token, .. } => token,
            Self::TypeExpectedSpecifierName { token } => token,
            Self::UnknownStartOfExpression { token } => token,
            Self::UnexpectedTokenAtStartOfStructureMember { token } => token,
            Self::UnexpectedTokenAtStartOfStructureInstantiation { token } => token,
            Self::UnexpectedTokenInsideStructureInstantiation { token } => token,
        }
    }

    pub fn range(&self) -> FileRange {
        match self {
            Self::ExpectedColon { range, .. } => *range,
            Self::ExpectedCommaAfterStructureMember { location, .. } => location.as_zero_range(),
            Self::ExpectedSemicolonOrCurlyBracketForFunction { range, .. } => *range,
            Self::FunctionMustHaveDefinition { range, .. } => *range,
            Self::PostfixMemberOrReferenceExpectedIdentifier { period, .. } => period.range(),
            Self::ResidualTokensInTemplateString { range, .. } => *range,
            _ => self.token().range(),
        }
    }

    #[must_use]
    pub fn name(&self) -> &str {
        self.as_ref()
    }
}

#[derive(Clone, Debug, thiserror::Error, AsRefStr)]
pub enum ParseError {
    #[error("Onverwacht einde van het bestand")]
    EndOfFile,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionParsingContext {
    Function,
    Method,
}

impl FunctionParsingContext {
    #[must_use]
    pub const fn require_body(&self) -> bool {
        matches!(self, Self::Method)
    }
}

#[cfg(test)]
mod tests {
    use crate::{Lexer, SourceCode};

    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(" schrijf(\"Hallo\") ")]
    #[case("schrijf(\"Hallo\")")]
    fn parse_function_call_expression(#[case] input: &'static str) {
        let source_code = SourceCode::new_test(BabString::new_static(input));
        let tokens: Vec<Token> = Lexer::new(&source_code).collect();
        let mut parser = Parser::new(source_code.path().to_path_buf(), &tokens);
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
