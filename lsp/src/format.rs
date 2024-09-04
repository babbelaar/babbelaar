// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

//! Format the AST back to a String. Since [`std::fmt::Formatter`] doesn't have
//! a way to signal indentations (as I know of), we have a custom struct.

use std::fmt::Write;

use babbelaar::{BiExpression, BuiltinType, Expression, Field, ForStatement, FunctionCallExpression, FunctionStatement, IfStatement, MethodCallExpression, OptionExt, Parameter, PostfixExpression, PostfixExpressionKind, PrimaryExpression, ReturnStatement, Statement, StatementKind, Structure, StructureInstantiationExpression, TemplateStringExpressionPart, Type, TypeSpecifier, VariableStatement};

pub struct Formatter<'source> {
    #[allow(unused)]
    source: &'source str,
    buffer: String,
    indents: Vec<LastSiblingKind>,
    must_indent: bool,
}

impl<'source_code> Formatter<'source_code> {
    #[must_use]
    pub fn new(source: &'source_code str) -> Self {
        Self {
            source,
            buffer: String::new(),
            indents: vec![LastSiblingKind::None],
            must_indent: false,
        }
    }

    pub fn write_char(&mut self, c: char) {
        self.ensure_indented();
        self.buffer.push(c);
    }

    pub fn write_str(&mut self, s: &str) {
        for (line_idx, line) in s.lines().enumerate() {
            if line_idx != 0 {
                self.new_line();
            }

            self.ensure_indented();
            self.buffer += line;
        }
    }

    fn new_line(&mut self) {
        self.buffer += "\n";
        self.must_indent = true;
    }

    fn ensure_indented(&mut self) {
        if self.must_indent {
            self.indent();
        }
    }

    fn indent(&mut self) {
        if self.indents.len() == 1 {
            return;
        }

        self.buffer += &"    ".repeat(self.indents.len() - 1);
        self.must_indent = false;
    }

    fn with_curly_block(&mut self, f: impl FnOnce(&mut Self)) {
        self.write_str(" {");
        self.new_line();

        self.with_indent(f);
        self.write_char('}');
        self.new_line();
    }

    fn with_body_statements(&mut self, body: &[Statement<'_>]) {
        self.with_curly_block(|f| {
            for statement in body {
                statement.format(f);
            }
        })
    }

    fn with_indent(&mut self, f: impl FnOnce(&mut Self)) {
        self.indents.push(LastSiblingKind::None);
        f(self);
        self.indents.pop();
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum LastSiblingKind {
    #[default]
    None,
    Expression,
    OtherStatement,
}

impl<'source_code> Write for Formatter<'source_code> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.write_str(s);
        Ok(())
    }
}

pub trait Format {
    fn format(&self, f: &mut Formatter);

    #[must_use]
    fn format_to_string(&self, source: &str) -> String {
        let mut formatter = Formatter::new(source);
        self.format(&mut formatter);
        formatter.buffer
    }
}

impl Format for BuiltinType {
    fn format(&self, f: &mut Formatter<'_>) {
        f.write_str(self.name());
    }
}

impl<'source_code> Format for Statement<'source_code> {
    fn format(&self, f: &mut Formatter<'_>) {
        self.kind.format(f)
    }
}

impl<'source_code> Format for StatementKind<'source_code> {
    fn format(&self, f: &mut Formatter<'_>) {
        match f.indents.last().copied().unwrap_or_default() {
            LastSiblingKind::None => (),
            LastSiblingKind::OtherStatement => f.new_line(),
            LastSiblingKind::Expression => if !self.is_expression() {
                f.new_line();
            }
        }

        match self {
            Self::Expression(expr) => {
                expr.format(f);
                f.write_char(';');
                f.new_line();

                if let Some(last) = f.indents.last_mut() {
                    *last = LastSiblingKind::Expression;
                } else {
                    debug_assert!(false, "invalid state");
                }
                return;
            }
            Self::Function(statement) => statement.format(f),
            Self::For(statement) => statement.format(f),
            Self::If(statement) => statement.format(f),
            Self::Return(statement) => statement.format(f),
            Self::Structure(statement) => statement.format(f),
            Self::Variable(statement) => statement.format(f),
        }

        if let Some(last) = f.indents.last_mut() {
            *last = LastSiblingKind::OtherStatement;
        } else {
            debug_assert!(false, "invalid state");
        }
    }
}

impl<'source_code> Format for Expression<'source_code> {
    fn format(&self, f: &mut Formatter<'_>) {
        match self {
            Self::BiExpression(bi) => bi.format(f),
            Self::Postfix(postfix) => postfix.format(f),
            Self::Primary(primary) => primary.format(f),
        }
    }
}

impl<'source_code> Format for BiExpression<'source_code> {
    fn format(&self, f: &mut Formatter<'_>) {
        self.lhs.format(f);
        f.write_char(' ');
        f.write_str(self.operator.as_str());
        f.write_char(' ');
        self.rhs.format(f);
    }
}

impl<'source_code> Format for PostfixExpression<'source_code> {
    fn format(&self, f: &mut Formatter<'_>) {
        self.lhs.format(f);
        self.kind.format(f);
    }
}

impl<'source_code> Format for PostfixExpressionKind<'source_code> {
    fn format(&self, f: &mut Formatter<'_>) {
        match self {
            Self::Call(call) => call.format(f),
            Self::Member(member) => f.write_str(&member),
            Self::MethodCall(method) => method.format(f),
        }
    }
}

impl<'source_code> Format for FunctionCallExpression<'source_code> {
    fn format(&self, f: &mut Formatter<'_>) {
        f.write_char('(');

        for (idx, arg) in self.arguments.iter().enumerate() {
            if idx != 0 {
                f.write_str(", ");
            }

            arg.format(f);
        }

        f.write_char(')');
    }
}

impl<'source_code> Format for MethodCallExpression<'source_code> {
    fn format(&self, f: &mut Formatter<'_>) {
        f.write_char('.');
        f.write_str(&self.method_name);
        self.call.format(f);
    }
}

impl<'source_code> Format for FunctionStatement<'source_code> {
    fn format(&self, f: &mut Formatter<'_>) {
        f.write_str("functie ");
        f.write_str(self.name.value());
        f.write_char('(');
        for (idx, param) in self.parameters.iter().enumerate() {
            if idx != 0 {
                f.write_str(", ");
            }

            param.format(f);
        }
        f.write_char(')');

        f.with_body_statements(self.body.as_inner_slice());
    }
}

impl<'source_code> Format for ForStatement<'source_code> {
    fn format(&self, f: &mut Formatter<'_>) {
        f.write_str("volg ");
        f.write_str(self.iterator_name.value());
        f.write_str(" in reeks(");
        self.range.start.value().format(f);
        f.write_str(", ");
        self.range.end.value().format(f);
        f.write_char(')');

        f.with_body_statements(&self.body);
    }
}

impl<'source_code> Format for IfStatement<'source_code> {
    fn format(&self, f: &mut Formatter<'_>) {
        f.write_str("als ");
        self.condition.format(f);

        f.with_curly_block(|f| {
            for statement in &self.body {
                statement.format(f);
            }
        });
    }
}

impl<'source_code> Format for ReturnStatement<'source_code> {
    fn format(&self, f: &mut Formatter<'_>) {
        f.write_str("bekeer");

        if let Some(expr) = &self.expression {
            f.write_char(' ');
            expr.format(f);
        }

        f.write_str(";\n");
    }
}

impl<'source_code> Format for Structure<'source_code> {
    fn format(&self, f: &mut Formatter<'_>) {
        f.write_str("structuur ");
        f.write_str(self.name.value());
        f.with_curly_block(|f| {
            for field in &self.fields {
                field.format(f);
                f.write_char(',');
                f.new_line();
            }
        });

    }
}

impl<'source_code> Format for Field<'source_code> {
    fn format(&self, f: &mut Formatter<'_>) {
        f.write_str("veld ");
        f.write_str(self.name.value());
        f.write_str(": ");
        self.ty.format(f);
    }
}

impl<'source_code> Format for VariableStatement<'source_code> {
    fn format(&self, f: &mut Formatter<'_>) {
        f.write_str("stel ");
        f.write_str(&self.name);
        f.write_str(" = ");
        self.expression.format(f);
        f.write_str(";\n");
    }
}

impl<'source_code> Format for Parameter<'source_code> {
    fn format(&self, f: &mut Formatter<'_>) {
        f.write_str(&self.name);
        f.write_str(": ");
        self.ty.format(f);
    }
}

impl<'source_code> Format for Type<'source_code> {
    fn format(&self, f: &mut Formatter<'_>) {
        self.specifier.format(f);
    }
}

impl<'source_code> Format for TypeSpecifier<'source_code> {
    fn format(&self, f: &mut Formatter<'_>) {
        match self {
            Self::BuiltIn(builtin) => builtin.format(f),
            Self::Custom { .. } => todo!(),
        }
    }
}

impl<'source_code> Format for PrimaryExpression<'source_code> {
    fn format(&self, f: &mut Formatter<'_>) {
        match self {
            Self::Boolean(true) => f.write_str("waar"),
            Self::Boolean(false) => f.write_str("onwaar"),
            Self::IntegerLiteral(integer) => {
                // TODO use source code for original formatting, e.g. with number separators etc.
                _ = f.write_fmt(format_args!("{integer}"));
            }
            Self::Reference(s) => f.write_str(s.value()),
            Self::StringLiteral(s) => {
                f.write_char('"');
                f.write_str(s);
                f.write_char('"');
            }
            Self::TemplateString { parts } => {
                f.write_str("€\"");
                for part in parts {
                    part.format(f);
                }
                f.write_char('"');
            }
            Self::Parenthesized(expr) => {
                f.write_char('(');
                expr.format(f);
                f.write_char(')');
            }
            Self::StructureInstantiation(structure) => structure.format(f),
        }
    }
}

impl<'source_code> Format for StructureInstantiationExpression<'source_code> {
    fn format(&self, f: &mut Formatter) {
        f.write_str("nieuwe ");
        f.write_str(&self.name);
        f.write_str(" ");
        f.with_curly_block(|f| {
            for field in &self.fields {
                f.write_str(&field.name);
                f.write_str(": ");
                field.value.format(f);
                f.write_char(',');
                f.new_line();
            }
        })
    }
}

impl<'source_code> Format for TemplateStringExpressionPart<'source_code> {
    fn format(&self, f: &mut Formatter) {
        match self {
            Self::Expression(expr) => {
                f.write_char('{');
                expr.format(f);
                f.write_char('}');
            }

            Self::String(s) => f.write_str(s),
        }
    }
}
