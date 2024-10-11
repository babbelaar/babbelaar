// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

//! Format the AST back to a String. Since [`std::fmt::Formatter`] doesn't have
//! a way to signal indentations (as I know of), we have a custom struct.

use std::fmt::Write;

use babbelaar::{AssignStatement, BiExpression, BuiltinType, Expression, Field, ForIterableKind, ForStatement, FunctionCallExpression, FunctionStatement, IfStatement, Keyword, MethodCallExpression, OptionExt, Parameter, PostfixExpression, PostfixExpressionKind, PrimaryExpression, ReturnStatement, Statement, StatementKind, Structure, StructureInstantiationExpression, TemplateStringExpressionPart, Type, TypeSpecifier, VariableStatement};

pub struct Formatter {
    buffer: String,
    indents: Vec<LastSiblingKind>,
    must_indent: bool,
}

impl Formatter {
    #[must_use]
    pub fn new() -> Self {
        Self {
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

    fn with_body_statements(&mut self, body: &[Statement]) {
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

impl Write for Formatter {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.write_str(s);
        Ok(())
    }
}

pub trait Format {
    fn format(&self, f: &mut Formatter);

    #[must_use]
    fn format_to_string(&self) -> String {
        let mut formatter = Formatter::new();
        self.format(&mut formatter);
        formatter.buffer
    }
}

impl Format for BuiltinType {
    fn format(&self, f: &mut Formatter) {
        f.write_str(&self.name());
    }
}

impl Format for Statement {
    fn format(&self, f: &mut Formatter) {
        self.kind.format(f)
    }
}

impl Format for StatementKind {
    fn format(&self, f: &mut Formatter) {
        match f.indents.last().copied().unwrap_or_default() {
            LastSiblingKind::None => (),
            LastSiblingKind::OtherStatement => f.new_line(),
            LastSiblingKind::Expression => if !self.is_expression() {
                f.new_line();
            }
        }

        match self {
            Self::Assignment(statement) => statement.format(f),
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

impl Format for Expression {
    fn format(&self, f: &mut Formatter) {
        match self {
            Self::BiExpression(bi) => bi.format(f),
            Self::Postfix(postfix) => postfix.format(f),
            Self::Primary(primary) => primary.format(f),
        }
    }
}

impl Format for BiExpression {
    fn format(&self, f: &mut Formatter) {
        self.lhs.format(f);
        f.write_char(' ');
        f.write_str(self.operator.as_str());
        f.write_char(' ');
        self.rhs.format(f);
    }
}

impl Format for PostfixExpression {
    fn format(&self, f: &mut Formatter) {
        self.lhs.format(f);
        self.kind.format(f);
    }
}

impl Format for PostfixExpressionKind {
    fn format(&self, f: &mut Formatter) {
        match self {
            Self::Call(call) => call.format(f),
            Self::Member(member) => f.write_str(&member),
            Self::MethodCall(method) => method.format(f),
            Self::Subscript(subscript) => {
                f.write_char('[');
                subscript.format(f);
                f.write_char(']');
            }
        }
    }
}

impl Format for FunctionCallExpression {
    fn format(&self, f: &mut Formatter) {
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

impl Format for MethodCallExpression {
    fn format(&self, f: &mut Formatter) {
        f.write_char('.');
        f.write_str(&self.method_name);
        self.call.format(f);
    }
}

impl Format for AssignStatement {
    fn format(&self, f: &mut Formatter) {
        self.destination.format(f);
        f.write_str(" = ");
        self.source.format(f);
        f.write_str(";\n");
    }
}

impl Format for FunctionStatement {
    fn format(&self, f: &mut Formatter) {
        f.write_str("werkwijze ");
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

impl Format for ForStatement {
    fn format(&self, f: &mut Formatter) {
        f.write_str("volg ");
        f.write_str(self.iterator_name.value());
        f.write_str(" in ");

        match self.iterable.value() {
            ForIterableKind::Expression(expression) => expression.value().format(f),
            ForIterableKind::Range(range) => {
                f.write_str("reeks(");
                range.start.value().format(f);
                f.write_str(", ");
                range.end.value().format(f);
                f.write_char(')');
            }
        }

        f.with_body_statements(&self.body);
    }
}

impl Format for IfStatement {
    fn format(&self, f: &mut Formatter) {
        f.write_str("als ");
        self.condition.format(f);

        f.with_curly_block(|f| {
            for statement in &self.body {
                statement.format(f);
            }
        });
    }
}

impl Format for ReturnStatement {
    fn format(&self, f: &mut Formatter) {
        f.write_str("bekeer");

        if let Some(expr) = &self.expression {
            f.write_char(' ');
            expr.format(f);
        }

        f.write_str(";\n");
    }
}

impl Format for Structure {
    fn format(&self, f: &mut Formatter) {
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

impl Format for Field {
    fn format(&self, f: &mut Formatter) {
        f.write_str("veld ");
        f.write_str(self.name.value());
        f.write_str(": ");
        self.ty.format(f);
    }
}

impl Format for VariableStatement {
    fn format(&self, f: &mut Formatter) {
        f.write_str("stel ");
        f.write_str(&self.name);
        f.write_str(" = ");
        self.expression.format(f);
        f.write_str(";\n");
    }
}

impl Format for Parameter {
    fn format(&self, f: &mut Formatter) {
        f.write_str(&self.name);
        f.write_str(": ");
        self.ty.format(f);
    }
}

impl Format for Type {
    fn format(&self, f: &mut Formatter) {
        self.specifier.format(f);
    }
}

impl Format for TypeSpecifier {
    fn format(&self, f: &mut Formatter) {
        match self {
            Self::BuiltIn(builtin) => builtin.format(f),
            Self::Custom { .. } => todo!(),
        }
    }
}

impl Format for PrimaryExpression {
    fn format(&self, f: &mut Formatter) {
        match self {
            Self::Boolean(true) => f.write_str("waar"),
            Self::Boolean(false) => f.write_str("onwaar"),
            Self::IntegerLiteral(integer) => {
                // TODO use source code for original formatting, e.g. with number separators etc.
                _ = f.write_fmt(format_args!("{integer}"));
            }
            Self::ReferenceThis => f.write_str(Keyword::Dit.as_ref()),
            Self::Reference(s) => f.write_str(s.value()),
            Self::CharacterLiteral(c) => {
                f.write_char('"');
                f.write_char(*c);
                f.write_char('"');
            }
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
            Self::SizedArrayInitializer{ typ, size } => {
                f.write_str("nieuw ");
                typ.format(f);
                f.write_char('[');
                size.format(f);
                f.write_char(']');
            }
        }
    }
}

impl Format for StructureInstantiationExpression {
    fn format(&self, f: &mut Formatter) {
        f.write_str("nieuw ");
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

impl Format for TemplateStringExpressionPart {
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
