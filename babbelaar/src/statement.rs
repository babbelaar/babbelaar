// Copyright (C) 2023 - 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{AttributeList, BabString, Expression, FileRange, InterfaceStatement, Method, Parameter, RangeExpression, Ranged, Structure, Type, TypeSpecifier};

#[derive(Debug, Clone)]
pub struct Statement {
    pub range: FileRange,
    pub attributes: AttributeList,
    pub kind: StatementKind,
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    Assignment(Ranged<AssignStatement>),
    Expression(Ranged<Expression>),
    Extension(ExtensionStatement),
    Function(FunctionStatement),
    For(ForStatement),
    If(IfStatement),
    Interface(InterfaceStatement),
    Return(ReturnStatement),
    Structure(Structure),
    Variable(VariableStatement),
}

impl StatementKind {
    #[must_use]
    pub const fn is_expression(&self) -> bool {
        matches!(self, Self::Expression(..))
    }
}

/// In babbelaar, we have Assignment `=` statements instead of expressions,
/// since this avoid bugs (IMO).
#[derive(Clone, Debug)]
pub struct AssignStatement {
    pub range: FileRange,
    pub equals_sign: FileRange,
    pub destination: Ranged<Expression>,
    pub source: Ranged<Expression>,
}

#[derive(Debug, Clone)]
pub struct ExtensionStatement {
    pub type_specifier: Ranged<TypeSpecifier>,
    pub methods: Vec<Method>,
    pub right_curly_bracket: FileRange,
}

#[derive(Clone, Debug)]
pub struct ForStatement {
    pub file_range: FileRange,
    pub keyword: FileRange,
    pub iterator_name: Ranged<BabString>,
    pub iterable: Ranged<ForIterableKind>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum ForIterableKind {
    Range(RangeExpression),
    Expression(Box<Ranged<Expression>>),
}

#[derive(Clone, Debug)]
pub struct FunctionStatement {
    pub range: FileRange,
    pub name: Ranged<BabString>,
    pub parameters: Vec<Parameter>,
    pub body: Option<Vec<Statement>>,
    pub parameters_right_paren_range: FileRange,
    pub return_type: Option<Ranged<Type>>,
}

#[derive(Clone, Debug)]
pub struct IfStatement {
    pub range: FileRange,
    pub condition: Ranged<Expression>,
    pub body: Vec<Statement>,
}

#[derive(Clone, Debug)]
pub struct ReturnStatement {
    pub keyword_range: FileRange,
    pub expression: Option<Ranged<Expression>>,
}

#[derive(Clone, Debug)]
pub struct VariableStatement {
    pub range: FileRange,
    pub name: Ranged<BabString>,
    pub expression: Ranged<Expression>,
}
