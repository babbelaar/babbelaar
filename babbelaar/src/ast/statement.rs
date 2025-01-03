// Copyright (C) 2023 - 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{AttributeList, BabString, Expression, FileRange, InterfaceSpecifier, InterfaceStatement, Method, Parameter, RangeExpression, Ranged, Structure, Type, TypeSpecifier};

use super::MathOperator;

#[derive(Debug, Clone)]
pub struct Statement {
    pub range: FileRange,
    pub attributes: AttributeList,
    pub kind: StatementKind,
}

impl Statement {
    #[must_use]
    pub fn is_freestanding(&self) -> bool {
        self.kind.is_freestanding()
    }
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    Assignment(Ranged<AssignStatement>),
    Break,
    Continue,
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

    #[must_use]
    pub fn is_freestanding(&self) -> bool {
        match self {
            Self::Assignment(..) => false,
            Self::Break => false,
            Self::Continue => false,
            Self::Expression(..) => false,
            Self::For(..) => false,
            Self::If(..) => false,
            Self::Return(..) => false,
            Self::Variable(..) => false,

            Self::Extension(..) => true,
            Self::Function(..) => true,
            Self::Interface(..) => true,
            Self::Structure(..) => true,
        }
    }

    #[must_use]
    pub fn is_function(&self) -> bool {
        match self {
            Self::Function(..) => true,
            _ => false,
        }
    }
}

/// In babbelaar, we have Assignment `=` statements instead of expressions,
/// since this avoid bugs (IMO).
#[derive(Clone, Debug)]
pub struct AssignStatement {
    pub range: FileRange,
    pub kind: Ranged<AssignKind>,
    pub destination: Ranged<Expression>,
    pub source: Ranged<Expression>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AssignKind {
    Regular,
    Math(MathOperator),
}

#[derive(Debug, Clone)]
pub struct ExtensionStatement {
    pub generic_types: Vec<Ranged<BabString>>,
    pub interface_specifier: Option<Ranged<InterfaceSpecifier>>,
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
    pub typ: Option<Ranged<Type>>,
}
