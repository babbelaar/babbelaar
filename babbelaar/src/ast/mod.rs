// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

mod attribute;
mod expression;
mod parser;
mod statement;
mod structure;
mod tree;
mod type_;

pub use self::{
    attribute::{
        Attribute,
        AttributeArgument,
        AttributeList,
    },
    expression::{
        BiExpression,
        BiOperator,
        Comparison,
        Expression,
        FieldInstantiation,
        FunctionCallExpression,
        MathOperator,
        MethodCallExpression,
        PostfixExpression,
        PostfixExpressionKind,
        PrimaryExpression,
        RangeExpression,
        StructureInstantiationExpression,
        TemplateStringExpressionPart,
        UnaryExpression,
        UnaryExpressionKind,
    },
    parser::{
        Parser,
        ParseDiagnostic,
        ParseError,
    },
    statement::{
        AssignKind,
        AssignStatement,
        ExtensionStatement,
        ForIterableKind,
        ForStatement,
        FunctionStatement,
        IfStatement,
        ReturnStatement,
        Statement,
        StatementKind,
        VariableStatement
    },
    structure::{
        Field,
        InterfaceStatement,
        Method,
        Structure,
    },
    tree::ParseTree,
    type_::{
        InterfaceSpecifier,
        Parameter,
        Type,
        TypeQualifier,
        TypeSpecifier,
    },
};
