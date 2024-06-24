// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use babbelaar::Value;
use rstest::rstest;
use tests::interpret_expression;

#[rstest]
#[case("10", Value::Integer(10))]
#[case("5 + 2", Value::Integer(7))]
#[case("4 * 9", Value::Integer(36))]
#[case("52 % 30", Value::Integer(22))]
fn binary_operations(#[case] input: &str, #[case] expected: Value) {
    let actual = interpret_expression(input);
    assert_eq!(actual, expected);
}

#[rstest]
#[case("10 * 4 + 5", Value::Integer(45))]
#[case("10 + 4 + 5", Value::Integer(19))]
#[case("10 + 4 * 5", Value::Integer(30))]
fn pemdas(#[case] input: &str, #[case] expected: Value) {
    let actual = interpret_expression(input);
    assert_eq!(actual, expected);
}
