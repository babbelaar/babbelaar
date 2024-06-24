// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use babbelaar::Value;
use rstest::rstest;
use tests::interpret_expression;

#[rstest]
#[case("\"hallo\".begintMet(\"ha\")", Value::Bool(true))]
#[case("\"hallo\".begintMet(\"allo\")", Value::Bool(false))]
fn characteristics(#[case] input: &str, #[case] expected: Value) {
    let actual = interpret_expression(input);
    assert_eq!(actual, expected);
}
