// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use rstest::rstest;
use tests::interpret_and_return_stdout;

#[rstest]
#[case(
    r#"
        schrijf("Hallo!");
    "#,
    &[
        "Hallo!",
    ],
)]
#[case(
    r#"
        stel a = 5;
        schrijf(€"{a}");
    "#,
    &[
        "5",
    ],
)]
#[case(
    r#"
        werkwijze hallo(a: g32) {
            schrijf(€"{a * 5}");
        }

        volg i in reeks(0, 10) {
            hallo(i);
        }
    "#,
    &[
        "0",
        "5",
        "10",
        "15",
        "20",
        "25",
        "30",
        "35",
        "40",
        "45"
    ],
)]
fn interpret_and_return_stdout_tests(#[case] input: &str, #[case] expected: &[&str]) {
    let expected: Vec<String> = expected.into_iter().map(|x| x.to_string()).collect();
    assert_eq!(interpret_and_return_stdout(input), expected);
}
