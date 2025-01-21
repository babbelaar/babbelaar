// Copyright (C) 2024 - 2025 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{error::Error, io::Read, path::{Path, PathBuf}, process::{Command, Stdio}};

use babbelaar::{parse_string_to_tree, ArchiveKind};
use babbelaar_compiler::{Pipeline, Platform, Signal};
use log::info;
use rstest::rstest;
use temp_dir::TempDir;

#[test]
fn simple_return_0() {
    let result = create_and_run_single_object_executable("
        werkwijze hoofd() -> g32 {
            bekeer 0;
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(0));
}

#[test]
fn simple_reassign_1_to_2() {
    let result = create_and_run_single_object_executable("
        werkwijze hoofd() -> g32 {
            stel a = 1;
            a = 2;
            bekeer a;
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(2));
}

#[test]
fn simple_return_123() {
    let result = create_and_run_single_object_executable("
        werkwijze hoofd() -> g32 {
            bekeer 123;
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(123));
}

#[test]
fn simple_return_with_discard_result_of_subroutine() {
    let result = create_and_run_single_object_executable("
        werkwijze krijgLeukGetal() -> g32 { bekeer 12; }

        werkwijze hoofd() -> g32 {
            stel _a = krijgLeukGetal();
            bekeer 123;
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(123));
}

#[test]
fn simple_return_with_discard_result_of_subroutine2() {
    let result = create_and_run_single_object_executable("
        werkwijze krijgLeukGetal() -> g32 { bekeer 12; }

        werkwijze hoofd() -> g32 {
            stel a = 8;
            stel _a = krijgLeukGetal();
            bekeer a;
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(8));
}

#[test]
fn simple_return_1_plus_2() {
    let result = create_and_run_single_object_executable("
        werkwijze een() -> g32 { bekeer 1; }

        werkwijze hoofd() -> g32 {
            bekeer een() + 2;
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(3));
}

#[test]
fn return_1_plus_subroutine_val() {
    let result = create_and_run_single_object_executable("
        werkwijze een() -> g32 { bekeer 1; }

        werkwijze hoofd() -> g32 {
            stel a = 2;
            a += 0;
            bekeer een() + a;
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(3));
}

#[test]
fn simple_return_8_minus_6() {
    let result = create_and_run_single_object_executable("
        werkwijze acht() -> g32 { bekeer 8; }

        werkwijze hoofd() -> g32 {
            bekeer acht() - 6;
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(2));
}

#[test]
fn simple_call_other_than_returns_100() {
    let result = create_and_run_single_object_executable("
        werkwijze a() -> g32 {
            bekeer b();
        }

        werkwijze hoofd() -> g32 {
            bekeer a();
        }

        werkwijze b() -> g32 {
            bekeer 100;
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(100));
}

#[test]
fn return_1_plus_1_with_subroutine_in_between() {
    let result = create_and_run_single_object_executable("
        werkwijze subroutine() {}

        werkwijze hoofd() -> g32 {
            stel een = 1;
            subroutine();
            bekeer 1 + een;
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(2));
}

#[test]
fn use_variables_after_subroutine() {
    let result = create_and_run_single_object_executable("
        werkwijze krijgB() -> g32 { bekeer 2; }
        werkwijze krijgC() -> g32 { bekeer 3; }
        werkwijze krijgE() -> g32 { bekeer 5; }

        werkwijze hoofd() -> g32 {
            stel a = 1;
            stel b = krijgB();
            stel c = krijgC();
            stel d = 4;
            stel e = krijgE();
            bekeer a + c + b + d + e + 6;
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(21));
}

#[test]
fn return_8_if_5_is_equal_to_5() {
    let result = create_and_run_single_object_executable("
        werkwijze hoofd() -> g32 {
            als 5 == 5 {
                stel c = 9;
                bekeer 8;
            }

            stel a = 4;
            stel b = 4;
            bekeer a;
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(8));
}

#[test]
fn subroutine_for_minus() {
    let result = create_and_run_single_object_executable("
        werkwijze minus(a: g32, b: g32) -> g32 {
            bekeer a - b - 1;
        }

        werkwijze hoofd() -> g32 {
            stel a = 5;
            stel b = 4;
            bekeer minus(a, b);
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(0));
}

#[test]
fn negate_immediate() {
    let result = create_and_run_single_object_executable("
    werkwijze hoofd() -> g32 {
        bekeer -8;
    }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(-8));
}

#[test]
fn negate_with_function_call() {
    let result = create_and_run_single_object_executable("
    werkwijze keer_negatief(i: g32) -> g32 {
        bekeer -i;
    }

    werkwijze hoofd() -> g32 {
        bekeer keer_negatief(94);
    }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(-94));
}

#[test]
fn method_call() {
    let result = create_and_run_single_object_executable("
        structuur MijnGeavanceerdeStructuur {
            werkwijze krijgGetal() -> g32 {
                bekeer 3;
            }
        }

        werkwijze hoofd() -> g32 {
            stel a = nieuw MijnGeavanceerdeStructuur {};
            bekeer a.krijgGetal();
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(3));
}

#[test]
fn method_call_with_this() { //
    let result = create_and_run_single_object_executable("
    structuur MijnStructuurMetGetal {
        veld getal: g32,

        werkwijze gebruikGetal() -> g32 {
            bekeer dit.getal + 2;
        }
    }

    werkwijze hoofd() -> g32 {
        stel a = nieuw MijnStructuurMetGetal {
            getal: 5,
        };
        bekeer a.gebruikGetal();
    }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(7));
}

#[test]
fn method_call_with_this_and_two_fields() {
    let result = create_and_run_single_object_executable("
    structuur MijnStructuurMetTweeGetallen {
        veld a: g32,
        veld b: g32,

        werkwijze gebruikGetal() -> g32 {
            bekeer dit.a + dit.b + 2;
        }
    }

    werkwijze hoofd() -> g32 {
        stel paar = nieuw MijnStructuurMetTweeGetallen {
            a: 0,
            b: 9,
        };
        bekeer paar.gebruikGetal();
    }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(11));
}

#[test]
fn function_with_unused_string_literal_variable() {
    let value = create_and_run_single_object_executable("
    werkwijze hoofd() -> g32 {
        stel a = \"Hallo\";
        bekeer 1;
    }
    ");

    assert_eq!(value.signal, None);
    assert_eq!(value.exit_code, Some(1));
}

#[test]
fn function_returns_length_of_string_literal() {
    let value = create_and_run_single_object_executable("
    werkwijze hoofd() -> g32 {
        bekeer \"Hallo\".lengte();
    }
    ");

    assert_eq!(value.signal, None);
    assert_eq!(value.exit_code, Some(5));
}

#[test]
fn two_strings_one_program() {
    let value = create_and_run_single_object_executable("
    werkwijze hoofd() -> g32 {
        bekeer \"Hallo\".lengte() + \"Doei\".lengte();
    }
    ");

    assert_eq!(value.signal, None);
    assert_eq!(value.exit_code, Some(9));
}

#[test]
fn return_comparison_value() {
    let result = create_and_run_single_object_executable("
        werkwijze bekeerAls5(a: g32) -> g32 {
            bekeer a == 5;
        }

        werkwijze hoofd() -> g32 {
            bekeer bekeerAls5(5234) + bekeerAls5(5) + bekeerAls5(550);
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(1));
}

#[test]
fn pass_single_comparison_value() {
    let result = create_and_run_single_object_executable("
        werkwijze bekeerResultaat(a: g32) -> g32 {
            bekeer a;
        }

        werkwijze bekeerAls5(a: g32) -> g32 {
            bekeer bekeerResultaat(a == 5);
        }

        werkwijze hoofd() -> g32 {
            bekeer bekeerAls5(5);
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(1));
}

#[test]
fn pass_comparison_value() {
    let result = create_and_run_single_object_executable("
        werkwijze bekeerResultaat(_negeer: g32, a: g32) -> g32 {
            bekeer a;
        }

        werkwijze bekeerAls5(a: g32) -> g32 {
            bekeer bekeerResultaat(123, a == 5);
        }

        werkwijze hoofd() -> g32 {
            bekeer bekeerAls5(5234) + bekeerAls5(5) + bekeerAls5(550);
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(1));
}

#[test]
fn check_returned_negative_one() {
    let result = create_and_run_single_object_executable("
        werkwijze krijgSocket() -> g32 {
            bekeer -1;
        }

        werkwijze hoofd() -> g32 {
            als krijgSocket() == -1 {
                bekeer 2;
            }
            bekeer 3;
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(2));
}

#[test]
fn multiply_immediate() {
    let result = create_and_run_single_object_executable("
        werkwijze hoofd() -> g32 {
            bekeer 12 * 2;
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(24));
}

#[test]
fn multiply_unknown_lhs_and_rhs() {
    let result = create_and_run_single_object_executable("
        werkwijze keer(a: g32, b: g32) -> g32 {
            bekeer a * b;
        }

        werkwijze hoofd() -> g32 {
            bekeer keer(9, 3);
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(27));
}

#[test]
fn multiply_known_lhs() {
    let result = create_and_run_single_object_executable("
        werkwijze zevenKeerIets(a: g32) -> g32 {
            bekeer 7 * a;
        }

        werkwijze hoofd() -> g32 {
            bekeer zevenKeerIets(5);
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(35));
}

#[test]
fn multiply_known_rhs() {
    let result = create_and_run_single_object_executable("
        werkwijze ietsKeer5(a: g32) -> g32 {
            bekeer a * 5;
        }

        werkwijze hoofd() -> g32 {
            bekeer ietsKeer5(40);
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(-56));
}

#[test]
fn divide_immediate() {
    let result = create_and_run_single_object_executable("
        werkwijze hoofd() -> g32 {
            bekeer 12 / 2;
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(6));
}

#[test]
fn divide_unknown_lhs_and_rhs() {
    let result = create_and_run_single_object_executable("
        werkwijze deelDoor(a: g32, b: g32) -> g32 {
            bekeer a / b;
        }

        werkwijze hoofd() -> g32 {
            bekeer deelDoor(100, 4);
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(25));
}

#[test]
fn divide_known_lhs() {
    let result = create_and_run_single_object_executable("
        werkwijze deel100DoorIets(a: g32) -> g32 {
            bekeer 100 / a;
        }

        werkwijze hoofd() -> g32 {
            bekeer deel100DoorIets(25);
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(4));
}

#[test]
fn divide_known_rhs() {
    let result = create_and_run_single_object_executable("
        werkwijze deelDoor5(a: g32) -> g32 {
            bekeer a / 5;
        }

        werkwijze hoofd() -> g32 {
            bekeer deelDoor5(200);
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(40));
}

#[test]
fn modulo_immediate() {
    let result = create_and_run_single_object_executable("
        werkwijze hoofd() -> g32 {
            bekeer 13 % 2;
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(1));
}

#[test]
fn modulo_unknown_lhs_and_rhs() {
    let result = create_and_run_single_object_executable("
        werkwijze modulo(a: g32, b: g32) -> g32 {
            bekeer a % b;
        }

        werkwijze hoofd() -> g32 {
            bekeer modulo(100, 25);
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(0));
}

#[test]
fn modulo_known_lhs() {
    let result = create_and_run_single_object_executable("
        werkwijze module100MetIets(a: g32) -> g32 {
            bekeer 100 % a;
        }

        werkwijze hoofd() -> g32 {
            bekeer module100MetIets(6);
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(4), "{result:#?}");
}

/// TODO: investigate why this test is flaky
/// run solely it works, but running all tests lets this test specifically crash sometimes
#[test]
fn modulo_known_rhs() {
    let result = create_and_run_single_object_executable("
        werkwijze moduloIetsMet5(a: g32) -> g32 {
            bekeer a % 5;
        }

        werkwijze hoofd() -> g32 {
            bekeer moduloIetsMet5(200);
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(0));
}

#[test]
fn simple_array_add() {
    let result = create_and_run_single_object_executable("
        werkwijze hoofd() -> g32 {
            stel x = nieuw g32[2];
            x[0] = 5;
            x[1] = 2;

            bekeer x[0] + x[1];
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(7));
}

#[test]
fn simple_array_add_zero_initialized() {
    let result = create_and_run_single_object_executable("
        werkwijze hoofd() -> g32 {
            stel x = nieuw g32[2];
            bekeer x[0] + x[1];
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(0));
}

#[test]
fn array_zero_initialised() {
    let result = create_and_run_single_object_executable("
        werkwijze hoofd() -> g32 {
            stel x = nieuw g32[2];
            bekeer x[0] + x[1];
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(0));
}

#[test]
fn iterate_fixed_range_sum() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze hoofd() -> g32 {
            stel totaal = 0;
            volg i in reeks(0, 5) {
                totaal = totaal + 1;
            }

            bekeer totaal;
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(5));
}

#[test]
fn loop_string_length() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze hoofd() -> g32 {
            stel totaal = 0;
            volg i in reeks(0, "hallo".lengte()) {
                totaal = totaal + 1;
            }

            bekeer totaal;
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(5));
}

#[test]
fn loop_string_length_from_subroutine() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze krijgSlinger() -> Slinger {
            bekeer "hallo";
        }

        werkwijze hoofd() -> g32 {
            stel totaal = 0;
            volg i in reeks(0, krijgSlinger().lengte()) {
                totaal += 1;
            }

            bekeer totaal;
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(5));
}

#[test]
fn loop_string_chars() {
    let result = create_and_run_single_object_executable(r#"
        @uitheems(naam: "putchar")
        werkwijze schrijfLetter(t: teken);

        werkwijze hoofd() -> g32 {
            volg t in "Hallo" {
                schrijfLetter(t);
            }
            bekeer 0;
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(0));
}

#[test]
fn loop_g32_array() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze hoofd() -> g32 {
            stel s = nieuw g32[4];
            s[0] = 12;
            s[1] = 3;
            s[2] = 6;
            s[3] = 8;

            stel res = 0;

            volg i in reeks(0, 4) {
                res += s[i];
            }

            bekeer res;
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(29));
}

#[test]
fn left_shift_immediate_2() {
    let result = create_and_run_single_object_executable(r#"

        werkwijze hoofd() -> g32 {
            bekeer 3 << 2;
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(12));
}

#[test]
fn left_shift_immediate_lhs_0() {
    let result = create_and_run_single_object_executable(r#"

        werkwijze hoofd() -> g32 {
            bekeer 0 << 3;
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(0));
}

#[test]
fn left_shift_immediate_rhs_0() {
    let result = create_and_run_single_object_executable(r#"

        werkwijze hoofd() -> g32 {
            bekeer 3 << 0;
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(3));
}

#[test]
fn left_shift_unknown_lhs() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze schuifLinks(a: g32) -> g32 {
            bekeer a << 3;
        }

        werkwijze hoofd() -> g32 {
            bekeer schuifLinks(2);
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(16));
}

#[test]
fn left_shift_unknown_rhs() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze schuifLinks(a: g32) -> g32 {
            bekeer 8 << a;
        }

        werkwijze hoofd() -> g32 {
            bekeer schuifLinks(3);
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(64));
}

#[test]
fn left_shift_unknown_lhs_and_rhs() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze schuifLinks(a: g32, b: g32) -> g32 {
            bekeer a << b;
        }

        werkwijze hoofd() -> g32 {
            bekeer schuifLinks(5, 3);
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(40));
}

#[test]
fn right_shift_immediate_2() {
    let result = create_and_run_single_object_executable(r#"

        werkwijze hoofd() -> g32 {
            bekeer 12 >> 2;
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(3));
}

#[test]
fn right_shift_immediate_lhs_0() {
    let result = create_and_run_single_object_executable(r#"

        werkwijze hoofd() -> g32 {
            bekeer 0 >> 3;
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(0));
}

#[test]
fn right_shift_immediate_rhs_0() {
    let result = create_and_run_single_object_executable(r#"

        werkwijze hoofd() -> g32 {
            bekeer 3 >> 0;
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(3));
}

#[test]
fn right_shift_unknown_lhs() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze schuifLinks(a: g32) -> g32 {
            bekeer a >> 3;
        }

        werkwijze hoofd() -> g32 {
            bekeer schuifLinks(16);
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(2));
}

#[test]
fn right_shift_unknown_rhs() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze schuifLinks(a: g32) -> g32 {
            bekeer 64 >> a;
        }

        werkwijze hoofd() -> g32 {
            bekeer schuifLinks(3);
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(8));
}

#[test]
fn right_shift_unknown_lhs_and_rhs() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze schuifLinks(a: g32, b: g32) -> g32 {
            bekeer a >> b;
        }

        werkwijze hoofd() -> g32 {
            bekeer schuifLinks(255, 6);
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(3));
}

#[test]
fn printf_with_single_number() {
    let result = create_and_run_single_object_executable(r#"
        @flexibeleArgumenten
        @uitheems(naam: "printf")
        werkwijze printf(format: Slinger);

        werkwijze hoofd() -> g32 {
            printf("Hallo, %d", 10);
            bekeer 0;
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(0));
    assert_eq!(result.stdout, "Hallo, 10");
}

#[test]
fn printf_with_three_numbers() {
    let result = create_and_run_single_object_executable(r#"
        @flexibeleArgumenten
        @uitheems(naam: "printf")
        werkwijze printf(format: Slinger);

        werkwijze hoofd() -> g32 {
            printf("%x + %x = %x", 1, 3, 7);
            bekeer 0;
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(0));
    assert_eq!(result.stdout, "1 + 3 = 7");
}

#[test]
fn printf_with_string() {
    let result = create_and_run_single_object_executable(r#"
        @flexibeleArgumenten
        @uitheems(naam: "printf")
        werkwijze printf(format: Slinger);

        werkwijze hoofd() -> g32 {
            printf("Hallo, %s", "wereld");
            bekeer 0;
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(0));
    assert_eq!(result.stdout, "Hallo, wereld");
}

#[test]
fn printf_with_concatenated_string() {
    let result = create_and_run_single_object_executable(r#"
        @flexibeleArgumenten
        @uitheems(naam: "printf")
        werkwijze printf(format: Slinger);

        werkwijze hoofd() -> g32 {
            stel wereld = "wereld";
            stel hallo = "Hallo";
            printf(hallo + ", " + wereld);
            bekeer 0;
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(0));
    assert_eq!(result.stdout, "Hallo, wereld");
}

#[test]
fn printf_with_number_from_subroutine() {
    let result = create_and_run_single_object_executable(r#"
        @flexibeleArgumenten
        @uitheems(naam: "printf")
        werkwijze printf(format: Slinger);

        werkwijze krijgGetal() -> g32 {
            bekeer 100 + 20 + 3;
        }

        werkwijze hoofd() -> g32 {
            stel getal = krijgGetal();
            printf("%d ! %d", 99, getal);
            bekeer 0;
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(0));
    assert_eq!(result.stdout, "99 ! 123");
}

#[test]
fn var_arg_function_after_main() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze hoofd() -> g32 {
            printf("Hallo, %s", "wereld");
            bekeer 0;
        }

        @flexibeleArgumenten
        @uitheems(naam: "printf")
        werkwijze printf(format: Slinger);
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(0));
    assert_eq!(result.stdout, "Hallo, wereld");
}

#[test]
fn argument_survives_after_subroutine_call() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze doeIetsLeuks(a: g32) -> g32 {
            stel b = 10;
            bekeer a + b;
        }

        werkwijze overleef(g: g32) -> g32 {
            _ = doeIetsLeuks(g);
            bekeer g;
        }

        werkwijze hoofd() -> g32 {
            bekeer overleef(99);
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(99));
}

/// This test ensures that we keep good care of callee-saved registers, if it fails with a
/// SIGSEGV or incorrect exit code, look at whether or not registers are saved correctly.
#[test]
fn callee_saved_registers_correct_with_two_calls() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze doeIetsLeuks(a: g32) -> g32 {
            stel b = 10;
            bekeer a + b;
        }

        werkwijze overleef() -> g32 {
            stel b = doeIetsLeuks(5);
            stel a = doeIetsLeuks(9);
            bekeer a + b;
        }

        werkwijze doeIets() -> g32 {
            stel a = overleef();
            stel b = overleef();
            bekeer a + b;
        }

        werkwijze hoofd() -> g32 {
            bekeer doeIets();
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(68));
}

#[test]
fn store_bigger_value_in_smaller_field() {
    let result = create_and_run_single_object_executable(r#"
        structuur Data {
            veld a: g8,
            veld b: g8,
        }

        werkwijze hoofd() -> g32 {
            stel x = 123456;
            stel y = 777777;

            stel data = nieuw Data {
                a: x,
                b: y,
            };

            als data.a == x {
                als data.b == y {
                    bekeer 2;
                }

                bekeer 1;
            }

            bekeer 0;
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(0));
}

#[test]
fn string_concat_static() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze hoofd() -> g32 {
            stel a = "Hallo,";
            stel b = " wereld!";
            stel c = a + b;
            schrijf(c);
            bekeer c.lengte();
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(14));
    assert_eq!(result.stdout.trim_end_matches(|c| c == '\r' || c == '\n'), "Hallo, wereld!");
}

#[test]
fn string_concat_assign_static() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze hoofd() -> g32 {
            stel a = "Doei,";
            a += " gastje!";
            schrijf(a);
            bekeer a.lengte();
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(13));
    assert_eq!(result.stdout.trim_end_matches(|c| c == '\r' || c == '\n'), "Doei, gastje!");
}

#[test]
fn string_concat_loop() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze hoofd() -> g32 {
            stel a = "Doei";

            volg _ in reeks(0, 5) {
                a += "!";
            }

            schrijf(a);

            bekeer a.lengte();
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.stdout.trim_end_matches(|c| c == '\r' || c == '\n'), "Doei!!!!!");
    assert_eq!(result.exit_code, Some(9));
}

#[test]
fn type_resolution_return_g8() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze hoofd() -> g8 {
            bekeer 10;
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(10));
    assert_eq!(result.stdout, "");
}

#[test]
fn type_resolution_pass_g32() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze krijgG8(g: g8) -> g8 {
            bekeer g;
        }

        werkwijze hoofd() -> g8 {
            bekeer krijgG8(8);
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(8));
    assert_eq!(result.stdout, "");
}

#[test]
fn type_resolution_array_g8() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze hoofd() -> g8 {
            stel s = nieuw g8[2];
            s[0] = 5;
            s[1] = 4;

            bekeer s[0] * s[1];
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(20));
    assert_eq!(result.stdout, "");
}

#[rstest]
#[case("0 == 1", false)]
#[case("1 == 0", false)]
#[case("1 == 1", true)]
#[case("1 > 0", true)]
#[case("0 > 1", false)]
#[case("1 < 0", false)]
#[case("0 < 1", true)]
#[case("19 >= 19", true)]
#[case("16 >= 20", false)]
fn comparison_immediate(#[case] expr: &str, #[case] expected: bool) {
    let result = create_and_run_single_object_executable(&format!(r#"
        werkwijze hoofd() -> g32 {{
            bekeer {expr};
        }}
    "#));

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(expected as _));
}

#[rstest]
#[case("0 == 1", false)]
#[case("1 == 0", false)]
#[case("1 == 1", true)]
#[case("1 > 0", true)]
#[case("0 > 1", false)]
#[case("1 < 0", false)]
#[case("0 < 1", true)]
#[case("19 >= 19", true)]
#[case("16 >= 20", false)]
fn if_with_comparison_immediate(#[case] expr: &str, #[case] expected: bool) {
    let result = create_and_run_single_object_executable(&format!(r#"
        werkwijze hoofd() -> g32 {{
            als {expr} {{
                bekeer 1;
            }}
            bekeer 0;
        }}
    "#));

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(expected as _));
}

#[rstest]
#[case(0, "+=", 0, 0)]
#[case(1, "+=", 0, 1)]
#[case(0, "+=", 1, 1)]
#[case(31, "+=", 42, 73)]
#[case(0, "*=", 0, 0)]
#[case(1, "*=", 1, 1)]
#[case(31, "*=", 2, 62)]
fn assign_with_math_operation(#[case] first: i64, #[case] op: &str, #[case] second: i64, #[case] expected: i32) {
    let result = create_and_run_single_object_executable(&format!(r#"
        werkwijze krijgWaarde() -> g32 {{
            bekeer {second};
        }}

        werkwijze hoofd() -> g32 {{
            stel waarde = {first};
            waarde {op} krijgWaarde();
            bekeer waarde;
        }}
    "#));

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(expected));
}

#[test]
fn loop_continue_does_nothing() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze hoofd() -> g32 {
            volg i in reeks(0, 5) {
                schrijf("Hallo");
                vervolg;
            }
            bekeer 0;
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(0));
    assert_eq!(result.stdout, "Hallo\nHallo\nHallo\nHallo\nHallo\n");
}

#[test]
fn loop_break_without_if() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze hoofd() -> g32 {
            volg i in reeks(0, 5) {
                schrijf("Hallo");
                kap;
            }
            bekeer 0;
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(0));
    assert_eq!(result.stdout, "Hallo\n");
}

#[test]
fn loop_break_with_if_modulo() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze hoofd() -> g32 {
            volg i in reeks(0, 5) {
                als i % 3 == 2 {
                    kap;
                }
                schrijf("Doei");
            }
            bekeer 0;
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(0));
    assert_eq!(result.stdout, "Doei\nDoei\n");
}

/// Deze test checkt of de MADD-optimalisatie voor AArch64 goed werkt en dus
/// dat platform of andere platforms niet breekt.
#[test]
fn opt_multiply_then_add() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze keer_en_plus(a: g32, b: g32, c: g32) -> g32 {
            bekeer a * b + c;
        }

        werkwijze hoofd() -> g32 {
            bekeer keer_en_plus(3, 5, 6);
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(21));
    assert_eq!(result.stdout, "");
}

#[test]
fn opt_add_then_multiply() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze plus_en_keer(a: g32, b: g32, c: g32) -> g32 {
            bekeer a + b * c;
        }

        werkwijze hoofd() -> g32 {
            bekeer plus_en_keer(12, 5, 6);
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(42));
    assert_eq!(result.stdout, "");
}

/// Deze test checkt of de MSUB-optimalisatie voor AArch64 goed werkt en dus
/// dat platform of andere platforms niet breekt.
#[test]
fn opt_multiply_then_sub() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze keer_en_min(a: g32, b: g32, c: g32) -> g32 {
            bekeer a * b - c;
        }

        werkwijze hoofd() -> g32 {
            bekeer keer_en_min(3, 5, 6);
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(9));
    assert_eq!(result.stdout, "");
}

/// Deze test checkt of de MSUB-optimalisatie voor AArch64 goed werkt en dus
/// dat platform of andere platforms niet breekt.
#[test]
fn opt_sub_then_multiply() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze keer_en_min(a: g32, b: g32, c: g32) -> g32 {
            bekeer a - b * c;
        }

        werkwijze hoofd() -> g32 {
            bekeer keer_en_min(19, 3, 5);
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(4));
    assert_eq!(result.stdout, "");
}

#[test]
fn not_immediate_bool() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze hoofd() -> g32 {
            stel s = onwaar;

            als !s {
                bekeer 1;
            }

            bekeer 0;
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(1));
    assert_eq!(result.stdout, "");
}

#[test]
fn not_not_immediate_bool() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze hoofd() -> g32 {
            stel s = onwaar;

            als !!s {
                bekeer 1;
            }

            bekeer 0;
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(0));
    assert_eq!(result.stdout, "");
}

#[test]
fn not_from_subroutine() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze krijgBooleaan() -> bool {
            bekeer onwaar;
        }

        werkwijze hoofd() -> g32 {
            als !krijgBooleaan() {
                bekeer 1;
            }

            bekeer 2;
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(1));
    assert_eq!(result.stdout, "");
}

#[test]
fn not_not_from_subroutine() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze krijgBooleaan() -> bool {
            bekeer onwaar;
        }

        werkwijze hoofd() -> g32 {
            als !!krijgBooleaan() {
                bekeer 1;
            }

            bekeer 2;
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(2));
    assert_eq!(result.stdout, "");
}

#[test]
fn not_from_parameter() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze keerOm(a: bool) -> bool {
            bekeer !a;
        }

        werkwijze hoofd() -> g32 {
            als keerOm(onwaar) {
                bekeer 1;
            }

            bekeer 2;
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(1));
    assert_eq!(result.stdout, "");
}

#[test]
fn fill_array_using_subroutine() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze vulOpeenvolging(a: g32[]) {
            a[0] = 1;
            a[1] = 2;
            a[2] = 3;
            a[3] = 4;
            a[4] = 5;
        }

        werkwijze hoofd() -> g32 {
            stel x = nieuw g32[5];
            vulOpeenvolging(x);

            bekeer x[0] + x[1] + x[2] + x[3] + x[4];
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(15));
    assert_eq!(result.stdout, "");
}

#[test]
fn fill_array_g8() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze vulOpeenvolging(a: g8[]) {
            a[0] = 1;
            a[1] = 2;
            a[2] = 3;
            a[3] = 4;
            a[4] = 5;
        }

        werkwijze hoofd() -> g32 {
            stel x = nieuw g8[5];
            vulOpeenvolging(x);

            als x[2] == 3 {
                bekeer 1;
            }

            bekeer 0;
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(1));
    assert_eq!(result.stdout, "");
}

#[test]
fn variable_statement_type_specifier() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze krijg(a: g8) -> g8 {
            bekeer a;
        }

        werkwijze hoofd() -> g8 {
            stel a: g8 = 15;
            stel b = krijg(a);
            bekeer b;
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(15));
    assert_eq!(result.stdout, "");
}

#[test]
fn fill_local_by_ptr() {
    let result = create_and_run_single_object_executable(r#"
        werkwijze krijgGetal() -> g32 {
            bekeer 5;
        }

        @uitheems(naam: "memcpy")
        werkwijze memcpy(bestemming: g32*, bron: g32*, size: g64);

        werkwijze hoofd() -> g32 {
            stel a = 1;
            stel g = krijgGetal();

            memcpy(&a, &g, 4);

            bekeer a;
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(5));
    assert_eq!(result.stdout, "");
}

#[rstest]
#[case("%u", "g8", "128")]
#[case("%#x", "g8", "0xfe")]
#[case("%#x", "g8", "0xff")]
#[case("%u", "g16", "255")]
#[case("%#x", "g16", "0xfe")]
#[case("%#x", "g16", "0xff")]
#[case("%#x", "g16", "0x100")]
#[case("%#x", "g16", "0xfffe")]
#[case("%#x", "g16", "0xffff")]
#[case("%u", "g32", "255")]
#[case("%#x", "g32", "0xfe")]
#[case("%#x", "g32", "0xff")]
#[case("%#x", "g32", "0x100")]
#[case("%#x", "g32", "0xfffe")]
#[case("%#x", "g32", "0xffff")]
#[case("%#x", "g32", "0x10000")]
#[case("%#x", "g32", "0xfffffffe")]
#[case("%#x", "g32", "0xffffffff")]
#[case("%llu", "g64", "255")]
#[case("%#llx", "g64", "0xfe")]
#[case("%#llx", "g64", "0xff")]
#[case("%#llx", "g64", "0x100")]
#[case("%#llx", "g64", "0xfffe")]
#[case("%#llx", "g64", "0xffff")]
#[case("%#llx", "g64", "0x10000")]
#[case("%#llx", "g64", "0xfffffffe")]
#[case("%#llx", "g64", "0xffffffff")]
#[case("%#llx", "g64", "0x100000000")]
#[case("%#llx", "g64", "0xfffffffffffffffe")]
#[case("%#llx", "g64", "0xffffffffffffffff")]
#[case("%#llx", "g64", "0x1234567890abcdef")]
fn numeric_bounds_printf(#[case] format: &str, #[case] ty: &str, #[case] number: &str) {
    let result = create_and_run_single_object_executable(&format!(r#"
        werkwijze schrijfGetal(g: {ty}) {{
            printf("{format}", g);
        }}

        @flexibeleArgumenten
        @uitheems(naam: "printf")
        werkwijze printf(format: Slinger);

        werkwijze hoofd() -> g32 {{
            stel getal = {number};
            schrijfGetal(getal);
            bekeer 0;
        }}
    "#));

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(0));
    assert_eq!(result.stdout, number);
}

#[test]
fn ptr_to_first_field_is_same_as_ptr_to_structure() {
    let result = create_and_run_single_object_executable(r#"
        @flexibeleArgumenten
        @uitheems(naam: "printf")
        werkwijze printf(format: Slinger);

        structuur Structuurtje {
            veld lengte: g32,
        }

        werkwijze hoofd() -> g32 {
            stel adres = nieuw Structuurtje {
                lengte: 64,
            };

            printf("%p\n", &adres.lengte);
            printf("%p\n", &adres);

            bekeer 0;
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(0));

    let parts = result.stdout.trim().split('\n').collect::<Vec<_>>();
    let ptr_to_first_field = parts[0];
    let ptr_to_structure = parts[1];
    assert_eq!(ptr_to_first_field, ptr_to_structure);
}

#[test]
fn memcpy_of_structure_field_g32() {
    let result = create_and_run_single_object_executable(r#"
        @flexibeleArgumenten
        @uitheems(naam: "printf")
        werkwijze printf(format: Slinger);

        @uitheems(naam: "memcpy")
        werkwijze memcpy(bestemming: g32*, bron: Structuurtje*, lengte: g64);

        structuur Structuurtje {
            veld lengte: g32,
        }

        werkwijze krijgVeld(adres: Structuurtje*) -> g32 {
            stel w = 12;

            memcpy(&w, adres, 4);

            bekeer w;
        }

        werkwijze hoofd() -> g32 {
            stel adres = nieuw Structuurtje {
                lengte: 64,
            };

            bekeer krijgVeld(&adres);
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(64));
    assert_eq!(result.stdout, "");
}

#[test]
fn memcpy_of_structure_field_g8() {
    let result = create_and_run_single_object_executable(r#"
        @flexibeleArgumenten
        @uitheems(naam: "printf")
        werkwijze printf(format: Slinger);

        @uitheems(naam: "memcpy")
        werkwijze memcpy(bestemming: g32*, bron: Structuurtje*, lengte: g64);

        structuur Structuurtje {
            veld lengte: g8,
        }

        werkwijze krijgVeld(adres: Structuurtje*) -> g32 {
            stel w: g32 = 0;

            memcpy(&w, adres, 1);

            bekeer w;
        }

        werkwijze hoofd() -> g32 {
            stel adres = nieuw Structuurtje {
                lengte: 77,
            };

            bekeer krijgVeld(&adres);
        }
    "#);

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(77));
    assert_eq!(result.stdout, "");
}

fn create_and_run_single_object_executable(code: &str) -> ProgramResult {
    if !std::env::args().nth(1).unwrap_or_default().is_empty() || std::env::var("GITHUB_ACTION").is_ok() {
        let _ = env_logger::builder().is_test(true).filter(None, log::LevelFilter::max()).try_init();
    }

    let dir = TempDir::new().unwrap().panic_on_cleanup_error();
    let directory = dir.path().to_path_buf();
    dir.leak();

    let executable = create_single_object_executable(code, &directory);
    info!("Running executable {}", executable.display());
    run(executable).unwrap()
}

fn create_single_object_executable(code: &str, directory: &Path) -> std::path::PathBuf {
    let tree = parse_string_to_tree(code).unwrap();

    let mut pipeline = Pipeline::new(Platform::host_platform(), true);
    // let mut pipeline = Pipeline::new(Platform::new(babbelaar_compiler::Architecture::X86_64, babbelaar_compiler::Environment::Darwin, babbelaar_compiler::OperatingSystem::MacOs, Default::default()), true);
    pipeline.compile_trees(&[tree]);
    pipeline.create_object(directory, "BabBestand").unwrap();

    let executable = pipeline.link(directory, "BabUitvoerbare", ArchiveKind::Applicatie).unwrap();
    executable
}

fn run(path: impl AsRef<Path>) -> Result<ProgramResult, Box<dyn Error>> {
    let mut command = Command::new(path.as_ref());
    command.stdout(Stdio::piped());

    let mut process = command.spawn()?;
    let exit_status = process.wait()?;

    let stdout = process.stdout.and_then(|mut x| {
        let mut buf = String::new();
        x.read_to_string(&mut buf).ok()?;
        Some(buf)
    }).unwrap_or_default();

    let mut result = ProgramResult {
        exit_code: exit_status.code(),
        signal: None, // only set on UNIX-platforms below
        stdout,
        path: path.as_ref().into(),
    };

    result.exit_code = result.exit_code.map(|x| if x > 127 { x - 256 } else { x });

    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        result.signal = exit_status.signal().map(Signal::from);
    }

    Ok(result)
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(unused)]
struct ProgramResult {
    exit_code: Option<i32>,
    signal: Option<Signal>,
    stdout: String,
    path: PathBuf,
}
