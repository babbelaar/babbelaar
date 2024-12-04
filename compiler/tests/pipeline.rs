// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{error::Error, path::Path, process::{Command, ExitStatus}};

use babbelaar::parse_string_to_tree;
use babbelaar_compiler::{Pipeline, Platform, Signal};
use temp_dir::TempDir;

#[test]
fn simple_return_0() {
    let result = create_and_run_single_object_executable("
        werkwijze hoofd() -> g32 {
            bekeer 123;
        }
    ");

    assert_eq!(result.signal, None);
    assert_eq!(result.exit_code, Some(123));
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
    assert_eq!(result.exit_code, Some(4));
}

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
            stel s = "hallo".lengte();
            volg i in reeks(0, s) {
                totaal = totaal + 1;
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

fn create_and_run_single_object_executable(code: &str) -> ProgramResult {
    let _ = env_logger::builder().is_test(true).filter(None, log::LevelFilter::max()).try_init();

    let dir = TempDir::new().unwrap().panic_on_cleanup_error();
    let directory = dir.path().to_path_buf();
    dir.leak();

    let executable = create_single_object_executable(code, &directory);
    println!("Running executable {}", executable.display());
    let exit_status = run(executable).unwrap();

    let mut result = ProgramResult {
        exit_code: exit_status.code(),
        signal: None, // only set on UNIX-platforms below
    };

    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        result.signal = exit_status.signal().map(Signal::from);
    }

    result
}

fn create_single_object_executable(code: &str, directory: &Path) -> std::path::PathBuf {
    let tree = parse_string_to_tree(code).unwrap();

    let mut pipeline = Pipeline::new(Platform::host_platform());
    pipeline.compile_trees(&[tree]);
    pipeline.create_object(directory, "BabBestand").unwrap();

    let executable = pipeline.link_to_executable(directory, "BabUitvoerbare").unwrap();
    executable
}

fn run(path: impl AsRef<Path>) -> Result<ExitStatus, Box<dyn Error>> {
    let mut command = Command::new(path.as_ref());
    let mut process = command.spawn()?;
    Ok(process.wait()?)
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ProgramResult {
    exit_code: Option<i32>,
    signal: Option<Signal>,
}
