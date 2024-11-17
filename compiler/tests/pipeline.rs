// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{error::Error, path::Path, process::{Command, ExitStatus}};

use babbelaar::parse_string_to_tree;
use babbelaar_compiler::{Pipeline, Platform};
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
fn method_call_with_this() {
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

fn create_and_run_single_object_executable(code: &str) -> ProgramResult {
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
        result.signal = exit_status.signal();
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
    signal: Option<i32>,
}
