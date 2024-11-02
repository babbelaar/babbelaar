// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{error::Error, path::Path, process::{Command, ExitStatus}};

use babbelaar::parse_string_to_tree;
use babbelaar_compiler::{Pipeline, Platform};
use temp_dir::TempDir;

#[test]
fn simple_return_0() {
    let dir = TempDir::new().unwrap().panic_on_cleanup_error();

    let tree = parse_string_to_tree("
        werkwijze hoofd() -> g32 {
            bekeer 123;
        }
    ").unwrap();

    let mut pipeline = Pipeline::new(Platform::host_platform());
    pipeline.compile_trees(&[tree]);
    pipeline.create_object(dir.path(), "BabBestand").unwrap();

    let executable = pipeline.link_to_executable(dir.path(), "BabUitvoerbare").unwrap();

    let exit_status = run(executable).unwrap();

    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        assert_eq!(exit_status.signal(), None);
    }

    assert_eq!(exit_status.code(), Some(123));
}

fn run(path: impl AsRef<Path>) -> Result<ExitStatus, Box<dyn Error>> {
    let mut command = Command::new(path.as_ref());
    let mut process = command.spawn()?;
    Ok(process.wait()?)
}
