// Copyright (C) 2023 - 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

#![feature(thread_id_value)]
#![deny(elided_lifetimes_in_paths)]

mod debugger;
mod debug_adapter;
mod interpreter;
mod scope;

use std::path::{Path, PathBuf};

pub use babbelaar::*;
use clap::Subcommand;

pub use self::{
    debugger::{
        Debugger,
        DebuggerFunction,
        DebuggerFunctionType,
    },
    debug_adapter::DebugAdapter,
    interpreter::Interpreter,
    scope::Scope,
};

#[derive(clap::Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Commands,

    #[arg(short, long)]
    verbose: bool,
}

impl Args {
    pub fn parse_args() -> Self {
        use clap::Parser;
        Self::parse()
    }
}

#[derive(Subcommand, Debug)]
enum Commands {
    Debug {
        bestand: PathBuf,
    },
    Uitvoeren {
        bestand: PathBuf,
    },
}

fn main() {
    let args = Args::parse_args();

    match args.command {
        Commands::Debug { bestand } => {
            interpret(&bestand, DebugAdapter::new(bestand.to_string_lossy().to_string()));
        }
        Commands::Uitvoeren { bestand } => {
            interpret(&bestand, ());
        }
    }
}

pub fn interpret<D: Debugger>(path: &Path, debugger: D) {
    eprintln!("INTEPRETTTT");
    let source_code = std::fs::read_to_string(path).unwrap();

    let lexer = Lexer::new(&source_code);
    let tokens: Vec<_> = lexer.collect();

    let mut parser = Parser::new(&tokens);

    let mut interpreter = Interpreter::new(debugger);

    loop {
        let statement = parser.parse_statement();

        match statement {
            Ok(statement) => {
                interpreter.execute(&statement);
            }
            Err(ParseError::EndOfFile) => break,
            Err(e) => {
                eprintln!("Fout: {e}");
                break;
            }
        }
    }
}
