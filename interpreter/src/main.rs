// Copyright (C) 2023 - 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

#![feature(thread_id_value)]
#![deny(elided_lifetimes_in_paths)]

mod data;
mod debugger;
mod debug_adapter;
mod error;
mod ffi;
mod interpreter;
mod logger;
mod scope;

use std::{fmt::Display, fs::read_dir, path::{Path, PathBuf}, process::exit};

pub use babbelaar::*;
// use babbelaar_compiler::LlvmContext;
use clap::Subcommand;
use colored::Colorize;
use logger::Logger;

pub use self::{
    data::{
        InterpreterFunction,
        InterpreterInterface,
        InterpreterStructure,
    },
    debugger::{
        Debugger,
        DebuggerFunction,
        DebuggerFunctionType,
    },
    debug_adapter::DebugAdapter,
    error::RuntimeError,
    ffi::FFIManager,
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
    Bouwen {
        bestand: PathBuf,
    },
    Debug {
        bestand: PathBuf,
    },
    Uitvoeren {
        bestand: PathBuf,
    },
}

fn main() {
    Logger::initialize();
    let args = Args::parse_args();

    match args.command {
        Commands::Bouwen { bestand } => {
            _ = bestand;
            // compile(bestand)
        }

        Commands::Debug { bestand } => {
            interpret(&bestand, DebugAdapter::new(bestand.to_string_lossy().to_string()));
        }
        Commands::Uitvoeren { bestand } => {
            interpret(&bestand, ());
        }
    }
}

// fn compile(bestand: PathBuf) {
//     let source_code = std::fs::read_to_string(&bestand).unwrap();
//     let mut babbelaar = BabbelaarContext::new(bestand, source_code);
//     let mut llvm = LlvmContext::new();

//     babbelaar.with_tree(|tree| llvm.parse_tree(tree)).unwrap();

//     llvm.finish();
// }

pub fn interpret<D: Debugger>(path: &Path, debugger: D) {
    let files: Vec<(SourceCode, ParseTree)> = read_dir(&path.parent().unwrap())
        .unwrap()
        .flatten()
        .filter(|x| x.file_name().to_string_lossy().ends_with(".bab"))
        .map(|x| parse(&x.path()))
        .collect();

    analyze(&files);

    let mut interpreter = Interpreter::new(debugger);

    for (_, tree) in files {
        interpreter.execute_tree(&tree);
    }
}

fn analyze(files: &[(SourceCode, ParseTree)]) {
    let file_ids = files.iter()
        .map(|(source_code, _)| (source_code.file_id(), source_code.clone()))
        .collect();

    let mut analyzer = SemanticAnalyzer::new(file_ids);

    for (_, tree) in files {
        analyzer.analyze_tree_phase_1(tree);
    }

    for (_, tree) in files {
        analyzer.analyze_tree_phase_2(tree);
    }

    let diags = analyzer.into_diagnostics();
    let mut has_error = false;
    for diagnostic in &diags {
        eprintln!("{}: {}", diagnostic.severity(), diagnostic.kind());

        if diagnostic.severity() == SemanticDiagnosticSeverity::Error {
            has_error = true;
        }
    }

    if !diags.is_empty() {
        if diags.len() == 1 {
            eprintln!("1 semantisch probleem gevonden");
        } else {
            eprintln!("{} semantische {} gevonden",
                diags.len(),
                if diags.len() == 1 {  "probleem" } else { "problemen" }
            );
        }
    }

    if has_error {
        exit(2);
    }
}

fn parse(path: &Path) -> (SourceCode, ParseTree) {
    let source_code = std::fs::read_to_string(path).unwrap();
    let source_code = SourceCode::new(path, 0, source_code);

    let lexer = Lexer::new(&source_code);
    let (tokens, errors) = lexer.collect_all();

    for e in &errors {
        print_error(&source_code, e.location.as_zero_range(), &e.kind);
    }

    let mut parser = Parser::new(path.to_path_buf(), &tokens);
    let tree = parser.parse_tree();
    if parser.diagnostics().is_empty() && errors.is_empty() {
        return (source_code, tree);
    }

    for e in parser.diagnostics() {
        print_error(&source_code, e.range(), e);
    }

    exit(1);
}

fn print_error(source_code: &SourceCode, range: FileRange, message: impl Display) {
    eprintln!("{}: {}", "fout".red().bold(), message.to_string().bold());

    eprintln!();
    let mut iter = source_code.lines().skip(range.start().line().saturating_sub(1));

    if let Some(line) = iter.next() {
        if !line.trim().is_empty() {
            eprintln!("{}", line);
        }
    }

    if let Some(line) = iter.next() {
        eprintln!("{line}");
        eprintln!(
            "{spaces}{caret}{tildes} {description}",
            spaces = " ".repeat(range.start().column()),
            caret = "^".bright_red().bold(),
            tildes = "~".repeat(range.len().saturating_sub(1)).bright_blue(),
            description = "fout trad hier op".bright_red()
        );
    }

    if let Some(line) = iter.next() {
        if !line.trim().is_empty() {
            eprintln!("{}", line);
        }
    }

    eprintln!();

    eprintln!("In {}:{}:{}\n", source_code.path().display(), range.start().line() + 1, range.start().column() + 1);
}
