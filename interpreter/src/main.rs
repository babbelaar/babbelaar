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
mod scope;

use std::{fmt::Display, fs::{create_dir_all, read_dir}, path::{Path, PathBuf}, process::{exit, Command, Stdio}, time::Instant};

pub use babbelaar::*;
use babbelaar_compiler::{Pipeline, Platform};
use clap::Subcommand;
use colored::Colorize;
use env_logger::Env;

pub use self::{
    data::{
        InterpreterExtension,
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

    #[arg(short, long)]
    debug: bool,
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
    let args = Args::parse_args();
    init_logger(&args);

    match args.command {
        Commands::Bouwen { bestand } => {
            let start = Instant::now();

            let path = compile(bestand);

            println!("Gecompileerd in {}ms naar {}", start.elapsed().as_millis(), path.display());
        }

        Commands::Debug { bestand } => {
            interpret(&bestand, DebugAdapter::new(bestand.to_string_lossy().to_string()));
        }
        Commands::Uitvoeren { bestand } => {
            // interpret(&bestand, ());
            let path = compile(bestand);
            Command::new(path)
                .stderr(Stdio::inherit())
                .stdout(Stdio::inherit())
                .spawn().unwrap()
                .wait().unwrap();
        }
    }
}

fn compile(bestand: PathBuf) -> PathBuf {
    let files: Vec<(SourceCode, ParseTree)> = read_dir(&bestand.parent().unwrap())
        .unwrap()
        .flatten()
        .filter(|x| x.file_name().to_string_lossy().ends_with(".bab"))
        .map(|x| parse(&x.path()))
        .collect();

    analyze(&files);

    let trees: Vec<ParseTree> = files.into_iter()
        .map(|(_, tree)| tree)
        .collect();

    let mut pipeline = Pipeline::new(Platform::host_platform());
    pipeline.compile_trees(&trees);

    let dir = output_dir();
    let exec_name = bestand.file_stem().unwrap().to_string_lossy();

    pipeline.create_object(&dir, &exec_name).unwrap();
    pipeline.link_to_executable(&dir, &exec_name).unwrap()
}

pub fn interpret<D: Debugger>(path: &Path, debugger: D) {
    let files: Vec<(SourceCode, ParseTree)> = read_dir(&path.parent().unwrap())
        .unwrap()
        .flatten()
        .filter(|x| x.file_name().to_string_lossy().ends_with(".bab"))
        .map(|x| parse(&x.path()))
        .collect();

    analyze(&files);

    let mut interpreter = Interpreter::new(debugger);

    let trees: Vec<ParseTree> = files.into_iter()
        .map(|(_, tree)| tree)
        .collect();

    interpreter.execute_trees(&trees);
}

fn analyze(files: &[(SourceCode, ParseTree)]) {
    let file_ids = files.iter()
        .map(|(source_code, _)| (source_code.file_id(), source_code.clone()))
        .collect();

    let mut analyzer = SemanticAnalyzer::new(file_ids, true);

    for phase in SemanticAnalysisPhase::iter() {
        for (_, tree) in files {
            analyzer.analyze_tree(tree, phase);
        }
    }
    analyzer.finish_analysis();

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

#[must_use]
fn output_dir() -> PathBuf {
    let mut dir = std::env::current_dir().unwrap();

    dir.push("uit");

    create_dir_all(&dir).unwrap();
    dir
}

fn init_logger(args: &Args) {
    let mut builder = env_logger::builder();

    if args.debug {
        builder.filter(None, log::LevelFilter::Debug);
    }

    let env = Env::default()
        .filter("BABBELAAR_LOG")
        .write_style("BABBELAAR_LOGSTIJL");
    builder.parse_env(env);


    builder.init();
}
