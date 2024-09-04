// Copyright (C) 2023 - 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

#![feature(thread_id_value)]
#![deny(elided_lifetimes_in_paths)]

mod debugger;
mod debug_adapter;
mod interpreter;
mod scope;

use std::{path::{Path, PathBuf}, process::exit};

pub use babbelaar::*;
use babbelaar_compiler::LlvmContext;
use clap::Subcommand;
use colored::Colorize;

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

    match args.command {
        Commands::Bouwen { bestand } => {
            compile(bestand)
        }

        Commands::Debug { bestand } => {
            interpret(&bestand, DebugAdapter::new(bestand.to_string_lossy().to_string()));
        }
        Commands::Uitvoeren { bestand } => {
            interpret(&bestand, ());
        }
    }
}

fn compile(bestand: PathBuf) {
    let source_code = std::fs::read_to_string(&bestand).unwrap();
    let mut babbelaar = BabbelaarContext::new(bestand, source_code);
    let mut llvm = LlvmContext::new();

    babbelaar.with_tree(|tree| llvm.parse_tree(tree)).unwrap();

    llvm.finish();
}

pub fn interpret<D: Debugger>(path: &Path, debugger: D) {
    parse(path, |tree| {
        analyze(&tree);

        let mut interpreter = Interpreter::new(debugger);
        interpreter.execute_tree(&tree);
    });
}

fn analyze(tree: &ParseTree<'_>) {
    let mut analyzer = SemanticAnalyzer::new();
    analyzer.analyze_tree(&tree);

    let diags = analyzer.into_diagnostics();
    for diagnostic in &diags {
        eprintln!("Fout: {}", &diagnostic.kind);
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

        exit(2);
    }
}

fn parse(path: &Path, f: impl FnOnce(ParseTree<'_>)) {
    let source_code = std::fs::read_to_string(path).unwrap();

    let lexer = Lexer::new(&source_code);
    let tokens: Vec<_> = lexer.collect();

    let mut parser = Parser::new(path.to_path_buf(), &tokens);
    match parser.parse_tree() {
        Ok(tree) => f(tree),
        Err(e) => {
            eprintln!("{}: {}", "fout".red().bold(), e.to_string().bold());

            if let Some(range) = e.range() {
                eprintln!();
                let mut iter = source_code.lines().skip(range.start().line() - 1);

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

                eprintln!("In {}:{}:{}\n", path.display(), range.start().line() + 1, range.start().column() + 1);
                exit(1);
            }
        }
    }
}
