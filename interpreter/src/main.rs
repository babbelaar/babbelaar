// Copyright (C) 2023 - 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

#![feature(thread_id_value)]
#![deny(elided_lifetimes_in_paths)]


mod data;
mod debugger;
mod error;
mod ffi;
mod interpreter;
mod scope;

use std::{collections::HashMap, fs::{create_dir_all, read_dir}, io, path::{Path, PathBuf}, process::{exit, Command, Stdio}, time::Instant};

use anyhow::Result;
pub use babbelaar::*;
use babbelaar_compiler::{Pipeline, Platform};
use clap::Subcommand;
use colored::Colorize;
use env_logger::Env;
use error::ErrorPrinter;

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
    debug: Option<bool>,
}

impl Args {
    #[must_use]
    pub fn parse_args() -> Self {
        use clap::Parser;
        Self::parse()
    }

    #[must_use]
    pub fn map(&self) -> Option<&Path> {
        let map = match &self.command {
            Commands::Bouwen { map } => map,
            Commands::Uitvoeren { map } => map,
        };
        Some(map.as_ref()?.as_path())
    }
}

#[derive(Subcommand, Debug)]
enum Commands {
    Bouwen {
        map: Option<PathBuf>,
    },

    Uitvoeren {
        map: Option<PathBuf>,
    },
}

fn main() {
    let args = Args::parse_args();
    let map = create_valid_project_directory(args.map());
    let config = read_config_file(&map, &args);

    init_logger(&config);

    match args.command {
        Commands::Bouwen { .. } => {
            let start = Instant::now();

            let path = compile(map, &config);

            println!("Gecompileerd in {}ms naar {}", start.elapsed().as_millis(), path.display());
        }

        Commands::Uitvoeren { .. } => {
            let path = compile(map, &config);
            Command::new(path)
                .stderr(Stdio::inherit())
                .stdout(Stdio::inherit())
                .spawn().unwrap()
                .wait().unwrap();
        }
    }
}

#[must_use]
fn create_valid_project_directory(map: Option<&Path>) -> PathBuf {
    let result = if let Some(dir) = map {
        if !dir.is_dir() {
            eprintln!("{} Gegeven pad is geen map: {}", "fout:".red().bold(), dir.display());
            exit(1);
        }

        dir.canonicalize()
    } else {
        std::env::current_dir().and_then(|x| x.canonicalize())
    };

    match result {
        Ok(dir) => dir,
        Err(e) => {
            eprintln!("{} {e}", "fout:".red().bold());

            match e.kind() {
                io::ErrorKind::NotFound => {
                    eprintln!("{} Projectmap bestaat niet. Heb je de huidige map verwijderd?", "tip".blue().bold());
                }

                _ => (),
            }

            exit(1);
        }
    }
}

fn compile(map: PathBuf, config: &ConfigRoot) -> PathBuf {
    if !map.is_dir() {
        eprintln!("{} Pad naar projectmap is geen map: {}", "fout:".red().bold(), map.display());
        exit(1);
    }

    let files: Vec<(SourceCode, ParseTree)> = read_dir(&map)
        .unwrap()
        .flatten()
        .filter(|x| x.file_name().to_string_lossy().ends_with(".bab"))
        .map(|x| parse(&x.path()))
        .collect();

    if files.is_empty() {
        eprintln!("{} Geen Babbelaar-bestanden gevonden in deze projectmap.", "fout:".red().bold());
        eprintln!("{} Map: {}", "fout:".red().bold(), map.display());
        exit(1);
    }

    analyze(&files);

    let trees: Vec<ParseTree> = files.into_iter()
        .map(|(_, tree)| tree)
        .collect();

    let mut pipeline = Pipeline::new(Platform::host_platform());
    pipeline.compile_trees(&trees);

    let dir = output_dir();

    pipeline.create_object(&dir, &config.project.naam).unwrap();
    pipeline.link_to_executable(&dir, &config.project.naam).unwrap()
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
    let file_ids: HashMap<FileId, SourceCode> = files.iter()
        .map(|(source_code, _)| (source_code.file_id(), source_code.clone()))
        .collect();

    let mut analyzer = SemanticAnalyzer::new(file_ids.clone(), true);

    for phase in SemanticAnalysisPhase::iter() {
        for (_, tree) in files {
            analyzer.analyze_tree(tree, phase);
        }
    }
    analyzer.finish_analysis();

    let diags = analyzer.into_diagnostics();
    let mut has_error = false;
    for diagnostic in &diags {
        let source_code = file_ids.get(&diagnostic.range().file_id()).unwrap();
        ErrorPrinter::new(source_code, diagnostic.range(), diagnostic.kind())
            .severity(diagnostic.severity())
            .hint(diagnostic.actions().first().map(|action| action.type_().to_string()))
            .print();

        if diagnostic.severity() == SemanticDiagnosticSeverity::Error {
            has_error = true;
        }
    }

    if !diags.is_empty() {
        if diags.len() == 1 {
            eprintln!("1 semantisch probleem gevonden");
        } else {
            eprintln!("{} semantische problemen gevonden", diags.len());
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
        ErrorPrinter::new(&source_code, e.location.as_zero_range(), &e.kind)
            .print();
    }

    let mut parser = Parser::new(path.to_path_buf(), &tokens);
    let tree = parser.parse_tree();
    if parser.diagnostics().is_empty() && errors.is_empty() {
        return (source_code, tree);
    }

    for e in parser.diagnostics() {
        ErrorPrinter::new(&source_code, e.range(), e)
            .print();
    }

    exit(1);
}

#[must_use]
fn output_dir() -> PathBuf {
    let mut dir = std::env::current_dir().unwrap();

    dir.push("uit");

    create_dir_all(&dir).unwrap();
    dir
}

fn init_logger(config: &ConfigRoot) {
    let mut builder = env_logger::builder();

    if config.log.debug {
        builder.filter(None, log::LevelFilter::Debug);
    }

    let env = Env::default()
        .filter("BABBELAAR_LOG")
        .write_style("BABBELAAR_LOGSTIJL");
    builder.parse_env(env);


    builder.init();
}

#[must_use]
fn read_config_file(root: &Path, args: &Args) -> ConfigRoot {
    let mut config = read_config_file_in(root).unwrap();

    if config.project.naam.is_empty() {
        config.project.naam = root.file_name().unwrap().to_string_lossy().into_owned();
    }

    if let Some(debug) = args.debug {
        config.log.debug = debug;
    }

    config
}

#[must_use]
fn read_config_file_in(root: &Path) -> Result<ConfigRoot> {
    let mut path = root.to_path_buf();
    path.push("Babbelaar.toml");

    let from_root = try_config_file(&path);

    path.pop();
    path.push("config");
    path.push("Babbelaar.toml");
    let from_config_dir = try_config_file(&path);

    if from_root.as_ref().is_ok_and(Option::is_some) && from_config_dir.as_ref().is_ok_and(Option::is_some) {
        eprintln!("{} `Babbelaar.toml` bestaat in de hoofdmap en in `config/Babbelaar.toml`!", "fout:     ".red().bold());
        eprintln!("{} Kies een van de twee opties, allebei kan niet.", "opmerking:".yellow().bold());
        exit(1);
    }

    if let Ok(Some(config)) = from_root {
        return Ok(config);
    }

    if let Ok(Some(config)) = from_config_dir {
        return Ok(config);
    }

    _ = from_root?;
    _ = from_config_dir?;

    Ok(ConfigRoot::default())
}

fn try_config_file(path: &Path) -> Result<Option<ConfigRoot>> {
    if !path.exists() {
        return Ok(None);
    }

    match std::fs::read_to_string(&path) {
        Ok(input) => {
            Ok(Some(toml::from_str(&input)?))
        }

        Err(e) => {
            if e.kind() == io::ErrorKind::NotFound {
                Ok(None)
            } else {
                Err(e.into())
            }
        }
    }
}
