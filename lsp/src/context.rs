// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{path::{Path, PathBuf}, sync::RwLock};

use dashmap::DashMap;

use babbelaar::{Lexer, ParseDiagnostic, ParseTree, Parser, SemanticAnalyzer, SourceCode, Token};

use crate::BabbelaarLspError;

#[derive(Debug)]
pub struct BabbelaarContext {
    files: DashMap<PathBuf, BabbelaarFile>,
    semantic_analysis: RwLock<Option<SemanticAnalyzer>>,
}

impl BabbelaarContext {
    pub fn new() -> Self {
        Self {
            files: DashMap::new(),
            semantic_analysis: RwLock::new(None),
        }
    }

    pub fn register_file(&self, source_code: SourceCode) {
        let path = source_code.path().canonicalize().unwrap();
        self.files.insert(path, BabbelaarFile::new(source_code));
        *self.semantic_analysis.write().unwrap() = None;
    }

    pub async fn load_and_register_file(&self, path: PathBuf) -> Result<SourceCode, BabbelaarLspError> {
        let mut contents = match tokio::fs::read_to_string(&path).await {
            Ok(contents) => contents,
            Err(e) => {
                log::warn!("Failed to read path \"{}\" {e}", path.display());
                return Err(e.into());
            }
        };

        // Either `read_to_string` omits the trailing empty line, or the editor assumes
        // there is one regardless of the disk contents, but we have to append one to be sure.
        if !contents.ends_with("\n\n") {
            contents += "\n";
        }

        let source_code = SourceCode::new(path, contents);
        self.register_file(source_code.clone());

        Ok(source_code)
    }

    pub fn source_code_of(&self, path: impl AsRef<Path>) -> Option<SourceCode> {
        Some(self.files.get(path.as_ref())?.source_code.clone())
    }

    fn analyze(&self) {
        // let mut analyzer = SemanticAnalyzer::new(self.source_code.clone());
        // analyzer.analyze_tree(ctx.tree.as_ref().unwrap());

        // ctx.analyzer = Some(analyzer);
    }
}

#[derive(Debug)]
struct BabbelaarFile {
    source_code: SourceCode,

    tokens: Option<Vec<Token>>,
    tree: Option<Result<ParseTree, ParseDiagnostic>>,
}

impl BabbelaarFile {
    pub fn new(source_code: SourceCode) -> Self {
        Self {
            source_code,
            tokens: None,
            tree: None,
        }
    }

    pub fn with_tree(&mut self) -> &ParseTree {
        self.ensure_parsed();
        self.tree.as_ref().unwrap().as_ref().unwrap()
    }

    pub fn ensure_lexed(&mut self) {
        if self.tokens.is_some() {
            return;
        }

        let lexer = Lexer::new(&self.source_code);
        self.tokens = Some(lexer.collect());
    }

    pub fn ensure_parsed(&mut self) {
        self.ensure_lexed();

        if self.tree.is_some() {
            return;
        }

        let tokens = self.tokens.as_ref().expect("ensure_lexed() should've prepared our tokens");

        let path = self.source_code.path().to_path_buf();
        let tree = Parser::new(path, &tokens)
            .attempt_to_ignore_errors()
            .parse_tree();

        self.tree = Some(tree);
    }

    pub fn ensure_analyzed(&mut self) {
        self.ensure_parsed();
    }
}
