// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::HashMap, path::{Path, PathBuf}, sync::Arc};

use dashmap::DashMap;

use babbelaar::{FileId, Lexer, ParseDiagnostic, ParseTree, Parser, SemanticAnalyzer, SourceCode, Token};
use tokio::sync::{Mutex, RwLock};
use tower_lsp::lsp_types::Url;

use crate::BabbelaarLspError;

#[derive(Debug)]
pub struct BabbelaarContext {
    files: DashMap<PathBuf, Arc<Mutex<BabbelaarFile>>>,
    semantic_analysis: RwLock<Option<Arc<SemanticAnalyzer>>>,
}

impl BabbelaarContext {
    pub fn new() -> Self {
        Self {
            files: DashMap::new(),
            semantic_analysis: RwLock::new(None),
        }
    }

    pub async fn register_file(&self, source_code: SourceCode) {
        let path = source_code.path().canonicalize().unwrap();
        self.files.insert(path, Arc::new(Mutex::new(BabbelaarFile::new(source_code))));
        *self.semantic_analysis.write().await = None;
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

        let source_code = SourceCode::new(path, 0, contents);
        self.register_file(source_code.clone()).await;

        Ok(source_code)
    }

    pub async fn with_file<K, F, R>(&self, key: K, f: F) -> Result<R, BabbelaarLspError>
            where K: ContextKey,
                  F: FnOnce(&mut BabbelaarFile) -> Result<R, BabbelaarLspError> {
        let path = key.to_context_key();

        if !self.files.contains_key(&path) {
            self.load_and_register_file(path.clone()).await?;
        }

        let mutex = self.files.get(&path).unwrap().value().clone();
        let mut file = mutex.lock().await;
        let result = f(&mut file);
        result
    }

    pub async fn with_all_files<F>(&self, mut f: F) -> Result<(), BabbelaarLspError>
            where F: FnMut(&mut BabbelaarFile) -> Result<(), BabbelaarLspError> {
        for file in self.files.iter() {
            let mut file = file.lock().await;
            f(&mut file)?;
        }
        Ok(())
    }

    pub async fn semantic_analysis(&self) -> Arc<SemanticAnalyzer> {
        if let Some(analyzer) = self.semantic_analysis.read().await.as_ref() {
            return Arc::clone(&analyzer);
        }

        let mut lock = self.semantic_analysis.write().await;
        let mut files = HashMap::new();
        for file in self.files.iter() {
            let file = file.lock().await;
            files.insert(file.source_code.file_id(), file.source_code.clone());
        }

        let mut analyzer = SemanticAnalyzer::new(files);

        // Pre-phase
        for file in self.files.iter() {
            let mut file = file.lock().await;
            let tree = file.ensure_parsed().0;
            analyzer.analyze_tree_phase_1(tree);
        }

        for file in self.files.iter() {
            let mut file = file.lock().await;
            let tree = file.ensure_parsed().0;
            analyzer.analyze_tree_phase_2(tree);
        }

        let analyzer = Arc::new(analyzer);
        *lock = Some(Arc::clone(&analyzer));

        analyzer
    }

    pub async fn path_of(&self, file_id: FileId) -> Option<PathBuf> {
        for pair in self.files.iter() {
            let file = pair.lock().await;
            if file.source_code.file_id() == file_id {
                return Some(file.source_code.path().to_path_buf());
            }
        }

        log::warn!("Kan bestand niet vinden met het nummer {file_id:?}");
        None
    }
}

#[derive(Debug)]
pub struct BabbelaarFile {
    source_code: SourceCode,

    tokens: Option<Vec<Token>>,
    tree: Option<Result<ParseTree, ParseDiagnostic>>,
    parse_errors: Vec<ParseDiagnostic>,
}

impl BabbelaarFile {
    pub fn new(source_code: SourceCode) -> Self {
        Self {
            source_code,
            tokens: None,
            tree: None,
            parse_errors: Vec::new(),
        }
    }

    pub fn ensure_lexed(&mut self) -> (&[Token], &SourceCode) {
        if self.tokens.is_some() {
            return (self.tokens.as_ref().unwrap(), self.source_code());
        }

        let lexer = Lexer::new(&self.source_code);
        self.tokens = Some(lexer.collect());

        (self.tokens.as_ref().unwrap(), self.source_code())
    }

    pub fn ensure_parsed(&mut self) -> (&ParseTree, &SourceCode) {
        self.ensure_lexed();

        if self.tree.is_some() {
            return (self.tree.as_ref().unwrap().as_ref().unwrap(), &self.source_code);
        }

        let tokens = self.tokens.as_ref().expect("ensure_lexed() should've prepared our tokens");

        let path = self.source_code.path().to_path_buf();
        let mut parser = Parser::new(path, &tokens)
            .attempt_to_ignore_errors();

        let tree = parser.parse_tree();

        self.tree = Some(tree);
        self.parse_errors = parser.errors;

        (self.tree.as_ref().unwrap().as_ref().unwrap(), &self.source_code)
    }

    pub fn parse_diagnostics(&mut self) -> &[ParseDiagnostic] {
        _ = self.ensure_parsed();
        &self.parse_errors
    }

    #[must_use]
    pub fn source_code(&self) -> &SourceCode {
        &self.source_code
    }
}

pub trait ContextKey {
    fn to_context_key(self) -> PathBuf;
}

impl ContextKey for PathBuf {
    fn to_context_key(self) -> PathBuf {
        self.clone()
    }
}

impl ContextKey for &Path {
    fn to_context_key(self) -> PathBuf {
        self.to_path_buf()
    }
}

impl ContextKey for Url {
    fn to_context_key(self) -> PathBuf {
        self.to_file_path().unwrap().to_context_key()
    }
}

impl ContextKey for &Url {
    fn to_context_key(self) -> PathBuf {
        self.to_file_path().unwrap().to_context_key()
    }
}
