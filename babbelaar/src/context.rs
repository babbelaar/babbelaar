// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::Debug;

use crate::{Lexer, ParseDiagnostic, ParseTree, Parser, SemanticAnalyzer, SourceCode, Token};

pub struct BabbelaarContext {
    source_code: SourceCode,

    tokens: Option<Vec<Token>>,
    tree: Option<BabbelaarParserContext>,
}

impl BabbelaarContext {
    pub fn new(source_code: SourceCode) -> Self {
        Self {
            source_code,
            tokens: None,
            tree: None,
        }
    }

    pub fn with_tree(&mut self) -> &ParseTree {
        self.ensure_parsed();
        self.tree.as_ref().unwrap().tree.as_ref().unwrap()
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

        self.tree = Some(BabbelaarParserContext {
            tree,
            analyzer: None,
        });
    }

    pub fn ensure_analyzed(&mut self) {
        self.ensure_parsed();

        let ctx = self.tree.as_mut().unwrap();

        if ctx.analyzer.is_some() {
            return;
        }

        let mut analyzer = SemanticAnalyzer::new(self.source_code.clone());
        analyzer.analyze_tree(ctx.tree.as_ref().unwrap());

        ctx.analyzer = Some(analyzer);
    }
}

struct BabbelaarParserContext {
    tree: Result<ParseTree, ParseDiagnostic>,
    analyzer: Option<SemanticAnalyzer>,
}

impl<'a> Debug for BabbelaarParserContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BabbelaarParserContext")
            .finish_non_exhaustive()
    }
}
