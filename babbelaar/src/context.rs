// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{fmt::Debug, path::PathBuf};

use self_cell::self_cell;

use crate::{Lexer, ParseDiagnostic, ParseTree, Parser, SemanticAnalyzer, Token};

pub struct BabbelaarContext {
    inner: BabbelaarContextImpl,
}

impl BabbelaarContext {
    pub fn new(path: PathBuf, source_code: String) -> Self {
        Self {
            inner: BabbelaarContextImpl::new(
                BabbelaarContextData {
                    path,
                    source_code,
                },
                |_| {
                    BabbelaarContextState::default()
                }
            )
        }
    }

    pub fn with_tree<F: FnOnce(&ParseTree<'_>) -> R, R>(&mut self, f: F) -> R{
        self.ensure_parsed();

        self.inner.with_dependent(|_, state| {
            f(state.tree.as_ref().unwrap().borrow_owner().as_ref().unwrap())
        })
    }

    pub fn ensure_lexed(&mut self) {
        self.inner.with_dependent_mut(|data, state| {
            if state.tokens.is_some() {
                return;
            }

            let lexer = Lexer::new(&data.source_code);
            state.tokens = Some(lexer.collect());
        });
    }

    pub fn ensure_parsed(&mut self) {
        self.ensure_lexed();

        self.inner.with_dependent_mut(|data, state| {
            if state.tree.is_some() {
                return;
            }

            let tokens = state.tokens.as_ref().expect("ensure_lexed() should've prepared our tokens");

            let tree = Parser::new(data.path.clone(), &tokens)
                .attempt_to_ignore_errors()
                .parse_tree();

            state.tree = Some(BabbelaarParserContext::new(tree, |_| None));
        });
    }

    pub fn ensure_analyzed(&mut self) {
        self.ensure_parsed();

        self.inner.with_dependent_mut(|ctx, state| {
            let tree = state.tree.as_mut().unwrap();

            tree.with_dependent_mut(|tree, analyzer_value| {
                if analyzer_value.is_some() {
                    return;
                }

                let mut analyzer = SemanticAnalyzer::new(&ctx.source_code);
                analyzer.analyze_tree(tree.as_ref().unwrap());

                *analyzer_value = Some(analyzer);
            });
        });
    }
}

#[derive(Debug)]
struct BabbelaarContextData {
    path: PathBuf,
    source_code: String,
}

#[derive(Debug, Default)]
struct BabbelaarContextState<'owner> {
    tokens: Option<Vec<Token<'owner>>>,
    tree: Option<BabbelaarParserContext<'owner>>,
}

self_cell! {
    struct BabbelaarContextImpl {
        owner: BabbelaarContextData,

        #[covariant]
        dependent: BabbelaarContextState,
    }

    impl {Debug}
}

type SemanticAnalyzerOpt<'owner> = Option<SemanticAnalyzer<'owner>>;

self_cell! {
    struct BabbelaarParserContext<'a> {
        owner: Result<ParseTree<'a>, ParseDiagnostic<'a>>,

        #[covariant]
        dependent: SemanticAnalyzerOpt,
    }
}

impl<'a> Debug for BabbelaarParserContext<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BabbelaarParserContext")
            .finish_non_exhaustive()
    }
}
