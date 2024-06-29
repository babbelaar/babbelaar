// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use babbelaar::*;
use tower_lsp::{
    jsonrpc::Result,
    lsp_types::{CompletionItem, CompletionItemKind, CompletionItemLabelDetails, CompletionParams, CompletionResponse, Documentation, InsertTextFormat, MarkupContent, MarkupKind},
};

use crate::Backend;

pub struct CompletionEngine<'b> {
    server: &'b Backend,
    params: CompletionParams,
    completions: Vec<CompletionItem>,
}

impl<'b> CompletionEngine<'b> {
    pub async fn complete(server: &'b Backend, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let mut this = Self {
            server,
            params,
            completions: Vec::new(),
        };

        if this.do_completions().await? {
            Ok(Some(CompletionResponse::Array(this.completions)))
        } else {
            Ok(None)
        }
    }

    async fn do_completions(&mut self) -> Result<bool> {
        match self.detect_completion_mode().await? {
            Some(CompletionMode::Function((range, ident))) => {
                self.complete_global_function(&ident).await?;
                self.complete_variable(range).await?;
                self.suggest_identifiers(&ident);
                Ok(true)
            }

            Some(CompletionMode::Method((last_identifier, _))) => {
                self.complete_method(last_identifier).await?;
                Ok(true)
            }

            None => Ok(false),
        }
    }

    async fn detect_completion_mode(&self) -> Result<Option<CompletionMode>> {
        let mut was_new_func = false;
        let mut prev_punc = None;
        let completion_mode = self.server.find_tokens_at(&self.params.text_document_position, |token, previous| {
            was_new_func = previous.last().is_some_and(|tok| matches!(tok.kind, TokenKind::Keyword(Keyword::Functie)));
            prev_punc = previous.last().and_then(|x| if let TokenKind::Punctuator(punc) = x.kind { Some(punc) } else { None });

            if prev_punc == Some(Punctuator::Period) {
                if let Some(token) = previous.get(previous.len() - 2) {
                    if let TokenKind::Identifier(ident) = &token.kind {
                        return Ok(Some(CompletionMode::Method((token.range(), ident.to_string()))));
                    }
                }
            }

            if token.kind == TokenKind::Punctuator(Punctuator::Period) {
                if let Some(token) = previous.last() {
                    eprintln!("Previous = {token:?}");
                    if let TokenKind::Identifier(ident) = &token.kind {
                        return Ok(Some(CompletionMode::Method((token.range(), ident.to_string()))));
                    }
                }
            }

            if let TokenKind::Identifier(ident) = &token.kind {
                Ok(Some(CompletionMode::Function((token.range(), ident.to_string()))))
            } else {
                Ok(None)
            }
        }).await?;

        if was_new_func {
            eprintln!("Was new func");
            return Ok(None);
        }

        Ok(completion_mode)
    }

    async fn complete_global_function(&mut self, ident: &str) -> Result<()> {
        let document = &self.params.text_document_position.text_document;

        self.server.with_semantics(document, |analyzer| {
            if let Some(func) = analyzer.find_function_by_name(|f| f.starts_with(&ident)) {
                self.completions.push(CompletionItem {
                    label: func.function_name().to_string(),
                    kind: Some(CompletionItemKind::FUNCTION),
                    detail: func.inline_detail().map(|x| x.to_string()),
                    documentation: func.documentation().map(|x| Documentation::MarkupContent(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: x.to_string(),
                    })),
                    insert_text: Some(func.lsp_completion().into_owned()),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    ..Default::default()
                });
            }

            Ok(())
        }).await
    }

    async fn complete_variable(&mut self, range: FileRange) -> Result<()> {
        let document = &self.params.text_document_position.text_document;

        self.server.with_semantics(document, |analyzer| {
            analyzer.scopes_surrounding(range.start(), |scope| {
                for (name, local) in &scope.locals {
                    self.completions.push(CompletionItem {
                        label: name.to_string(),
                        label_details: Some(CompletionItemLabelDetails {
                            detail: Some(local.typ.to_string()),
                            description: Some(local.typ.to_string()),
                        }),
                        kind: Some(CompletionItemKind::VARIABLE),
                        documentation: None,
                        ..Default::default()
                    });
                }
            });

            Ok(())
        }).await
    }

    async fn complete_method(&mut self, last_identifier: FileRange) -> Result<()> {
        let document = &self.params.text_document_position.text_document;

        self.server.with_semantics(document, |analyzer| {
            if let Some(typ) = analyzer.context.definition_tracker.as_ref().and_then(|tracker| Some(tracker.get(&last_identifier)?.typ.clone())) {
                match typ {
                    SemanticType::Builtin(builtin) => {
                        for method in builtin.methods() {
                            self.completions.push(CompletionItem {
                                label: method.lsp_label(),
                                detail: Some(method.inline_detail.to_string()),
                                kind: Some(CompletionItemKind::METHOD),
                                documentation: Some(Documentation::MarkupContent(MarkupContent {
                                    kind: MarkupKind::Markdown,
                                    value: method.documentation.to_string(),
                                })),
                                insert_text: Some(method.lsp_completion().into_owned()),
                                insert_text_format: Some(InsertTextFormat::SNIPPET),
                                ..Default::default()
                            });
                        }
                        return Ok(());
                    }
                    _ => (),
                }
            }

            Ok(())
        }).await
    }

    fn suggest_identifiers(&mut self, ident: &str) {
        let ident = ident.to_lowercase();
        let ident = &ident;

        for keyword in Keyword::iter_variants() {
            if keyword.as_ref().to_lowercase().starts_with(ident) {
                let lsp = keyword.lsp_completion();
                self.completions.push(CompletionItem {
                    label: keyword.as_ref().to_string(),
                    label_details: Some(CompletionItemLabelDetails {
                        detail: Some("Details??".into()),
                        description: Some("Description??".into()),
                    }),
                    kind: Some(CompletionItemKind::KEYWORD),
                    insert_text: lsp.map(|x| x.completion.to_string()),
                    detail: lsp.map(|x| x.inline_detail.to_string()),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    ..Default::default()
                });
            }
        }

        for ty in Builtin::TYPES {
            if ty.name().to_lowercase().starts_with(ident) {
                self.completions.push(CompletionItem {
                    label: ty.name().to_string(),
                    label_details: Some(CompletionItemLabelDetails {
                        detail: Some("Details??".into()),
                        description: Some("Description??".into()),
                    }),
                    kind: Some(CompletionItemKind::KEYWORD),
                    detail: Some(ty.documentation().to_string()),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    ..Default::default()
                });
            }
        }
    }
}

#[derive(Debug)]
enum CompletionMode {
    Method((FileRange, String)),
    Function((FileRange, String)),
}
