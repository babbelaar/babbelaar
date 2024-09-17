// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::rc::Rc;

use babbelaar::*;
use log::warn;
use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind, CompletionItemLabelDetails, CompletionParams, CompletionResponse, Documentation, InsertTextFormat, MarkupContent, MarkupKind};

use crate::{Backend, BabbelaarLspResult as Result};

pub struct CompletionEngine<'b> {
    server: &'b Backend,
    params: CompletionParams,
    completions: Vec<CompletionItem>,
    support_snippets: bool,
}

impl<'b> CompletionEngine<'b> {
    pub async fn complete(server: &'b Backend, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let mut this = Self {
            server,
            params,
            completions: Vec::new(),
            support_snippets: check_if_snippets_are_supported_by_the_client(server).await,
        };

        if !this.do_completions().await? {
            return Ok(None);
        }

        if !this.support_snippets {
            this.ensure_format_of_snippets();
        }

        Ok(Some(CompletionResponse::Array(this.completions)))
    }

    async fn do_completions(&mut self) -> Result<bool> {
        let mode = self.detect_completion_mode().await?;

        match mode {
            Some(CompletionMode::FieldInstantiation { range, field, structure, new_line }) => {
                self.complete_field_instantiation(range, field, structure, new_line).await?;
                Ok(true)
            }

            Some(CompletionMode::Structure { range, name }) => {
                self.complete_structure_name(range, name).await?;
                Ok(true)
            }

            Some(CompletionMode::Function((range, ident))) => {
                self.suggest_this(range, &ident).await?;
                self.complete_global_function(&ident).await?;
                self.complete_variable(range).await?;
                self.suggest_identifiers(&ident);
                Ok(true)
            }

            Some(CompletionMode::Method(last_identifier)) => {
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
            let last_keyword = previous.iter().enumerate().rev()
                .filter_map(|(idx, token)| {
                    if let TokenKind::Keyword(kw) = token.kind {
                        Some((idx, kw))
                    } else {
                        None
                    }
                })
                .next();

            let last_punctuator = previous.iter().rev()
                .filter_map(|token| {
                    if let TokenKind::Punctuator(punctuator) = token.kind {
                        Some(punctuator)
                    } else {
                        None
                    }
                })
                .next();

            // TODO: this should be replaced by looking at the syntactic tree structure.
            if let TokenKind::Identifier(ident) = &token.kind {
                if previous.last().is_some_and(|last| last.kind == TokenKind::Keyword(Keyword::Nieuw)) {
                    return Ok(Some(CompletionMode::Structure {
                        range: token.range(),
                        name: ident.to_string(),
                    }));
                }

                if let Some((keyword_idx, last_keyword)) = last_keyword {
                    if last_keyword == Keyword::Nieuw {
                        if let Some(TokenKind::Identifier(struct_ident)) = previous.get(keyword_idx + 1).map(|x| &x.kind) {
                            if last_punctuator == Some(Punctuator::Comma) || last_punctuator == Some(Punctuator::LeftCurlyBracket) {
                                return Ok(Some(CompletionMode::FieldInstantiation {
                                    range: token.range(),
                                    field: ident.to_string(),
                                    structure: struct_ident.to_string(),
                                    new_line: false,
                                }));
                            }
                        }
                    }
                }
            }

            if let TokenKind::Punctuator(Punctuator::Comma) = &token.kind {
                if let Some((keyword_idx, last_keyword)) = last_keyword {
                    if last_keyword == Keyword::Nieuw {
                        if let Some(TokenKind::Identifier(struct_ident)) = previous.get(keyword_idx + 1).map(|x| &x.kind) {
                            return Ok(Some(CompletionMode::FieldInstantiation {
                                range: token.range(),
                                field: String::new(),
                                structure: struct_ident.to_string(),
                                new_line: true,
                            }));
                        }
                    }
                }
            }

            was_new_func = previous.last().is_some_and(|tok| matches!(tok.kind, TokenKind::Keyword(Keyword::Werkwijze)));
            prev_punc = previous.last().and_then(|x| if let TokenKind::Punctuator(punc) = x.kind { Some(punc) } else { None });

            if prev_punc == Some(Punctuator::Period) {
                if let Some(token) = previous.get(previous.len() - 2) {
                    if token.kind.can_be_variable() {
                        return Ok(Some(CompletionMode::Method(token.range())));
                    }
                }
            }

            if token.kind == TokenKind::Punctuator(Punctuator::Period) {
                if let Some(token) = previous.last() {
                    if token.kind.can_be_variable()  {
                        return Ok(Some(CompletionMode::Method(token.range())));
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
                    insert_text: Some(func.lsp_completion().to_string()),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    ..Default::default()
                });
            }

            Ok(())
        }).await
    }

    async fn complete_structure_name(&mut self, range: FileRange, structure_to_complete: String) -> Result<()> {
        let document = &self.params.text_document_position.text_document;

        self.server.with_semantics(document, |analyzer| {
            analyzer.scopes_surrounding(range.start(), |scope| {
                for (name, _) in &scope.structures {
                    if let Some(idx) = name.find(&structure_to_complete) {
                        self.completions.push(CompletionItem {
                            label: name.to_string(),
                            filter_text: Some(name[idx..].to_string()),
                            kind: Some(CompletionItemKind::FIELD),
                            documentation: None,
                            ..Default::default()
                        })
                    }
                }
            });

            Ok(())
        }).await
    }

    async fn complete_field_instantiation(&mut self, range: FileRange, field_to_complete: String, structure: impl Into<BabString>, new_line: bool) -> Result<()> {
        let structure = structure.into();
        let document = &self.params.text_document_position.text_document;

        self.server.with_semantics(document, |analyzer| {
            analyzer.scopes_surrounding(range.start(), |scope| {
                if let Some(structure) = scope.structures.get(&structure) {
                    for field in &structure.fields {
                        if let Some(idx) = field.name.find(&field_to_complete) {
                            let value_hint = field.ty.value_or_field_name_hint();
                            let new_line = if new_line { "\n" } else { "" };
                            self.completions.push(CompletionItem {
                                label: field.name.to_string(),
                                label_details: Some(CompletionItemLabelDetails {
                                    detail: Some(field.ty.to_string()),
                                    description: Some(field.ty.to_string()),
                                }),
                                filter_text: Some(field.name[idx..].to_string()),
                                insert_text: Some(format!("{new_line}{}: ${{1:{value_hint}}},$0", field.name.value())),
                                detail: Some(field.ty.to_string()),
                                insert_text_format: Some(InsertTextFormat::SNIPPET),
                                kind: Some(CompletionItemKind::FIELD),
                                documentation: None,
                                ..Default::default()
                            })
                        }
                    }
                }
            });

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

        let completions = self.server.with_semantics(document, |analyzer| {
            let completions = if let Some(typ) = analyzer.context.definition_tracker.as_ref().and_then(|tracker| Some(tracker.get(&last_identifier)?.typ.clone())) {
                match typ {
                    SemanticType::Builtin(builtin) => {
                        self.complete_builtin_type(builtin)
                    }

                    SemanticType::Custom(custom) => {
                        self.complete_structure_method_or_field(custom.clone(), "")
                    }

                    _ => Vec::new(),
                }
            } else {
                Vec::new()
            };

            Ok(completions)
        }).await?;

        self.completions.extend(completions);
        Ok(())
    }

    fn complete_builtin_type(&self, builtin: BuiltinType) -> Vec<CompletionItem> {
        let mut completions = Vec::new();

        for method in builtin.methods() {
            completions.push(CompletionItem {
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

        completions
    }

    fn complete_structure_method_or_field(&self, structure: Rc<SemanticStructure>, prefix: &str) -> Vec<CompletionItem> {
        let mut completions = Vec::new();

        for method in &structure.methods {
            let name = &method.function.name;
            completions.push(CompletionItem {
                label: format!("{prefix}{}()", name.value()),
                kind: Some(CompletionItemKind::METHOD),
                insert_text: Some(format!("{prefix}{}($1);$0", name.value())),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                preselect: Some(true),
                ..Default::default()
            });
        }

        for field in &structure.fields {
            completions.push(CompletionItem {
                label: format!("{prefix}{}", field.name.to_string()),
                label_details: Some(CompletionItemLabelDetails {
                    description: Some(field.ty.to_string()),
                    detail: None,
                }),
                preselect: Some(true),
                detail: Some(field.ty.to_string()),
                kind: Some(CompletionItemKind::FIELD),
                documentation: None,
                ..Default::default()
            });
        }

        completions
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
                    kind: match keyword {
                        Keyword::Dit => Some(CompletionItemKind::VARIABLE),
                        _ => Some(CompletionItemKind::KEYWORD)
                    },
                    preselect: Some(true),
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
                    preselect: Some(true),
                    kind: Some(CompletionItemKind::KEYWORD),
                    detail: Some(ty.documentation().to_string()),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    ..Default::default()
                });
            }
        }

    }

    async fn suggest_this(&mut self, range: FileRange, ident: &str) -> Result<()> {
        if !Keyword::Dit.as_ref().contains(&ident.to_lowercase()) {
            return Ok(());
        }

        let document = &self.params.text_document_position.text_document;

        let completions = self.server.with_semantics(document, |semantics| {
            let mut completions = Vec::new();

            semantics.scopes_surrounding(range.start(), |scope| {
                if let Some(this) = &scope.this {
                    if let SemanticType::Custom(structure) = this {
                        completions.extend(self.complete_structure_method_or_field(structure.clone(), "dit."));
                    }
                }
            });

            Ok(completions)
        }).await?;

        self.completions.extend(completions);
        Ok(())
    }

    fn ensure_format_of_snippets(&mut self) {
        assert!(!self.support_snippets);

        for item in &mut self.completions {
            let Some(insert_text) = &mut item.insert_text else { continue };

            item.insert_text_format = Some(InsertTextFormat::PLAIN_TEXT);

            while let Some(dollar) = insert_text.find('$') {
                let text = &insert_text[dollar..];

                let Some((next_offset, next)) = text.char_indices().nth(1) else { continue };
                let end_index = if next.is_ascii_digit() {
                    dollar + next_offset + next.len_utf8()
                } else if next == '{' {
                    match text.find('}') {
                        Some(pos) => dollar + pos + 1,
                        None => {
                            warn!("Geopende accolade gevonden, maar geen gesloten: '{text}'");
                            break;
                        }
                    }
                } else {
                    warn!("Failed to fix-up snippet because a weird $ continuation: '{text}'");
                    break;
                };

                let mut new_text = insert_text[..dollar].to_string();
                if end_index < insert_text.len() {
                    new_text += &insert_text[end_index..];
                }

                *insert_text = new_text;
            }
        }
    }
}

#[must_use]
async fn check_if_snippets_are_supported_by_the_client(server: &Backend) -> bool {
    check_if_snippets_are_supported_by_the_client_inner(server).await.unwrap_or(false)
}

#[must_use]
async fn check_if_snippets_are_supported_by_the_client_inner(server: &Backend) -> Option<bool> {
    let caps = server.client_configuration.read().await;
    let caps = caps.as_ref()?;


    caps.capabilities
        .text_document.as_ref()?
        .completion.as_ref()?
        .completion_item.as_ref()?
        .snippet_support
}

#[derive(Debug)]
enum CompletionMode {
    Method(FileRange),
    Function((FileRange, String)),
    FieldInstantiation {
        range: FileRange,
        field: String,
        structure: String,
        new_line: bool,
    },
    Structure { range: FileRange, name: String },
}
