// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

mod conversion;
mod symbolization;

use std::collections::HashMap;
use std::sync::Arc;

use babbelaar::{Builtin, DocumentationProvider, Expression, Keyword, Parser, StatementKind, Token, TokenKind};
use conversion::{convert_position, convert_token_range};
use log::{info, LevelFilter, Log};
use symbolization::{LspTokenType, Symbolizer};
use tokio::spawn;
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::{Error, Result};
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug, Clone)]
struct Backend {
    client: Client,
    file_store: Arc<Mutex<HashMap<Url, String>>>,
}

impl Backend {
    async fn find_tokens_at<F>(&self, params: &TextDocumentPositionParams, mut f: F) -> Result<()>
            where F: FnMut(Token) -> Result<()> {
        self.lexed_document(&params.text_document, |tokens| {
            for token in tokens {
                if token.begin.line() != params.position.line as usize {
                    continue;
                }

                if token.begin.column() > params.position.character as usize {
                    continue;
                }

                if params.position.character as usize > token.end.column(){
                    continue;
                }

                f(token)?;
            }

            Ok(())
        })
        .await
    }

    async fn collect_lenses(&self, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
        let url = params.text_document.uri.to_string();
        Ok(Some(vec![
            CodeLens {
                range: Range {
                    start: Position {
                        line: 0,
                        character: 0,
                    },
                    end: Position {
                        line: 0,
                        character: 1,
                    },
                },
                command: Some(Command {
                    title: "► Uitvoeren".into(),
                    command: "babbelaar.uitvoeren".into(),
                    arguments: Some(vec![
                        url.into(),
                    ]),
                }),
                data: None,
            }
        ]))
    }

    async fn lexed_document<F, R>(&self, text_document: &TextDocumentIdentifier, f: F) -> Result<R>
    where
        F: FnOnce(Vec<Token>) -> Result<R>,
    {
        self.with_contents(&text_document.uri, |contents| {
            let tokens = babbelaar::Lexer::new(&contents).collect();
            f(tokens)
        }).await
    }

    async fn with_contents<F, R>(&self, uri: &Url, f: F) -> Result<R>
    where
        F: FnOnce(&str) -> Result<R>,
    {
        let mut store = self.file_store.lock().await;
        if let Some(contents) = store.get(&uri) {
            return f(contents);
        }

        let path = uri.path();
        let Ok(contents) = tokio::fs::read_to_string(path).await else {
            self.client
                .log_message(MessageType::INFO, "Failed to read path!")
                .await;
            return Err(Error::internal_error());
        };
        let result = f(&contents);
        store.insert(uri.clone(), contents);
        result
    }

    async fn collect_diagnostics(&self, document: VersionedTextDocumentIdentifier) {
        let text_document = TextDocumentIdentifier {
            uri: document.uri,
        };

        let diags = self.lexed_document(&text_document, |tokens| {
            let mut parser = Parser::new(&tokens).attempt_to_ignore_errors();

            loop {
                match parser.parse_statement() {
                    Ok(_) => (),
                    Err(e) => {
                        parser.errors.push(e);
                        break;
                    }
                }
            }

            let diags = parser.errors
                .into_iter()
                .flat_map(|err| {
                    let token = err.token()?;

                    Some(Diagnostic {
                        range: convert_token_range(token),
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: Some(NumberOrString::String(err.name().to_string())),
                        code_description: None,
                        source: None,
                        message: err.to_string(),
                        related_information: None,
                        tags: None,
                        data: None,
                    })
                })
                .collect();

            Ok(diags)
        }).await.unwrap();

        self.client.publish_diagnostics(text_document.uri, diags, Some(document.version)).await;
    }

    async fn collect_signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let caret_line = params.text_document_position_params.position.line as usize;
        let caret_column = params.text_document_position_params.position.character as usize;

        self.lexed_document(&params.text_document_position_params.text_document, |tokens| {
            let mut parser = Parser::new(&tokens).attempt_to_ignore_errors();
            info!("Caret is @ {caret_line}:{caret_column}");
            loop {
                let res = parser.parse_statement();

                let Ok(statement) = res else {
                    info!("Error: {res:#?}");
                    break;
                };
                info!("Statement: {statement:#?}");
                if statement.range.end().line() < caret_line {
                    continue;
                }

                if statement.range.end().column() < caret_column {
                    continue;
                }

                let StatementKind::Expression(expr) = statement.kind else {
                    info!("No expression: {statement:?}");
                    return Ok(None);
                };

                let Expression::Function(func) = expr else {
                    info!("No func: {expr:?}");
                    return Ok(None);
                };

                let Some(builtin_function) = Builtin::FUNCTIONS.iter().find(|x| x.name == *func.function_identifier) else {
                    info!("No builtin: {func:?}");
                    return Ok(None);
                };

                return Ok(Some(SignatureHelp {
                    signatures: vec![
                        SignatureInformation {
                            label: format!("{}()", builtin_function.name),
                            documentation: Some(Documentation::MarkupContent(MarkupContent {
                                kind: MarkupKind::Markdown,
                                value: builtin_function.documentation.to_string(),
                            })),
                            parameters: None,
                            active_parameter: None,
                        }
                    ],
                    active_signature: Some(0),
                    active_parameter: None,
                }));
            }

            let client = self.client.clone();
            let total = tokens.len();
            let cursor = parser.cursor;
            spawn(async move {
                client.log_message(MessageType::INFO, format!("Parser is @ {cursor} of {total}")).await
            });
            Ok(None)
        }).await
    }

    fn capabilities(&self) -> ServerCapabilities {
        ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(
                TextDocumentSyncKind::FULL,
            )),
            completion_provider: Some(CompletionOptions {
                resolve_provider: Some(true),
                trigger_characters: Some(vec![".".to_string()]),
                work_done_progress_options: WorkDoneProgressOptions::default(),
                ..Default::default()
            }),
            signature_help_provider: Some(SignatureHelpOptions {
                trigger_characters: Some(vec!["(".into(), ",".into()]),
                retrigger_characters: Some(vec![",".into()]),
                work_done_progress_options: WorkDoneProgressOptions::default(),
            }),
            hover_provider: Some(true.into()),
            document_formatting_provider: Some(OneOf::Left(true)),
            document_highlight_provider: Some(OneOf::Left(true)),
            document_symbol_provider: Some(OneOf::Left(true)),
            semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
                SemanticTokensOptions {
                    work_done_progress_options: WorkDoneProgressOptions {
                        work_done_progress: None,
                    },
                    legend: SemanticTokensLegend {
                        token_types: LspTokenType::legend(),
                        token_modifiers: Vec::new(),
                    },
                    range: Some(false),
                    full: Some(SemanticTokensFullOptions::Bool(true)),
                }
            )),
            code_lens_provider: Some(CodeLensOptions {
                resolve_provider: Some(true),
            }),
            inlay_hint_provider: Some(OneOf::Left(true)),
            ..ServerCapabilities::default()
        }
    }

    async fn on_semantic_tokens_full(&self, params: SemanticTokensParams) -> Result<Option<SemanticTokensResult>> {
        let mut symbolizer = Symbolizer::new(params.text_document.uri.clone());

        self.lexed_document(&params.text_document, |tokens| {
            for token in &tokens {
                symbolizer.add_token(token);
            }

            let mut parser = Parser::new(&tokens).attempt_to_ignore_errors();
            while let Ok(statement) = parser.parse_statement() {
                symbolizer.add_statement(&statement);
            }

            Ok(())
        }).await?;

        let tokens = symbolizer.to_tokens();
        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: tokens,
        })))
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "Babbelaar Taalserveerder".to_string(),
                version: None,
            }),
            capabilities: self.capabilities(),
        })
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        self.collect_signature_help(params).await
    }

    async fn code_lens(&self, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
        self.collect_lenses(params).await
    }

    async fn code_lens_resolve(&self, params: CodeLens) -> Result<CodeLens> {
        Ok(params)
    }

    async fn initialized(&self, _: InitializedParams) {
        info!("Initialized");
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        _ = self.with_contents(&params.text_document.uri, |_| { Ok(()) }).await.ok();
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        info!("Change: {params:#?}");
        let mut file_store = self.file_store.lock().await;
        file_store.insert(params.text_document.uri.clone(), std::mem::take(&mut params.content_changes[0].text));
        let this = (*self).clone();
        spawn(async move {
            let this = this;
            this.collect_diagnostics(params.text_document).await;
        });
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let mut symbolizer = Symbolizer::new(params.text_document.uri.clone());

        self.lexed_document(&params.text_document, |tokens| {
            for token in &tokens {
                symbolizer.add_token(token);
            }

            let mut parser = Parser::new(&tokens).attempt_to_ignore_errors();
            while let Ok(statement) = parser.parse_statement() {
                symbolizer.add_statement(&statement);
            }

            Ok(())
        }).await?;

        let response = symbolizer.to_response();
        // self.client.log_message(MessageType::INFO, format!("Symbols: {response:#?}")).await;
        Ok(Some(response))
    }

    async fn semantic_tokens_full(&self, params: SemanticTokensParams) -> Result<Option<SemanticTokensResult>> {
        self.on_semantic_tokens_full(params).await
    }

    async fn document_highlight(&self, _params: DocumentHighlightParams) -> Result<Option<Vec<DocumentHighlight>>> {
        let highlights = Vec::new();
        Ok(Some(highlights))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let mut hover = None;

        self.find_tokens_at(&params.text_document_position_params, |token| {
            match &token.kind {
                TokenKind::Identifier(ident) => {
                    if let Some(builtin_function) = Builtin::FUNCTIONS.iter().find(|x| x.name == *ident) {
                        hover = Some(Hover {
                            contents: HoverContents::Markup(MarkupContent {
                                kind: MarkupKind::Markdown,
                                value: builtin_function.documentation.to_string(),
                            }),
                            range: Some(convert_token_range(&token)),
                        });
                    }
                }

                TokenKind::Keyword(keyword) => {
                    hover = Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: keyword.provide_documentation().into_owned(),
                        }),
                        range: Some(convert_token_range(&token)),
                    });
                }

                _ => (),
            }

            Ok(())
        }).await?;

        Ok(hover)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let mut completions = Vec::new();

        self.find_tokens_at(&params.text_document_position, |token| {
            if let TokenKind::Identifier(ident) = &token.kind {
                suggest_identifiers(ident, &mut completions);
            }

            Ok(())
        }).await?;

        Ok(Some(CompletionResponse::Array(completions)))
    }

    async fn completion_resolve(&self, params: CompletionItem) -> Result<CompletionItem> {
        Ok(params)
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let mut hints = Vec::new();

        self.lexed_document(&params.text_document, |tokens| {
            let mut parser = Parser::new(&tokens).attempt_to_ignore_errors();
            while let Ok(statement) = parser.parse_statement() {
                match statement.kind {
                    StatementKind::For(statement) => {
                        hints.push(InlayHint {
                            position: convert_position(statement.iterator_name.range().end()),
                            label: InlayHintLabel::String(": G32".into()),
                            kind: Some(InlayHintKind::TYPE),
                            text_edits: None,
                            tooltip: None,
                            padding_left: None,
                            padding_right: None,
                            data: None,
                        });
                    }

                    _ => (),
                }
            }

            Ok(())
        }).await?;

        self.client.show_message(MessageType::INFO, format!("Hints: {hints:#?}")).await;
        Ok(Some(hints))
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

fn suggest_identifiers(ident: &str, completions: &mut Vec<CompletionItem>) {
    let ident = ident.to_lowercase();
    let ident = &ident;

    for keyword in Keyword::iter_variants() {
        if keyword.as_ref().to_lowercase().starts_with(ident) {
            let lsp = keyword.lsp_completion();
            completions.push(CompletionItem {
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

    for builtin_function in Builtin::FUNCTIONS {
        if builtin_function.name.starts_with(ident) {
            completions.push(CompletionItem {
                label: builtin_function.name.to_string(),
                kind: Some(CompletionItemKind::FUNCTION),
                documentation: Some(Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: builtin_function.documentation.to_string(),
                })),
                insert_text: match builtin_function.lsp_completion {
                    Some(completion) => Some(completion.to_string()),
                    None => Some(format!("{}($1)$0", builtin_function.name)),
                },
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                ..Default::default()
            });
        }
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    log::set_max_level(LevelFilter::Trace);


    let (service, socket) = LspService::new(|client| {
        log::set_logger(Box::leak(Box::new(Logger {
            client: client.clone(),
        })))
        .unwrap();
        Backend { client, file_store: Arc::new(Mutex::new(HashMap::new())) }
    });

    Server::new(stdin, stdout, socket).serve(service).await;
}

struct Logger {
    client: Client,
}

impl Log for Logger {
    fn enabled(&self, metadata: &log::Metadata) -> bool {
        _ = metadata;
        log::set_max_level(LevelFilter::Trace);
        true
    }

    fn log(&self, record: &log::Record) {
        let typ = match record.level() {
            log::Level::Error => MessageType::ERROR,
            log::Level::Warn => MessageType::WARNING,
            log::Level::Info => MessageType::INFO,
            log::Level::Debug => MessageType::INFO,
            log::Level::Trace => MessageType::INFO,
        };
        let client = self.client.clone();
        let message = format!("{}: {}", record.file().unwrap_or_default(), record.args());
        tokio::task::spawn(async move {
            client.log_message(typ, message).await;
        });
    }

    fn flush(&self) {}
}
