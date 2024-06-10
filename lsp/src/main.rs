// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

mod conversion;
mod symbolization;

use std::collections::HashMap;

use babbelaar::{Builtin, DocumentationProvider, Keyword, Parser, Token, TokenKind};
use conversion::convert_token_range;
use log::{info, LevelFilter, Log};
use symbolization::{LspTokenType, Symbolizer};
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::{Error, Result};
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct Backend {
    client: Client,
    file_store: Mutex<HashMap<Url, String>>,
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
                trigger_characters: None,
                retrigger_characters: None,
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
            // workspace_symbol_provider: Some(OneOf::Left(true)),
            // definition_provider: Some(OneOf::Left(true)),
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
        // self.client.log_message(MessageType::INFO, format!("Symbols: {response:#?}")).await;
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

    async fn initialized(&self, _: InitializedParams) {
        info!("Initialized");
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        _ = self.with_contents(&params.text_document.uri, |_| { Ok(()) }).await.ok();
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        info!("Change: {params:#?}");
        let mut file_store = self.file_store.lock().await;
        file_store.insert(params.text_document.uri, std::mem::take(&mut params.content_changes[0].text));
        info!("DoneChange");
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
        info!("Completion: {params:#?}");
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

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

fn suggest_identifiers(ident: &str, completions: &mut Vec<CompletionItem>) {
    let ident = ident.to_lowercase();
    let ident = &ident;

    for keyword in Keyword::iter_variants() {
        if keyword.as_ref().to_lowercase().starts_with(ident) {
            completions.push(CompletionItem {
                label: keyword.as_ref().to_string(),
                label_details: Some(CompletionItemLabelDetails {
                    detail: Some("Details??".into()),
                    description: Some("Description??".into()),
                }),
                kind: Some(CompletionItemKind::KEYWORD),
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
        Backend { client, file_store: Mutex::new(HashMap::new()) }
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
