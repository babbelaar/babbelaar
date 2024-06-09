use std::collections::HashMap;

use babbelaar::{Builtin, FileLocation, Keyword, Token, TokenKind};
use log::{info, LevelFilter, Log};
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
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "Babbelaar Taalserveerder".to_string(),
                version: None,
            }),
            capabilities: ServerCapabilities {
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
                // workspace_symbol_provider: Some(OneOf::Left(true)),
                // definition_provider: Some(OneOf::Left(true)),
                ..ServerCapabilities::default()
            },
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
        self.lexed_document(&params.text_document, |tokens| {
            let mut symbols = Vec::new();

            for token in tokens {
                convert_token_to_semantic_symbol(&params.text_document.uri, token, &mut symbols);
            }

            Ok(Some(DocumentSymbolResponse::Flat(symbols)))
        })
        .await
    }

    async fn document_highlight(&self, _params: DocumentHighlightParams) -> Result<Option<Vec<DocumentHighlight>>> {
        let highlights = Vec::new();
        Ok(Some(highlights))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        self.client
            .log_message(MessageType::INFO, format!("Hover: {params:#?}"))
            .await;
        Ok(Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: "Babbelaar".into(),
            }),
            range: None,
        }))
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        info!("Completion: {params:#?}");
        self.lexed_document(&params.text_document_position.text_document, |tokens| {
            let mut completions = Vec::new();

            for token in tokens {
                if token.begin.line() != params.text_document_position.position.line as usize {
                    continue;
                }

                if token.begin.column() > params.text_document_position.position.character as usize {
                    continue;
                }

                if token.end.column() > params.text_document_position.position.character as usize {
                    continue;
                }

                if let TokenKind::Identifier(ident) = &token.kind {
                    suggest_identifiers(ident, &mut completions);
                }
            }
            Ok(Some(CompletionResponse::Array(completions)))
        })
        .await
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

fn convert_token_to_semantic_symbol(uri: &Url, token: Token, symbols: &mut Vec<SymbolInformation>) {
    #[allow(deprecated)]
    symbols.push(SymbolInformation {
        name: token.kind.name().to_string(),
        kind: convert_token_kind(&token.kind),
        tags: None,
        deprecated: None,
        location: convert_token_location(uri.clone(), &token),
        container_name: None,
    })
}

fn convert_token_kind(kind: &TokenKind) -> SymbolKind {
    match kind {
        TokenKind::Keyword(..) => SymbolKind::KEY,
        TokenKind::StringLiteral(..) => SymbolKind::STRING,
        TokenKind::TemplateString(..) => SymbolKind::STRING,
        TokenKind::Identifier(..) => SymbolKind::VARIABLE,
        TokenKind::Integer(..) => SymbolKind::NUMBER,

        TokenKind::Comma => SymbolKind::OPERATOR,
        TokenKind::LeftParenthesis => SymbolKind::OPERATOR,
        TokenKind::RightParenthesis => SymbolKind::OPERATOR,
        TokenKind::LeftCurlyBracket => SymbolKind::OPERATOR,
        TokenKind::RightCurlyBracket => SymbolKind::OPERATOR,
        TokenKind::LeftSquareBracket => SymbolKind::OPERATOR,
        TokenKind::RightSquareBracket => SymbolKind::OPERATOR,
        TokenKind::Semicolon => SymbolKind::OPERATOR,
        TokenKind::PlusSign => SymbolKind::OPERATOR,
        TokenKind::EqualsSign => SymbolKind::OPERATOR,
        TokenKind::HyphenMinus => SymbolKind::OPERATOR,
        TokenKind::Solidus => SymbolKind::OPERATOR,
        TokenKind::Asterisk => SymbolKind::OPERATOR,
        TokenKind::PercentageSign => SymbolKind::OPERATOR,
    }
}

fn convert_token_location(uri: Url, token: &Token) -> Location {
    Location {
        uri,
        range: Range {
            start: convert_position(token.begin),
            end: convert_position(token.end),
        }
    }
}

fn convert_position(location: FileLocation) -> Position {
    Position {
        line: location.line() as _,
        character: location.column() as _,
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
