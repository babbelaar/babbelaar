// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

mod completions;
mod conversion;
mod format;
mod symbolization;

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use babbelaar::*;
use completions::CompletionEngine;
use conversion::{convert_file_range, convert_position, convert_token_range};
use format::Format;
use log::{info, LevelFilter, Log};
use symbolization::{LspTokenType, Symbolizer};
use tokio::spawn;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::{Error, Result};
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug, Clone)]
struct Backend {
    client: Client,
    client_configuration: Arc<RwLock<Option<InitializeParams>>>,
    file_store: Arc<RwLock<HashMap<Url, Arc<str>>>>,
}

impl Backend {
    async fn find_tokens_at<F, R>(&self, params: &TextDocumentPositionParams, mut f: F) -> Result<Option<R>>
            where F: FnMut(Token, Vec<Token>) -> Result<Option<R>> {
        self.lexed_document(&params.text_document, |tokens, _| {
            let mut previous = Vec::new();
            for token in tokens {
                if token.begin.line() != params.position.line as usize {
                    previous.push(token);
                    continue;
                }

                if token.begin.column() > params.position.character as usize {
                    previous.push(token);
                    continue;
                }

                if params.position.character as usize > token.end.column() {
                    previous.push(token);
                    continue;
                }

                return f(token, previous);
            }

            Ok(None)
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
                        "uitvoeren".into(),
                        url.into(),
                    ]),
                }),
                data: None,
            }
        ]))
    }

    async fn lexed_document<F, R>(&self, text_document: &TextDocumentIdentifier, f: F) -> Result<R>
    where
        F: FnOnce(Vec<Token>, &str) -> Result<R>,
    {
        self.with_contents(&text_document.uri, |contents| {
            let tokens = babbelaar::Lexer::new(&contents).collect();
            f(tokens, contents)
        }).await
    }

    async fn with_contents<F, R>(&self, uri: &Url, f: F) -> Result<R>
    where
        F: FnOnce(&str) -> Result<R>,
    {
        let contents = self.file_store.read().await.get(&uri).cloned();
        if let Some(contents) = contents {
            return f(&contents);
        }

        let path = uri.to_file_path().unwrap();
        let contents = match tokio::fs::read_to_string(&path).await {
            Ok(contents) => contents,
            Err(e) => {
                self.client
                    .log_message(MessageType::INFO, format!("Failed to read path \"{}\" {e}", path.display()))
                    .await;
                return Err(Error::internal_error());
            }
        };

        let contents = Arc::from(contents);
        {
            let mut store = self.file_store.write().await;
            store.insert(uri.clone(), Arc::clone(&contents));
        }

        let result = f(&contents);
        result
    }

    async fn with_semantics<F, R>(&self, text_document: &TextDocumentIdentifier, f: F) -> Result<R>
    where
        F: FnOnce(SemanticAnalyzer) -> Result<R>,
    {
        self.lexed_document(text_document, |tokens, _| {
            let mut parser = Parser::new(text_document.uri.to_file_path().unwrap(), &tokens).attempt_to_ignore_errors();
            let tree = parser.parse_tree().expect("Failed to parse tree");

            let mut analyzer = SemanticAnalyzer::new();
            analyzer.analyze_tree(&tree);

            f(analyzer)
        }).await
    }

    async fn collect_diagnostics(&self, document: VersionedTextDocumentIdentifier) {
        let path = document.uri.to_file_path().unwrap();

        let text_document = TextDocumentIdentifier {
            uri: document.uri,
        };

        let diags = self.lexed_document(&text_document, |tokens, _| {
            let mut parser = Parser::new(path, &tokens).attempt_to_ignore_errors();

            let tree = match parser.parse_tree() {
                Ok(tree) => tree,
                Err(e) => {
                    parser.errors.push(e);
                    ParseTree::new(PathBuf::default())
                }
            };

            let mut diags = Vec::new();

            let parse_errors = parser.errors
                .into_iter()
                .flat_map(|err| {
                    Some(Diagnostic {
                        range: convert_file_range(err.range()?),
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: Some(NumberOrString::String(err.name().to_string())),
                        code_description: None,
                        source: None,
                        message: err.to_string(),
                        related_information: None,
                        tags: None,
                        data: None,
                    })
                });

            diags.extend(parse_errors);

            let mut analyzer = SemanticAnalyzer::new();
            analyzer.analyze_tree(&tree);

            diags.extend(
                analyzer.into_diagnostics()
                .into_iter()
                .map(|e| {
                    Diagnostic {
                        range: convert_file_range(e.range),
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: Some(NumberOrString::String(e.kind.name().to_string())),
                        code_description: None,
                        source: None,
                        message: e.kind.to_string(),
                        related_information: None,
                        tags: None,
                        data: None,
                    }
                })
            );

            Ok(diags)
        }).await.unwrap();

        self.client.publish_diagnostics(text_document.uri, diags, Some(document.version)).await;
    }

    async fn collect_signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let caret_line = params.text_document_position_params.position.line as usize;
        let caret_column = params.text_document_position_params.position.character as usize;

        self.lexed_document(&params.text_document_position_params.text_document, |tokens, _| {
            let mut parser = Parser::new(params.text_document_position_params.text_document.uri.to_file_path().unwrap(), &tokens).attempt_to_ignore_errors();
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
                    return Ok(None);
                };

                let Expression::Postfix(postfix) = expr.as_ref() else {
                    return Ok(None);
                };

                let PostfixExpressionKind::Call(..) = &postfix.kind else {
                    return Ok(None);
                };

                let Expression::Primary(PrimaryExpression::Reference(calling_name)) = postfix.lhs.value() else {
                    return Ok(None);
                };

                let Some(builtin_function) = Builtin::FUNCTIONS.iter().find(|x| &x.name == calling_name.value()) else {
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
                resolve_provider: Some(false),
                trigger_characters: Some(vec![".".to_string()]),
                work_done_progress_options: WorkDoneProgressOptions::default(),
                completion_item: Some(CompletionOptionsCompletionItem {
                    label_details_support: Some(true),
                }),
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
            inlay_hint_provider: Some(OneOf::Right(InlayHintServerCapabilities::Options(InlayHintOptions {
                work_done_progress_options: WorkDoneProgressOptions {
                    work_done_progress: None,
                },
                resolve_provider: Some(true),
            }))),
            declaration_provider: Some(DeclarationCapability::Options(DeclarationOptions {
                work_done_progress_options: WorkDoneProgressOptions {
                    work_done_progress: None,
                },
            })),
            definition_provider: Some(OneOf::Left(true)),
            ..ServerCapabilities::default()
        }
    }

    async fn on_semantic_tokens_full(&self, params: SemanticTokensParams) -> Result<Option<SemanticTokensResult>> {
        self.lexed_document(&params.text_document, |tokens, _| {
            let mut symbolizer = Symbolizer::new(params.text_document.uri.clone());

            for token in &tokens {
                symbolizer.add_token(token);
            }

            let mut parser = Parser::new(params.text_document.uri.to_file_path().unwrap(), &tokens).attempt_to_ignore_errors();
            let mut statements = Vec::new();
            while let Ok(statement) = parser.parse_statement() {
                statements.push(statement);
            }

            for statement in &statements {
                symbolizer.add_statement(statement);
            }

            let tokens = symbolizer.to_tokens();

            Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: tokens,
            })))
        }).await

    }

    async fn format(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let mut end = Position::default();

        let result = self.lexed_document(&params.text_document, |tokens, source| {
            let mut result = String::new();
            let mut parser = Parser::new(params.text_document.uri.to_file_path().unwrap(), &tokens);

            if let Some(last) = tokens.last() {
                end.line = last.end.line() as _;
                end.character = last.end.column() as _;
            }

            loop {
                match parser.parse_statement() {
                    Ok(statement) => {
                        result += &statement.format_to_string(source);
                        result += "\n";
                    }

                    Err(ParseDiagnostic::EndOfFile) => break,
                    Err(..) => return Ok(None),
                }
            }

            result = result.trim().to_string();
            Ok(Some(result))
        }).await?;

        let range = Range { start: Position::default(), end, };
        Ok(result.map(|new_text| vec![TextEdit { range, new_text }]))
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, config: InitializeParams) -> Result<InitializeResult> {
        *self.client_configuration.write().await = Some(config);

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

        let this = (*self).clone();
        spawn(async move {
            let this = this;
            this.collect_diagnostics(VersionedTextDocumentIdentifier {
                uri: params.text_document.uri,
                version: params.text_document.version,
            }).await;
        });
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        {
            let mut file_store = self.file_store.write().await;
            file_store.insert(params.text_document.uri.clone(), Arc::from(std::mem::take(&mut params.content_changes[0].text)));
        }
        let this = (*self).clone();
        spawn(async move {
            let this = this;
            this.collect_diagnostics(params.text_document).await;
        });
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        self.format(params).await
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {

        self.lexed_document(&params.text_document, |tokens, _| {
            let mut symbolizer = Symbolizer::new(params.text_document.uri.clone());

            for token in &tokens {
                symbolizer.add_token(token);
            }

            let mut statements = Vec::new();

            let mut parser = Parser::new(params.text_document.uri.to_file_path().unwrap(), &tokens).attempt_to_ignore_errors();
            while let Ok(statement) = parser.parse_statement() {
                statements.push(statement);
            }

            for statement in &statements {
                symbolizer.add_statement(statement);
            }

            let response = symbolizer.to_response();
            Ok(Some(response))
        }).await
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

        self.find_tokens_at(&params.text_document_position_params, |token, _| {
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

            Ok(Some(()))
        }).await?;

        if hover.is_none() {
            hover = self.with_semantics(&params.text_document_position_params.text_document, |analyzer| {
                let pos = params.text_document_position_params.position;
                let location = FileLocation::new(0, pos.line as _, pos.character as _);
                let reference = analyzer.find_reference_at(location)
                    .map(|(range, reference)| {
                        Hover {
                            contents: HoverContents::Markup(MarkupContent {
                                kind: MarkupKind::Markdown,
                                value: reference.hover(),
                            }),
                            range: Some(convert_file_range(range)),
                        }
                    });
                Ok(reference)
            }).await?;
        }

        Ok(hover)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        CompletionEngine::complete(self, params).await
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let mut hints = Vec::new();

        self.lexed_document(&params.text_document, |tokens, _| {
            let mut parser = Parser::new(params.text_document.uri.to_file_path().unwrap(), &tokens).attempt_to_ignore_errors();
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

    async fn goto_definition(&self, params: GotoDefinitionParams) -> Result<Option<GotoDefinitionResponse>> {
        self.with_semantics(&params.text_document_position_params.text_document, |analyzer| {
            let pos = params.text_document_position_params.position;
            let location = FileLocation::new(0, pos.line as _, pos.character as _);
            let reference = analyzer.find_reference_at(location)
                .map(|(range, reference)| {
                    let target_range = convert_file_range(reference.declaration_range);
                    GotoDefinitionResponse::Link(vec![
                        LocationLink {
                            origin_selection_range: Some(convert_file_range(range)),
                            // TODO: Base on FileLocation's file ID
                            target_uri: params.text_document_position_params.text_document.uri.clone(),
                            target_range,
                            target_selection_range: target_range,
                        }
                    ])
                });
            Ok(reference)
        }).await
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
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
        Backend {
            client,
            client_configuration: Arc::new(RwLock::new(None)),
            file_store: Arc::new(RwLock::new(HashMap::new())),
        }
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
