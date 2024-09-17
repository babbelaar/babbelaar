
use std::collections::HashMap;
use std::collections::VecDeque;
use std::path::PathBuf;
use std::sync::Arc;

use babbelaar::*;
use log::info;
use log::warn;
use request::InlayHintRefreshRequest;
use tokio::spawn;
use tokio::sync::RwLock;
use tokio::task::block_in_place;
use tower_lsp::lsp_types::*;
use tower_lsp::Client;

use crate::actions::CodeActionsAnalysisContext;
use crate::actions::CodeActionsAnalyzable;
use crate::conversion::convert_file_range_to_location;
use crate::conversion::StrExtension;
use crate::CodeActionRepository;
use crate::UrlExtension;
use crate::{
    BabbelaarLspResult as Result,
    conversion::{convert_file_range, convert_position, convert_token_range},
    completions::CompletionEngine,
    format::Format,
    symbolization::{LspTokenType, Symbolizer},
};

#[derive(Debug, Clone)]
pub struct Backend {
    pub(super) client: Client,
    pub(super) client_configuration: Arc<RwLock<Option<InitializeParams>>>,
    pub(super) file_store: Arc<RwLock<HashMap<Url, SourceCode>>>,
    pub(super) code_actions: Arc<RwLock<CodeActionRepository>>,
}

impl Backend {
    pub async fn find_tokens_at<F, R>(&self, params: &TextDocumentPositionParams, mut f: F) -> Result<Option<R>>
            where F: FnMut(Token, Vec<Token>) -> Result<Option<R>> {
        self.lexed_document(&params.text_document, |tokens, _| {
            let mut previous = Vec::new();
            let mut tokens = VecDeque::from(tokens);
            while let Some(token) = tokens.pop_front() {
                if let TokenKind::TemplateString(str) = &token.kind {
                    for tok in str {
                        if let TemplateStringToken::Expression(expr_tokens) = tok {
                            for token in expr_tokens.iter().rev() {
                                tokens.push_front(token.clone());
                            }
                        }
                    }

                    continue;
                }

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

    pub async fn collect_lenses(&self, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
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

    pub async fn lexed_document<F, R>(&self, text_document: &TextDocumentIdentifier, f: F) -> Result<R>
    where
        F: FnOnce(Vec<Token>, &SourceCode) -> Result<R>,
    {
        self.with_contents(&text_document.uri, |contents| {
            let tokens = babbelaar::Lexer::new(&contents).collect();
            f(tokens, contents)
        }).await
    }

    pub async fn with_contents<F, R>(&self, uri: &Url, f: F) -> Result<R>
    where
        F: FnOnce(&SourceCode) -> Result<R>,
    {
        let contents = self.file_store.read().await.get(&uri).cloned();
        if let Some(contents) = contents {
            return f(&contents);
        }

        let path = uri.to_path()?;
        let mut contents = match tokio::fs::read_to_string(&path).await {
            Ok(contents) => contents,
            Err(e) => {
                self.client
                    .log_message(MessageType::INFO, format!("Failed to read path \"{}\" {e}", path.display()))
                    .await;
                return Err(e.into());
            }
        };

        // Either `read_to_string` omits the trailing empty line, or the editor assumes
        // there is one regardless of the disk contents, but we have to append one to be sure.
        if !contents.ends_with("\n\n") {
            contents += "\n";
        }

        let contents = SourceCode::new(path, contents);
        {
            let mut store = self.file_store.write().await;
            store.insert(uri.clone(), contents.clone());
        }

        let result = f(&contents);
        result
    }

    pub async fn with_syntax<F, R>(&self, text_document: &TextDocumentIdentifier, f: F) -> Result<R>
    where
        F: FnOnce(ParseTree<'_>, &SourceCode) -> Result<R>,
    {
        self.lexed_document(text_document, |tokens, contents| {
            let mut parser = Parser::new(text_document.uri.to_path()?, &tokens).attempt_to_ignore_errors();
            let tree = parser.parse_tree()?;

            f(tree, contents)
        }).await
    }

    pub async fn with_semantics<F, R>(&self, text_document: &TextDocumentIdentifier, f: F) -> Result<R>
    where
        F: FnOnce(SemanticAnalyzer) -> Result<R>,
    {
        self.with_syntax(text_document, |tree, source_code| {
            let mut analyzer = SemanticAnalyzer::new(source_code);
            analyzer.analyze_tree(&tree);

            f(analyzer)
        }).await
    }

    pub async fn collect_diagnostics(&self, document: VersionedTextDocumentIdentifier) -> Result<()> {
        let path = document.uri.to_path()?;

        let text_document = TextDocumentIdentifier {
            uri: document.uri.clone(),
        };

        let diags = self.lexed_document(&text_document, |tokens, source_code| {
            let mut parser = Parser::new(path, &tokens).attempt_to_ignore_errors();

            let tree = match parser.parse_tree() {
                Ok(tree) => tree,
                Err(e) => {
                    parser.errors.push(e);
                    ParseTree::new(PathBuf::default())
                }
            };

            let mut diags = Vec::new();

            let mut analyzer = SemanticAnalyzer::new(source_code);
            analyzer.analyze_tree(&tree);

            for err in parser.errors {
                let Some(range) = err.range() else { continue };

                let mut ctx = CodeActionsAnalysisContext {
                    semantics: &analyzer,
                    items: Vec::new(),
                    cursor_range: FileRange::default(),
                    contents: source_code,
                };

                err.analyze(&mut ctx);

                let actions: Vec<_> = ctx.items.iter()
                    .map(|x| {
                        block_in_place(|| {
                            let mut actions = self.code_actions.blocking_write();
                            let id = actions.add(x.clone(), document.clone());
                            serde_json::Value::Number(id.into())
                        })
                    })
                    .collect();

                diags.push(Diagnostic {
                    range: convert_file_range(range),
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: Some(NumberOrString::String(err.name().to_string())),
                    code_description: None,
                    source: None,
                    message: err.to_string(),
                    related_information: None,
                    tags: None,
                    data: Some(serde_json::Value::Array(actions)),
                });
            }

            diags.extend(
                analyzer.into_diagnostics()
                .into_iter()
                .map(|e| {
                    let related_information = Some(
                            e.related_info()
                                .iter()
                                .map(|x| DiagnosticRelatedInformation {
                                    location: convert_file_range_to_location(text_document.uri.clone(), x.range()),
                                    message: x.message().to_string(),
                                })
                                .collect()
                    );

                    let actions: Vec<_> = e.actions().iter()
                        .map(|x| {
                            block_in_place(|| {
                                let mut actions = self.code_actions.blocking_write();
                                let id = actions.add(x.clone(), document.clone());
                                serde_json::Value::Number(id.into())
                            })
                        })
                        .collect();

                    Diagnostic {
                        range: convert_file_range(e.range()),
                        severity: Some(match e.severity() {
                            SemanticDiagnosticSeverity::Error => DiagnosticSeverity::ERROR,
                            SemanticDiagnosticSeverity::Warning => DiagnosticSeverity::WARNING,
                        }),
                        code: Some(NumberOrString::String(e.kind().name().to_string())),
                        code_description: None,
                        source: None,
                        message: e.kind().to_string(),
                        related_information,
                        tags: None,
                        data: Some(serde_json::Value::Array(actions)),
                    }
                })
            );

            Ok(diags)
        }).await?;

        self.client.publish_diagnostics(text_document.uri, diags, Some(document.version)).await;
        Ok(())
    }

    pub async fn collect_signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let caret_line = params.text_document_position_params.position.line as usize;
        let caret_column = params.text_document_position_params.position.character as usize;

        self.lexed_document(&params.text_document_position_params.text_document, |tokens, _| {
            let mut parser = Parser::new(params.text_document_position_params.text_document.uri.to_path()?, &tokens).attempt_to_ignore_errors();
            info!("Caret is @ {caret_line}:{caret_column}");
            loop {
                let res = parser.parse_statement();

                let Ok(statement) = res else {
                    info!("Error: {res:#?}");
                    break;
                };
                if statement.range.end().line() < caret_line {
                    continue;
                }

                if statement.range.end().column() < caret_column {
                    continue;
                }
                // info!("Statement: {statement:#?}");

                let StatementKind::Expression(expr) = statement.kind else {
                    log::warn!("Cannot give signature help for a non-expression Statement");
                    return Ok(None);
                };

                let Expression::Postfix(postfix) = expr.as_ref() else {
                    log::warn!("Cannot give signature help for a non-postfix Expression");
                    return Ok(None);
                };

                let PostfixExpressionKind::Call(..) = &postfix.kind else {
                    log::warn!("Cannot give signature help for a non-call Postfix Expression");
                    return Ok(None);
                };

                let Expression::Primary(PrimaryExpression::Reference(calling_name)) = postfix.lhs.value() else {
                    log::warn!("Cannot give signature help for a non-identifier Call Postfix Expression");
                    return Ok(None);
                };

                let Some(builtin_function) = Builtin::FUNCTIONS.iter().find(|x| &x.name == calling_name.value()) else {
                    log::warn!("Cannot give signature help for a non-builtin function");
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
                trigger_characters: Some(vec![".".to_string(), ",".to_string(), "{".to_string()]),
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
            code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
            rename_provider: Some(OneOf::Left(true)),
            ..ServerCapabilities::default()
        }
    }

    pub async fn on_semantic_tokens_full(&self, params: SemanticTokensParams) -> Result<Option<SemanticTokensResult>> {
        self.lexed_document(&params.text_document, |tokens, source_code| {
            let mut symbolizer = Symbolizer::new(params.text_document.uri.clone(), source_code);

            for token in &tokens {
                symbolizer.add_token(token);
            }

            let mut parser = Parser::new(params.text_document.uri.to_path()?, &tokens).attempt_to_ignore_errors();
            let tree = parser.parse_tree()?;

            for statement in tree.all() {
                symbolizer.add_statement(statement);
            }

            let tokens = symbolizer.to_tokens();

            Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: tokens,
            })))
        }).await
    }

    pub async fn format(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let mut end = Position::default();

        let result = self.lexed_document(&params.text_document, |tokens, source| {
            let mut result = String::new();
            let mut parser = Parser::new(params.text_document.uri.to_path()?, &tokens);

            if let Some(last) = tokens.last() {
                end.line = last.end.line() as _;
                end.character = last.end.column() as _;
            }

            for statement in parser.parse_tree().unwrap_or_default().all() {
                result += &statement.format_to_string(source);
                result += "\n";
            }

            result = result.trim().to_string();
            Ok(Some(result))
        }).await?;

        let range = Range { start: Position::default(), end, };
        Ok(result.map(|new_text| vec![TextEdit { range, new_text }]))
    }

    pub async fn initialize(&self, config: InitializeParams) -> Result<InitializeResult> {
        *self.client_configuration.write().await = Some(config);

        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "Babbelaar Taalserveerder".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
            capabilities: self.capabilities(),
        })
    }

    pub async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        self.collect_signature_help(params).await
    }

    pub async fn code_lens(&self, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
        self.collect_lenses(params).await
    }

    pub async fn code_lens_resolve(&self, params: CodeLens) -> Result<CodeLens> {
        Ok(params)
    }

    pub async fn initialized(&self, _: InitializedParams) {
        info!("Initialized");
    }

    pub async fn did_open(&self, params: DidOpenTextDocumentParams) {
        _ = self.with_contents(&params.text_document.uri, |_| { Ok(()) }).await.ok();

        _ = self.client.send_request::<InlayHintRefreshRequest>(()).await.ok();

        let this = (*self).clone();
        spawn(async move {
            let this = this;
            let result = this.collect_diagnostics(VersionedTextDocumentIdentifier {
                uri: params.text_document.uri,
                version: params.text_document.version,
            }).await;

            if let Err(e) = result {
                warn!("Kon bestand niet openen: {e}");
            }
        });
    }

    pub async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        {
            let mut file_store = self.file_store.write().await;
            let path = params.text_document.uri.to_path().unwrap();
            file_store.insert(params.text_document.uri.clone(), SourceCode::new(path, std::mem::take(&mut params.content_changes[0].text)));
        }
        let this = (*self).clone();
        spawn(async move {
            let this = this;
            if let Err(e) = this.collect_diagnostics(params.text_document).await {
                warn!("Kon geen diagnostieken verzamelen: {e}");
            }
        });
    }

    pub async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        self.format(params).await
    }

    pub async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {

        self.lexed_document(&params.text_document, |tokens, source_code| {
            let mut symbolizer = Symbolizer::new(params.text_document.uri.clone(), source_code);

            for token in &tokens {
                symbolizer.add_token(token);
            }

            let mut parser = Parser::new(params.text_document.uri.to_path()?, &tokens).attempt_to_ignore_errors();
            let tree = parser.parse_tree()?;

            for statement in tree.all() {
                symbolizer.add_statement(statement);
            }

            let response = symbolizer.to_response();
            Ok(Some(response))
        }).await
    }

    pub async fn semantic_tokens_full(&self, params: SemanticTokensParams) -> Result<Option<SemanticTokensResult>> {
        self.on_semantic_tokens_full(params).await
    }

    pub async fn document_highlight(&self, params: DocumentHighlightParams) -> Result<Option<Vec<DocumentHighlight>>> {
        let res = self.with_semantics(&params.text_document_position_params.text_document, |analyzer| {

            let pos = params.text_document_position_params.position;
            let location = FileLocation::new(0, pos.line as _, pos.character as _);

            let Some((_, reference)) = analyzer.find_reference_at(location) else {
                return Ok(Some(Vec::new()));
            };

            let Some(references) = analyzer.find_references_of(reference.declaration_range) else {
                return Ok(Some(Vec::new()));
            };

            let mut highlights = vec![
                DocumentHighlight {
                    range: convert_file_range(reference.declaration_range),
                    kind: None,
                }
            ];

            for reference in references {
                highlights.push(DocumentHighlight {
                    range: convert_file_range(reference),
                    kind: None,
                });
            }

            Ok(Some(highlights))
        }).await?;

        Ok(res)
    }

    pub async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
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

                    if let Some(builtin_typ) = Builtin::TYPES.iter().find(|x| x.name() == *ident) {
                        hover = Some(Hover {
                            contents: HoverContents::Markup(MarkupContent {
                                kind: MarkupKind::Markdown,
                                value: builtin_typ.documentation().to_string(),
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
                let location = analyzer.source_code.canonicalize_position(pos);
                let reference = analyzer.find_declaration_range_at(location)
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

    pub async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        CompletionEngine::complete(self, params).await
    }

    pub async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let mut hints = Vec::new();

        self.lexed_document(&params.text_document, |tokens, _| {
            let mut parser = Parser::new(params.text_document.uri.to_path()?, &tokens).attempt_to_ignore_errors();
            let tree = parser.parse_tree()?;

            for statement in tree.all() {
                match &statement.kind {
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

    pub async fn goto_definition(&self, params: GotoDefinitionParams) -> Result<Option<GotoDefinitionResponse>> {
        self.with_semantics(&params.text_document_position_params.text_document, |analyzer| {
            let pos = params.text_document_position_params.position;
            let location = analyzer.source_code.canonicalize_position(pos);
            let reference = analyzer.find_declaration_range_at(location)
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

    pub async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        let mut actions = vec![];

        self.create_code_actions_by_diagnostics(&mut actions, &params.context.diagnostics).await;
        self.create_code_actions_based_on_semantics(&mut actions, &params).await?;

        Ok(Some(actions))
    }

    async fn create_code_actions_by_diagnostics(&self, result: &mut Vec<CodeActionOrCommand>, diagnostics: &[Diagnostic]) {
        for diagnostic in diagnostics {
            self.create_code_actions_by_diagnostic(result, diagnostic).await;
        }
    }

    async fn create_code_actions_by_diagnostic(&self, result: &mut Vec<CodeActionOrCommand>, diagnostic: &Diagnostic) {
        if diagnostic.data.is_none() {
            return;
        }

        let Some(serde_json::Value::Array(action_ids)) = &diagnostic.data else {
            warn!("Invalid CodeAction data: {:#?}", diagnostic.data);
            return;
        };

        for id in action_ids {
            let serde_json::Value::Number(id) = id else { continue };
            let Some(id) = id.as_u64() else { continue };

            let diagnostic = Some(diagnostic.clone());
            let action = self.create_code_action_by_id(id, diagnostic).await;
            if let Some(action) = action {
                result.push(action);
            }
        }
    }

    async fn create_code_action_by_id(&self, id: u64, diagnostic: Option<Diagnostic>) -> Option<CodeActionOrCommand> {
        let repo = self.code_actions.read().await;

        let Some(item) = repo.get(id as usize) else {
            warn!("Could not find CodeAction in the repository with id {id}");
            return None;
        };

        let document = OptionalVersionedTextDocumentIdentifier {
            uri: item.document.uri.clone(),
            version: Some(item.document.version),
        };

        // If this Code Action is created by a diagnostic, it is preferred.
        let is_preferred = Some(diagnostic.is_some());
        let diagnostics = diagnostic.map(|diagnostic| vec![diagnostic.clone()]);
        let edit = Some(self.create_workspace_edit_by_code_action_item(&item.action, document));

        Some(CodeActionOrCommand::CodeAction(CodeAction {
            title: item.action.type_().to_string(),
            kind: Some(CodeActionKind::QUICKFIX),
            diagnostics,
            edit,
            command: None,
            is_preferred,
            disabled: None,
            data: None,
        }))
    }

    fn create_workspace_edit_by_code_action_item(&self, action: &BabbelaarCodeAction, document: OptionalVersionedTextDocumentIdentifier) -> WorkspaceEdit {
        let edit = self.create_text_document_edit(action, document);
        let edits = DocumentChanges::Edits(vec![edit]);

        WorkspaceEdit {
            changes: None,
            document_changes: Some(edits),
            change_annotations: None
        }
    }

    fn create_text_document_edit(&self, action: &BabbelaarCodeAction, text_document: OptionalVersionedTextDocumentIdentifier) -> TextDocumentEdit {
        let mut edits = Vec::new();

        for edit in action.edits() {
            let edit = TextEdit {
                range: convert_file_range(edit.replacement_range()),
                new_text: edit.new_text().to_string(),
            };

            edits.push(OneOf::Left(edit));
        }

        TextDocumentEdit {
            text_document,
            edits,
        }
    }

    async fn create_code_actions_based_on_semantics(&self, actions: &mut Vec<CodeActionOrCommand>, params: &CodeActionParams) -> Result<()> {
        let document = OptionalVersionedTextDocumentIdentifier {
            uri: params.text_document.uri.clone(),
            version: None,
        };

        self.with_syntax(&params.text_document, |tree, source_code| {
            let start = FileLocation::new(
                usize::MAX,
                params.range.start.line as _,
                params.range.start.character as _,
            );

            let end = FileLocation::new(
                usize::MAX,
                params.range.end.line as _,
                params.range.end.character as _,
            );

            let mut analyzer = SemanticAnalyzer::new(source_code);
            analyzer.analyze_tree(&tree);

            let mut ctx = CodeActionsAnalysisContext {
                semantics: &analyzer,
                items: Vec::new(),
                cursor_range: FileRange::default(),
                contents: source_code,
            };

            ctx.cursor_range = ctx.create_range_and_calculate_byte_column(start, end)?;

            tree.analyze(&mut ctx);

            for action in ctx.items {
                let edit = self.create_workspace_edit_by_code_action_item(&action, document.clone());
                actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                    title: action.type_().to_string(),
                    kind: Some(CodeActionKind::QUICKFIX),
                    diagnostics: None,
                    edit: Some(edit),
                    command: None,
                    is_preferred: Some(false),
                    disabled: None,
                    data: None,
                }));
            }

            Ok(())
        }).await
    }

    pub async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let text_document = OptionalVersionedTextDocumentIdentifier {
            uri: params.text_document_position.text_document.uri.clone(),
            version: None,
        };

        let pos = params.text_document_position.position;
        let location = FileLocation::new(0, pos.line as _, pos.character as _);

        self.with_semantics(&params.text_document_position.text_document, |analyzer| {
            let Some((declaration_range, _)) = analyzer.find_declaration_range_at(location) else {
                return Ok(None);
            };

            let Some(references) = analyzer.find_references_of(declaration_range) else {
                return Ok(None);
            };

            let edits = references.into_iter()
                    .chain(std::iter::once(declaration_range))
                    .map(|range| OneOf::Left(TextEdit {
                        range: convert_file_range(range),
                        new_text: params.new_name.clone(),
                    }))
                    .collect();

            let edits = TextDocumentEdit {
                text_document,
                edits,
            };

            Ok(Some(WorkspaceEdit {
                changes: None,
                document_changes: Some(DocumentChanges::Edits(vec![edits])),
                change_annotations: None,
            }))
        }).await
    }
}
