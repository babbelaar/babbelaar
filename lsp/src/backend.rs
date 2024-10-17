// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::collections::HashMap;
use std::collections::VecDeque;
use std::fs::read_dir;
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

use tower_lsp::lsp_types::Uri as Url;

use crate::actions::CodeActionsAnalysisContext;
use crate::actions::CodeActionsAnalyzable;
use crate::convert_command;
use crate::hints::InlayHintsEngine;
use crate::symbolization::LspSymbolModifier;
use crate::BabbelaarContext;
use crate::BabbelaarLspError;
use crate::CodeActionRepository;
use crate::Converter;
use crate::PathBufExt;
use crate::TextEncoding;
use crate::UrlExtension;
use crate::{
    BabbelaarLspResult as Result,
    completions::CompletionEngine,
    format::Format,
    symbolization::{LspTokenType, Symbolizer},
};

#[derive(Debug, Clone)]
pub struct Backend {
    pub(super) client: Client,
    pub(super) client_configuration: Arc<RwLock<Option<InitializeParams>>>,
    pub(super) context: Arc<BabbelaarContext>,
    pub(super) code_actions: Arc<RwLock<CodeActionRepository>>,
}

impl Backend {
    #[must_use]
    fn get_position_encoding(&self) -> TextEncoding {
        self.get_position_encoding_impl().unwrap_or_default()
    }

    fn get_position_encoding_impl(&self) -> Option<TextEncoding> {
        let config = block_in_place(|| {
            self.client_configuration.blocking_read()
        });
        let config = config.as_ref().unwrap();
        for encoding in config.capabilities.general.as_ref()?.position_encodings.as_ref()? {
            if *encoding == PositionEncodingKind::UTF8 {
                return Some(TextEncoding::Utf8);
            }
        }

        Some(TextEncoding::Utf16)
    }

    #[must_use]
    pub fn converter(&self, source_code: &SourceCode) -> Converter {
        let encoding = self.get_position_encoding();
        Converter::new(source_code.clone(), encoding)
    }

    async fn converter_for(&self, file_id: FileId) -> Converter {
        self.converter(&self.context.source_code_of(file_id).await)
    }

    pub async fn find_tokens_at<F, R>(&self, params: &TextDocumentPositionParams, mut f: F) -> Result<Option<R>>
            where F: FnMut(Token, Vec<Token>, &SourceCode) -> Result<Option<R>> {
        self.lexed_document(&params.text_document, |tokens, source_code| {
            let mut previous = Vec::new();
            let mut tokens = VecDeque::from(Vec::from(tokens));
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

                return f(token, previous, source_code);
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
        F: FnOnce(&[Token], &SourceCode) -> Result<R>,
    {
        self.context.with_file(&text_document.uri, |file| {
            let (tokens, source_code) = file.ensure_lexed();
            f(tokens, source_code)
        }).await
    }

    pub async fn with_contents<F, R>(&self, uri: &Url, f: F) -> Result<R>
    where
        F: FnOnce(&SourceCode) -> Result<R>,
    {
        self.context.with_file(uri, |file| {
            f(file.source_code())
        }).await
    }

    pub async fn with_syntax<F, R>(&self, text_document: &TextDocumentIdentifier, f: F) -> Result<R>
    where
        F: FnOnce(&ParseTree, &SourceCode) -> Result<R>,
    {
        self.context.with_file(&text_document.uri, |file| {
            let (tree, source_code) = file.ensure_parsed();
            f(tree, source_code)
        }).await
    }

    pub async fn with_semantics<F, R>(&self, text_document: &TextDocumentIdentifier, f: F) -> Result<R>
    where
        F: FnOnce(&Arc<SemanticAnalyzer>, &SourceCode) -> Result<R>,
    {
        let source_code = self.with_contents(&text_document.uri, |x| {
            Ok(x.clone())
        }).await?;

        let analyzer = self.context.semantic_analysis().await;
        f(&analyzer, &source_code)
    }

    pub fn collect_diagnostics_in_background(&self) {
        let this = (*self).clone();
        spawn(async move {
            let this = this;
            if let Err(e) = this.collect_diagnostics().await {
                warn!("Kon geen diagnostieken verzamelen: {e}");
            }
        });
    }

    pub async fn all_converters(&self) -> HashMap<FileId, Converter> {
        let mut map = HashMap::new();

        self.context.with_all_files(|file| {
            map.insert(file.source_code().file_id(), self.converter(file.source_code()));
            Ok(())
        }).await.unwrap();

        map
    }

    pub async fn collect_diagnostics(&self) -> Result<()> {
        let mut file_infos: HashMap<FileId, (Converter, VersionedTextDocumentIdentifier)> = HashMap::new();
        let mut diags: HashMap<FileId, Vec<Diagnostic>> = HashMap::new();

        self.context.with_all_files(|file| {
            let document = VersionedTextDocumentIdentifier {
                uri: file.source_code().path().to_uri(),
                version: file.source_code().version(),
            };
            file_infos.insert(file.source_code().file_id(), (self.converter(file.source_code()), document.clone()));
            diags.insert(file.source_code().file_id(), Vec::new());

            let converter = self.converter(file.source_code());

            let source_code = file.source_code().clone();
            for err in file.lexer_diagnostics() {
                diags.entry(err.location.file_id()).or_default().push(Diagnostic {
                    range: converter.convert_file_range(err.location.as_zero_range()),
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: Some(NumberOrString::String(err.kind.name().to_string())),
                    code_description: None,
                    source: None,
                    message: err.kind.to_string(),
                    related_information: None,
                    tags: None,
                    data: None,
                });
            }

            let errors = file.parse_diagnostics();
            let analyzer = SemanticAnalyzer::new(HashMap::new());

            for err in errors {
                let range = err.range();
                let mut ctx = CodeActionsAnalysisContext {
                    semantics: &analyzer,
                    items: Vec::new(),
                    cursor_range: FileRange::default(),
                    contents: source_code.contents(),
                    path: source_code.path().to_path_buf(),
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

                diags.entry(range.file_id()).or_default().push(Diagnostic {
                    range: converter.convert_file_range(range),
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

            Ok(())
        }).await?;

        let analyzer = self.context.semantic_analysis().await;

        for e in analyzer.diagnostics() {
            if e.range().file_id() == FileId::INTERNAL {
                continue;
            }

            let related_information = Some(
                    e.related_info()
                        .iter()
                        .filter_map(|x| {
                            let (converter, version) = file_infos.get(&x.range().file_id())?;
                            Some(DiagnosticRelatedInformation {
                                location: converter.convert_file_range_to_location(version.uri.clone(), x.range()),
                                message: x.message().to_string(),
                            })
                        })
                        .collect()
            );

            let Some((converter, url)) = file_infos.get(&e.range().file_id()).cloned() else {
                log::warn!("Ongeldige BestandID: {:?}", e.range().file_id());
                continue;
            };

            let actions: Vec<_> = e.actions().iter()
                .map(|x| {
                    block_in_place(|| {
                        let mut actions = self.code_actions.blocking_write();
                        let id = actions.add(x.clone(), url.clone());
                        serde_json::Value::Number(id.into())
                    })
                })
                .collect();

            diags.entry(e.range().file_id()).or_default().push(Diagnostic {
                range: converter.convert_file_range(e.range()),
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
            });
        }

        for (file_id, diags) in diags.into_iter() {
            let document = file_infos.get(&file_id).cloned().unwrap().1;
            self.client.publish_diagnostics(document.uri, diags, Some(document.version)).await;
        }
        Ok(())
    }

    pub async fn collect_signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let analyzer = self.with_semantics(&params.text_document_position_params.text_document, |a, _| Ok(Arc::clone(&a))).await?;

        self.lexed_document(&params.text_document_position_params.text_document, |tokens, source_code| {
            let converter = self.converter(source_code);
            let location = converter.convert_location(params.text_document_position_params.position);
            let caret_line = location.line();
            let caret_column = location.column();

            let mut parser = Parser::new(params.text_document_position_params.text_document.uri.to_path()?, &tokens);
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

                match postfix.kind.value() {
                    PostfixExpressionKind::Call(..) => {
                        let Expression::Primary(PrimaryExpression::Reference(calling_name)) = postfix.lhs.value() else {
                            log::warn!("Cannot give signature help for a non-identifier Call Postfix Expression");
                            return Ok(None);
                        };

                        let Some(builtin_function) = Builtin::FUNCTIONS.iter().find(|x| x.name == calling_name.value()) else {
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

                    PostfixExpressionKind::MethodCall(call_expr) => {
                        let Some(ty) = analyzer.context.value_type_tracker.as_ref().and_then(|x| x.get(&postfix.lhs.range())) else {
                            log::warn!("Kan geen handtekeningshulp aanbieden voor deze methode, want we weten niet aan welk type hij gebonden zit.");
                            return Ok(None);
                        };

                        let SemanticType::Custom { base, parameters: _ } = ty else {
                            log::warn!("Kan geen handtekeningshulp aanbieden voor deze methode, want het is gebonden aan een ingebouwd of speciaal type.");
                            return Ok(None);
                        };

                        let name_to_find = call_expr.method_name.value();
                        let Some(method) = base.methods.iter().find(|x| x.function.name.value() == name_to_find) else {
                            log::trace!("Kan geen handtekeningshulp aanbieden, want methode `{name_to_find}` bestaat niet.");
                            return Ok(None);
                        };

                        let mut label = format!("{}.{}(", base.name.value(), method.function.name.value());
                        let mut parameters = Vec::new();

                        for (idx, param) in method.function.parameters.iter().enumerate() {
                            if idx != 0 {
                                label += ", ";
                            }

                            let start = label.len() as u32;
                            label += param.name.value();
                            label += ": ";
                            label += &param.ty.as_ref().clone().resolve_against(ty).name();
                            let end = label.len() as u32;

                            parameters.push(ParameterInformation {
                                label: ParameterLabel::LabelOffsets([start, end]),
                                documentation: None,
                            });
                        }

                        label += ")";

                        let return_type = method.function.return_type.clone().resolve_against(ty);
                        if !return_type.is_null() {
                            label += " -> ";
                            label += &return_type.name();
                        }

                        let active_parameter = Some(call_expr.call.arguments.len().saturating_sub(1) as u32);
                        return Ok(Some(SignatureHelp {
                            signatures: [
                                SignatureInformation {
                                    label,
                                    documentation: None,
                                    parameters: Some(parameters),
                                    active_parameter,
                                }
                            ].to_vec(),
                            active_signature: Some(0),
                            active_parameter,
                        }));
                    }

                    _ => {
                        log::warn!("Cannot give signature help for this kind of Postfix Expression");
                        return Ok(None);
                    }
                }
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
                trigger_characters: Some(vec![".".to_string(), ",".to_string(), "{".to_string(), "@".to_string()]),
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
                        token_modifiers: LspSymbolModifier::legend()
                    },
                    range: Some(false),
                    full: Some(SemanticTokensFullOptions::Bool(true)),
                }
            )),
            code_lens_provider: Some(CodeLensOptions {
                resolve_provider: Some(true),
            }),
            inlay_hint_provider: Some(OneOf::Right(InlayHintServerCapabilities::Options(InlayHintOptions {
                work_done_progress_options: Default::default(),
                resolve_provider: Some(false),
            }))),
            declaration_provider: Some(DeclarationCapability::Options(DeclarationOptions {
                work_done_progress_options: WorkDoneProgressOptions {
                    work_done_progress: None,
                },
            })),
            definition_provider: Some(OneOf::Left(true)),
            code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
            rename_provider: Some(OneOf::Left(true)),
            notebook_document_sync: Some(OneOf::Left(NotebookDocumentSyncOptions {
                notebook_selector: [
                    NotebookSelector::ByCells {
                        notebook: Some(Notebook::String("*".into())),
                        cells: [
                            NotebookCellSelector {
                                language: "babbelaar".into(),
                            }
                        ].to_vec(),
                    }
                ].to_vec(),
                save: None,
            })),
            workspace: Some(WorkspaceServerCapabilities {
                workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                    supported: Some(true),
                    change_notifications: None,
                }),
                file_operations: Some(WorkspaceFileOperationsServerCapabilities {
                    will_delete: Some(FileOperationRegistrationOptions {
                        filters: [FileOperationFilter {
                            scheme: Some("file".into()),
                            pattern: FileOperationPattern {
                                glob: "**/*.bab".into(),
                                matches: None,
                                options: None,
                            },
                        }].to_vec(),
                    }),
                    did_delete: Some(FileOperationRegistrationOptions {
                        filters: [FileOperationFilter {
                            scheme: Some("file".into()),
                            pattern: FileOperationPattern {
                                glob: "**/*.bab".into(),
                                matches: None,
                                options: None,
                            },
                        }].to_vec(),
                    }),
                    ..Default::default()
                })
            }),
            ..Default::default()
        }
    }

    pub async fn on_semantic_tokens_full(&self, params: SemanticTokensParams) -> Result<Option<SemanticTokensResult>> {
        self.lexed_document(&params.text_document, |tokens, source_code| {
            let mut symbolizer = Symbolizer::new(params.text_document.uri.clone(), source_code, self.converter(source_code));

            for token in tokens {
                symbolizer.add_token(token);
            }

            let mut parser = Parser::new(params.text_document.uri.to_path()?, &tokens);
            let tree = parser.parse_tree();

            symbolizer.add_tree(&tree);

            let tokens = symbolizer.to_tokens();

            Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: tokens,
            })))
        }).await
    }

    pub async fn format(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let mut end = Position::default();

        let result = self.lexed_document(&params.text_document, |tokens, _| {
            let mut result = String::new();
            let mut parser = Parser::new(params.text_document.uri.to_path()?, &tokens);

            if let Some(last) = tokens.last() {
                end.line = last.end.line() as _;
                end.character = last.end.column() as _;
            }

            for statement in parser.parse_tree().all() {
                result += &statement.format_to_string();
                result += "\n";
            }

            result = result.trim().to_string();
            Ok(Some(result))
        }).await?;

        let range = Range { start: Position::default(), end, };
        Ok(result.map(|new_text| vec![TextEdit { range, new_text, insert_text_format: Some(InsertTextFormat::SNIPPET) }]))
    }

    pub async fn initialize(&self, config: InitializeParams) -> Result<InitializeResult> {
        let workspace_folder = config.workspace_folders
                .as_ref()
                .and_then(|folders| folders.get(0))
                .map(|workspace_folder| workspace_folder.uri.clone());

        *self.client_configuration.write().await = Some(config);

        if let Some(folder) = workspace_folder {
            let path = folder.to_path().unwrap();
            let read_dir = read_dir(&path).map_err(|error| BabbelaarLspError::InvalidWorkspacePath { error, path })?;

            for file in read_dir.flatten() {
                if file.file_name().to_string_lossy().ends_with(".bab") {
                    self.context.load_and_register_file(file.path()).await?;
                }
            }
        }

        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "Babbelaar Taaldienaar".to_string(),
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
        info!("Babbelaar Taaldienaar is geïnitialiseerd.");
    }

    pub async fn did_open(&self, params: DidOpenTextDocumentParams) {
        _ = self.with_contents(&params.text_document.uri, |_| { Ok(()) }).await.ok();

        _ = self.client.send_request::<InlayHintRefreshRequest>(()).await.ok();

        let this = (*self).clone();
        spawn(async move {
            let this = this;
            let result = this.collect_diagnostics().await;

            if let Err(e) = result {
                warn!("Kon bestand niet openen: {e}");
            }
        });
    }

    pub async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        {
            let path = params.text_document.uri.to_path().unwrap();
            let source_code = SourceCode::new(path, params.text_document.version, std::mem::take(&mut params.content_changes[0].text));
            self.context.register_file(source_code).await;
        }

        self.collect_diagnostics_in_background();
    }

    pub async fn did_delete_files(&self, params: DeleteFilesParams) {
        let mut paths = Vec::new();
        for file in params.files {
            assert!(file.uri.starts_with("file://"));
            let uri = &file.uri["file://".len()..];
            let path = PathBuf::from(uri);
            paths.push(path);
        }

        self.context.delete_files(&paths).await;
        self.collect_diagnostics_in_background();
    }

    pub async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        self.format(params).await
    }

    pub async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {

        self.lexed_document(&params.text_document, |tokens, source_code| {
            let mut symbolizer = Symbolizer::new(params.text_document.uri.clone(), source_code, self.converter(source_code));

            for token in tokens {
                symbolizer.add_token(token);
            }

            let mut parser = Parser::new(params.text_document.uri.to_path()?, &tokens);
            let tree = parser.parse_tree();

            symbolizer.add_tree(&tree);

            let response = symbolizer.to_response();
            Ok(Some(response))
        }).await
    }

    pub async fn semantic_tokens_full(&self, params: SemanticTokensParams) -> Result<Option<SemanticTokensResult>> {
        self.on_semantic_tokens_full(params).await
    }

    pub async fn document_highlight(&self, params: DocumentHighlightParams) -> Result<Option<Vec<DocumentHighlight>>> {
        let res = self.with_semantics(&params.text_document_position_params.text_document, |analyzer, source_code| {
            let converter = self.converter(source_code);
            let location = converter.convert_location(params.text_document_position_params.position);

            let Some((_, reference)) = analyzer.find_reference_at(location) else {
                if let Some((range, ..)) = analyzer.find_declaration_range_at(location) {
                    return Ok(Some([
                        DocumentHighlight {
                            range: converter.convert_file_range(range),
                            kind: None,
                        }
                    ].to_vec()));
                }

                return Ok(Some(Vec::new()));
            };

            let mut highlights = Vec::new();
            if reference.declaration_range.file_id() == source_code.file_id() {
                highlights.push(DocumentHighlight {
                    range: converter.convert_file_range(reference.declaration_range),
                    kind: None,
                });
            }

            let Some(references) = analyzer.find_references_of(reference.declaration_range) else {
                return Ok(Some(highlights));
            };

            for reference in references {
                if reference.file_id() != source_code.file_id() {
                    continue;
                }

                highlights.push(DocumentHighlight {
                    range: converter.convert_file_range(reference),
                    kind: None,
                });
            }

            Ok(Some(highlights))
        }).await?;

        Ok(res)
    }

    pub async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let mut hover = None;

        self.find_tokens_at(&params.text_document_position_params, |token, _, source_code| {
            let converter = self.converter(source_code);

            match &token.kind {
                TokenKind::Identifier(ident) => {
                    if let Some(builtin_function) = Builtin::FUNCTIONS.iter().find(|x| ident == x.name) {
                        hover = Some(Hover {
                            contents: HoverContents::Markup(MarkupContent {
                                kind: MarkupKind::Markdown,
                                value: builtin_function.documentation.to_string(),
                            }),
                            range: Some(converter.convert_token_range(&token)),
                        });
                    }

                    if let Some(builtin_typ) = Builtin::TYPES.iter().find(|x| x.name() == *ident) {
                        hover = Some(Hover {
                            contents: HoverContents::Markup(MarkupContent {
                                kind: MarkupKind::Markdown,
                                value: builtin_typ.documentation().to_string(),
                            }),
                            range: Some(converter.convert_token_range(&token)),
                        });
                    }
                }

                TokenKind::Keyword(keyword) => {
                    hover = Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: keyword.provide_documentation().into_owned(),
                        }),
                        range: Some(converter.convert_token_range(&token)),
                    });
                }

                _ => (),
            }

            Ok(Some(()))
        }).await?;

        if hover.is_none() {
            let (reference, source_code) = self.with_semantics(&params.text_document_position_params.text_document, |analyzer, source_code| {
                let pos = params.text_document_position_params.position;
                let location = self.converter(source_code).convert_location(pos);
                Ok((analyzer.find_reference_at(location), source_code.clone()))
            }).await?;

            if let Some((range, reference)) = reference {
                debug_assert_eq!(range.file_id(), source_code.file_id(), "is invalid");
                let text = reference.hover();
                let file_name = self.file_humanized_name(reference.declaration_range.file_id()).await.unwrap_or_else(|| "(onbekend)".to_string());

                hover = Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: format!("```babbelaar\n// In bestand {file_name}\n{text}\n```"),
                    }),
                    range: Some(self.converter(&source_code).convert_file_range(range)),
                });
            }
        }

        Ok(hover)
    }

    pub async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        CompletionEngine::complete(self, params).await
    }

    pub async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        InlayHintsEngine::hint(self, params).await
    }

    pub async fn goto_definition(&self, params: GotoDefinitionParams) -> Result<Option<GotoDefinitionResponse>> {
        let (reference, source_code) = self.with_semantics(&params.text_document_position_params.text_document, |analyzer, source_code| {
            let pos = params.text_document_position_params.position;
            let location = self.converter(source_code).convert_location(pos);
            if let Some(reference) = analyzer.find_reference_at(location) {
                return Ok((Some(reference), source_code.clone()));
            }
            Ok((analyzer.find_declaration_range_at(location), source_code.clone()))
        }).await?;

        Ok(match reference {
            Some((origin_range, reference)) => {
                let file_id = reference.declaration_range.file_id();
                if file_id == FileId::INTERNAL {
                    log::warn!("Verwijst naar een intern bestand: {reference:#?}");
                    return Ok(None);
                }

                let target_range = self.converter_for(reference.declaration_range.file_id()).await.convert_file_range(reference.declaration_range);

                let Some(path) = self.context.path_of(file_id).await else {
                    log::warn!("Bestand is onbekend");
                    return Ok(None);
                };

                let target_uri = path.to_uri();

                Some(GotoDefinitionResponse::Link(vec![
                    LocationLink {
                        origin_selection_range: Some(self.converter(&source_code).convert_file_range(origin_range)),
                        target_uri,
                        target_range,
                        target_selection_range: target_range,
                    }
                ]))
            }

            None => None,
        })
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

        // If this Code Action is created by a diagnostic, it is preferred.
        let is_preferred = Some(diagnostic.is_some());
        let diagnostics = diagnostic.map(|diagnostic| vec![diagnostic.clone()]);
        let edit = Some(self.create_workspace_edit_by_code_action_item(&item.action).await);

        Some(CodeActionOrCommand::CodeAction(CodeAction {
            title: item.action.type_().to_string(),
            kind: Some(match item.action.fix_kind() {
                BabbelaarFixKind::QuickFix => CodeActionKind::QUICKFIX,
                BabbelaarFixKind::Refactor => CodeActionKind::REFACTOR,
            }),
            diagnostics,
            edit,
            command: item.action.command().map(convert_command),
            is_preferred,
            disabled: None,
            data: None,
        }))
    }

    async fn create_workspace_edit_by_code_action_item(&self, action: &BabbelaarCodeAction) -> WorkspaceEdit {
        let document_changes = if action.edits().is_empty() {
            None
        } else {
            let edits = self.create_document_change_operation_for_action(action).await;
            let edits = DocumentChanges::Operations(edits);
            Some(edits)
        };

        WorkspaceEdit {
            changes: None,
            document_changes,
            change_annotations: None
        }
    }

    async fn create_document_change_operation_for_action(&self, action: &BabbelaarCodeAction) -> Vec<DocumentChangeOperation> {
        let mut operations = Vec::new();

        for edit in action.edits() {
            let mut uri = None;

            let source_code = if let Some(new_file_path) = edit.new_file_path() {
                let new_uri = new_file_path.to_uri();
                uri = Some(new_uri.clone());
                operations.push(DocumentChangeOperation::Op(ResourceOp::Create(CreateFile {
                    uri: new_uri,
                    options: Some(CreateFileOptions {
                        overwrite: Some(false),
                        ignore_if_exists: Some(false),
                    }),
                    annotation_id: None,
                })));
                SourceCode::new(new_file_path.to_string_lossy().to_string(), 0, edit.new_text().to_string())
            } else {
                self.context.source_code_of(edit.replacement_range().file_id()).await
            };

            let converter = self.converter(&source_code);

            let edit = TextEdit {
                range: converter.convert_file_range(edit.replacement_range()),
                new_text: edit.new_text().to_string(),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
            };

            let file_id = action.edits()[0].replacement_range().file_id();

            let uri = match uri {
                Some(uri) => uri,
                None => match self.context.path_of(file_id).await {
                    Some(path) => path.to_uri(),
                    None => panic!("Failed to find path of {file_id:?} for action {action:#?}"),
                }
            };

            operations.push(DocumentChangeOperation::Edit(TextDocumentEdit {
                text_document: OptionalVersionedTextDocumentIdentifier {
                    uri,
                    version: None,
                },
                edits: vec![OneOf::Left(edit)],
            }));
        }

        operations
    }

    async fn create_code_actions_based_on_semantics(&self, actions: &mut Vec<CodeActionOrCommand>, params: &CodeActionParams) -> Result<()> {
        let analyzer = self.context.semantic_analysis().await;

        let mut items = self.with_syntax(&params.text_document, |tree, source_code| {
            let converter = self.converter(source_code);
            let start = converter.convert_location(params.range.start);
            let end = converter.convert_location(params.range.end);

            let mut ctx = CodeActionsAnalysisContext {
                semantics: &analyzer,
                items: Vec::new(),
                cursor_range: FileRange::default(),
                contents: source_code,
                path: tree.path().to_path_buf(),
            };

            ctx.cursor_range = ctx.create_range_and_calculate_byte_column(start, end)?;

            tree.analyze(&mut ctx);

            Ok(ctx.items)
        }).await?;

        self.with_semantics(&params.text_document, |analyzer, source_code| {
            for diagnostic in analyzer.diagnostics() {
                self.create_code_actions_by_semantic_diagnostic(diagnostic, analyzer, source_code, &mut items);
            }

            Ok(())
        }).await?;

        for action in items {
            let edit = self.create_workspace_edit_by_code_action_item(&action).await;
            actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                title: action.type_().to_string(),
                kind: Some(CodeActionKind::QUICKFIX),
                diagnostics: None,
                edit: Some(edit),
                command: action.command().map(convert_command),
                is_preferred: Some(false),
                disabled: None,
                data: None,
            }));
        }

        Ok(())
    }

    pub async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let text_document = OptionalVersionedTextDocumentIdentifier {
            uri: params.text_document_position.text_document.uri.clone(),
            version: None,
        };

        let pos = params.text_document_position.position;

        let converters = self.all_converters().await;

        self.with_semantics(&params.text_document_position.text_document, |analyzer, source_code| {
            let converter = self.converter(source_code);
            let location = converter.convert_location(pos);

            let Some((declaration_range, _)) = analyzer.find_declaration_range_at(location) else {
                return Ok(None);
            };

            let Some(references) = analyzer.find_references_of(declaration_range) else {
                return Ok(None);
            };

            let edits = references.into_iter()
                    .chain(std::iter::once(declaration_range))
                    .map(|range| {
                        let converter = converters.get(&range.file_id()).unwrap();
                        OneOf::Left(TextEdit {
                            range: converter.convert_file_range(range),
                            new_text: params.new_name.clone(),
                            insert_text_format: Some(InsertTextFormat::SNIPPET),
                        })
                    })
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

    pub async fn file_humanized_name(&self, id: FileId) -> Option<String> {
        let path = self.context.path_of(id).await;
        let path = path.as_ref()?.to_string_lossy();

        let config = self.client_configuration.read().await;
        let config = config.as_ref()?;

        let Some(folders) = &config.workspace_folders else {
            return Some(path.to_string());
        };

        for folder in folders {
            let Some(folder) = folder.uri.to_path().ok() else {
                continue;
            };

            let folder = folder.to_string_lossy();
            if path.starts_with(folder.as_ref()) {
                return Some(path[folder.len()..].trim_start_matches(|c| c == '/' || c == '\\').to_string());
            }
        }

        Some(path.to_string())
    }

    fn create_code_actions_by_semantic_diagnostic(
        &self,
        diagnostic: &SemanticDiagnostic,
        analyzer: &SemanticAnalyzer,
        source_code: &SourceCode,
        items: &mut Vec<BabbelaarCodeAction>
    ) {
        _ = analyzer;
        _ = source_code;

        match diagnostic.kind() {
            SemanticDiagnosticKind::UnknownAttribute { name, range } => {
                items.push(
                    BabbelaarCodeAction::new(
                        BabbelaarCodeActionType::RemoveAttribute { name: name.clone() },
                        [
                            FileEdit::new(*range, "")
                        ].to_vec()
                    )
                );
            }

            _ => (),
        }
    }
}
