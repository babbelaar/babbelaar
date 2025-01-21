// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::sync::Arc;

use tokio::{sync::RwLock, task::block_in_place};
use tower_lsp::lsp_types::*;

use crate::{symbolization::LspSymbolModifier, LspCommand, LspTokenType, TextEncoding};

#[derive(Debug, Clone)]
pub struct LspConfiguration {
    client_configuration: Arc<RwLock<Option<Arc<InitializeParams>>>>,
}

impl LspConfiguration {
    #[must_use]
    pub fn new() -> Self {
        Self {
            client_configuration: Arc::new(RwLock::new(None)),
        }
    }

    #[must_use]
    pub async fn capabilities(&self, client_configuration: InitializeParams) -> ServerCapabilities {
        *self.client_configuration.write().await = Some(Arc::new(client_configuration));

        // this is done in the same function, to be able to do capabilities/config
        // based on what the client provides.
        self.get_capabilities()
    }

    fn get_capabilities(&self) -> ServerCapabilities {
        ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(
                TextDocumentSyncKind::FULL,
            )),
            completion_provider: Some(CompletionOptions {
                resolve_provider: Some(false),
                trigger_characters: Some(vec![".".to_string(), ",".to_string(), "{".to_string(), "@".to_string(), "\"".to_string()]),
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
            execute_command_provider: Some(ExecuteCommandOptions {
                commands: LspCommand::names(),
                ..Default::default()
            }),
            ..Default::default()
        }
    }

    #[must_use]
    pub fn position_encoding(&self) -> TextEncoding {
        self.position_encoding_impl().unwrap_or_default()
    }

    #[must_use]
    fn position_encoding_impl(&self) -> Option<TextEncoding> {
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
    pub fn server_info(&self) -> ServerInfo {
        ServerInfo {
            name: "Babbelaar Taaldienaar".to_string(),
            version: Some(env!("CARGO_PKG_VERSION").to_string()),
        }
    }

    #[must_use]
    pub async fn client(&self) -> Arc<InitializeParams> {
        self.client_configuration.read().await.clone().unwrap()
    }

    #[must_use]
    pub async fn client_kind(&self) -> LspClientKind {
        self.client_kind_impl().await.unwrap_or_default()
    }

    async fn client_kind_impl(&self) -> Option<LspClientKind> {
        let config = self.client_configuration.read().await;
        let name = config.as_ref()?
            .client_info.as_ref()?
            .name.as_str();

        match name {
            "Visual Studio Code" => Some(LspClientKind::VisualStudioCode),
            _ => Some(LspClientKind::Unknown),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub enum LspClientKind {
    #[default]
    Unknown,
    VisualStudioCode,
}
