// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

#![feature(thread_id_value)]

mod actions;
mod backend;
mod configuration;
mod context;
mod commands;
mod completions;
mod conversion;
mod error;
mod format;
mod hints;
mod logger;
mod symbolization;

use std::path::{Path, PathBuf};
use std::sync::Arc;

use std::{fs::File, io::Write, pin::Pin};

use log::{info, LevelFilter};
use logger::Logger;
use serde_json::Value;
use tokio::io::{AsyncRead, AsyncWrite, ReadBuf};
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{LanguageServer, LspService, Server};

pub use self::{
    actions::{CodeActionRepository, CodeActionItem, CodeActionsAnalysisContext, CodeActionsAnalyzable},
    backend::Backend,
    commands::LspCommand,
    configuration::{LspClientKind, LspConfiguration},
    context::{BabbelaarContext, BabbelaarFile},
    conversion::{UrlExtension, convert_command, Converter, TextEncoding},
    completions::CompletionEngine,
    error::{BabbelaarLspError, BabbelaarLspResult},
    format::Format,
    hints::InlayHintsEngine,
    symbolization::{LspTokenType, LspSymbolModifier, Symbolizer},
};

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, config: InitializeParams) -> Result<InitializeResult> {
        Ok(self.initialize(config).await?)
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        Ok(self.collect_signature_help(params).await?)
    }

    async fn code_lens(&self, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
        Ok(self.collect_lenses(params).await?)
    }

    async fn code_lens_resolve(&self, params: CodeLens) -> Result<CodeLens> {
        Ok(params)
    }

    async fn initialized(&self, params: InitializedParams) {
        self.initialized(params).await
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.did_open(params).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.did_change(params).await;
    }

    async fn did_delete_files(&self, params: DeleteFilesParams) {
        self.did_delete_files(params).await;
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        Ok(self.format(params).await?)
    }

    async fn document_symbol(&self, params: DocumentSymbolParams) -> Result<Option<DocumentSymbolResponse>> {
        Ok(self.document_symbol(params).await?)
    }

    async fn semantic_tokens_full(&self, params: SemanticTokensParams) -> Result<Option<SemanticTokensResult>> {
        Ok(self.on_semantic_tokens_full(params).await?)
    }

    async fn document_highlight(&self, params: DocumentHighlightParams) -> Result<Option<Vec<DocumentHighlight>>> {
        Ok(self.document_highlight(params).await?)
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        Ok(self.hover(params).await?)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        Ok(CompletionEngine::complete(self, params).await?)
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        Ok(self.inlay_hint(params).await?)
    }

    async fn goto_definition(&self, params: GotoDefinitionParams) -> Result<Option<GotoDefinitionResponse>> {
        Ok(self.goto_definition(params).await?)
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        Ok(self.code_action(params).await?)
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        Ok(self.rename(params).await?)
    }

    async fn execute_command(&self, params: ExecuteCommandParams) -> Result<Option<Value>> {
        Ok(self.execute_command(params).await?)
    }
}

#[tokio::main]
async fn main() {
    // let stdin = Reader::new();
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    run(stdin, stdout).await;
}

pub async fn run<I, O>(stdin: I, stdout: O)
        where I: AsyncRead + std::marker::Unpin,
              O: AsyncWrite {
    log::set_max_level(LevelFilter::Trace);

    let (service, socket) = LspService::new(|client| {
        Logger::initialize(client.clone());
        Backend {
            client,
            configuration: LspConfiguration::new(),
            context: Arc::new(BabbelaarContext::new()),
            code_actions: Arc::new(RwLock::new(CodeActionRepository::new()))
        }
    });

    info!("Babbelaar LSP-dienst starten... Versie: {}", env!("CARGO_PKG_VERSION"));

    Server::new(stdin, stdout, socket).serve(service).await;
}

struct Reader {
    reader: tokio::io::Stdin,
    file: File,
}

impl Reader {
    #[allow(unused)]
    pub fn new() -> Self {
        Self {
            reader: tokio::io::stdin(),
            file: File::create("/tmp/babbelaar-lsp.out").unwrap(),
        }
    }
}

impl AsyncRead for Reader {
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        let this = self.get_mut();

        let mut vec = vec![0; buf.remaining()];
        let mut temp_buf = ReadBuf::new(&mut vec);

        let result = AsyncRead::poll_read(Pin::new(&mut this.reader), cx, &mut temp_buf);
        buf.put_slice(temp_buf.filled());

        this.file.write_all(temp_buf.filled()).unwrap();

        result
    }
}

trait PathBufExt {
    fn to_uri(&self) -> Uri;
}

impl PathBufExt for &Path {
    fn to_uri(&self) -> Uri {
        url::Url::from_file_path(self).unwrap().to_string().parse().unwrap()
    }
}

impl PathBufExt for PathBuf {
    fn to_uri(&self) -> Uri {
        self.as_path().to_uri()
    }
}
