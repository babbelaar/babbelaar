// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

mod backend;
mod completions;
mod conversion;
mod error;
mod format;
mod logger;
mod symbolization;

use std::collections::HashMap;
use std::sync::Arc;

use log::{info, warn, LevelFilter};
use logger::Logger;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{LanguageServer, LspService, Server};

pub use self::{
    backend::Backend,
    conversion::{UrlExtension, convert_file_range, convert_position, convert_token_range},
    completions::CompletionEngine,
    error::{BabbelaarLspError, BabbelaarLspResult},
    format::Format,
    symbolization::{LspTokenType, Symbolizer},
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

    async fn initialized(&self, _: InitializedParams) {
        info!("Initialized");
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.did_open(params).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.did_change(params).await;
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

    async fn document_highlight(&self, _params: DocumentHighlightParams) -> Result<Option<Vec<DocumentHighlight>>> {
        // TODO is this needed anymore?
        warn!("Got document_highlight, will always return empty Vec, do we need it?");
        Ok(Some(Vec::new()))
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
        Ok(self.goto_declaration(params).await?)
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
        Logger::initialize(client.clone());
        Backend {
            client,
            client_configuration: Arc::new(RwLock::new(None)),
            file_store: Arc::new(RwLock::new(HashMap::new())),
        }
    });

    Server::new(stdin, stdout, socket).serve(service).await;
}
