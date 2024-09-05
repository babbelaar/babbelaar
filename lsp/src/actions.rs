// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::collections::HashMap;

use babbelaar::BabbelaarCodeAction;
use tower_lsp::lsp_types::VersionedTextDocumentIdentifier;

#[derive(Debug)]
pub struct CodeActionItem {
    pub action: BabbelaarCodeAction,
    pub document: VersionedTextDocumentIdentifier,
}

#[derive(Debug)]
pub struct CodeActionRepository {
    id_tracker: usize,

    /// TODO: find some way to invalidate & free the code actions when they're not used anymore,
    ///       to avoid memory exhaustion. E.g. when the document has x amount of edits, we discard
    ///       the old code actions.
    values: HashMap<usize, CodeActionItem>,
}

impl CodeActionRepository {
    pub fn new() -> Self {
        Self {
            id_tracker: 1,
            values: HashMap::new(),
        }
    }

    #[must_use]
    pub fn add(&mut self, action: BabbelaarCodeAction, document: VersionedTextDocumentIdentifier) -> usize {
        let id = self.id_tracker;
        self.id_tracker += 1;
        self.values.insert(id, CodeActionItem {
            action,
            document,
        });
        id
    }

    #[must_use]
    pub fn get(&self, id: usize) -> Option<&CodeActionItem> {
        self.values.get(&id)
    }
}
