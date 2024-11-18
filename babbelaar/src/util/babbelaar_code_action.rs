// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use super::{BabbelaarCodeActionType, BabbelaarCommand, BabbelaarFixKind, FileEdit, FileRange};

#[derive(Debug, Clone)]
pub struct BabbelaarCodeAction {
    ty: BabbelaarCodeActionType,
    edits: Vec<FileEdit>,
    command: Option<BabbelaarCommand>,
    fix_kind: BabbelaarFixKind,
}

impl BabbelaarCodeAction {
    #[must_use]
    pub fn new(ty: BabbelaarCodeActionType, edits: Vec<FileEdit>) -> Self {
        Self {
            ty,
            edits,
            command: None,
            fix_kind: BabbelaarFixKind::QuickFix,
        }
    }

    #[must_use]
    pub fn new_command(range: FileRange, command: BabbelaarCommand) -> Self {
        Self {
            ty: BabbelaarCodeActionType::Command(command),
            edits: vec![FileEdit {
                replacement_range: range.start().as_zero_range(),
                new_text: String::new(),
                new_file_path: None,
            }],
            command: Some(command),
            fix_kind: command.fix_kind(),
        }
    }

    #[must_use]
    pub fn type_(&self) -> &BabbelaarCodeActionType {
        &self.ty
    }

    #[must_use]
    pub fn edits(&self) -> &[FileEdit] {
        &self.edits
    }

    #[must_use]
    pub fn command(&self) -> Option<&BabbelaarCommand> {
        self.command.as_ref()
    }

    #[must_use]
    pub fn with_command(self, command: impl Into<Option<BabbelaarCommand>>) -> Self {
        Self {
            command: command.into(),
            ..self
        }
    }

    #[must_use]
    pub fn with_fix_kind(self, fix_kind: impl Into<BabbelaarFixKind>) -> Self {
        Self {
            fix_kind: fix_kind.into(),
            ..self
        }
    }

    #[must_use]
    pub const fn fix_kind(&self) -> BabbelaarFixKind {
        self.fix_kind
    }
}
