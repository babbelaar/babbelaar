// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

mod llvm;

pub use self::llvm::LlvmContext;

pub trait CompilerBackend {
    fn create_module(&self, name: &str);
}
