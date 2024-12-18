// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

mod ldd;
mod linker;

pub use self::{
    ldd::Ldd,
    linker::LinuxGccLinker,
};
