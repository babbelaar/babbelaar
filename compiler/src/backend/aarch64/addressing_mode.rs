// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

/// Represents the different addressing modes for memory operations like `STR`.
///
/// These modes determine how an offset is applied to the base register when accessing memory.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(unused)]
pub enum ArmSignedAddressingMode {
    /// Post-index addressing: the offset is applied after the store operation.
    ///
    /// Example:
    /// `STR x0, [sp], #8` - Stores the value in `x0` to `sp`, then updates `sp` by `8`.
    PostIndex,

    /// Pre-index addressing: the offset is applied before the store operation.
    ///
    /// Example:
    /// `STR x0, [sp, #8]!` - Updates `sp` by `8`, then stores `x0` at the new `sp` address.
    PreIndex,

    /// Signed offset addressing: the offset is applied without modifying the base register.
    ///
    /// Example:
    /// `STR x0, [sp, #8]` - Stores `x0` at `sp + 8`, leaving `sp` unchanged.
    SignedOffset,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(unused)]
pub enum ArmUnsignedAddressingMode {
    PostIndex,

    PreIndex,

    UnsignedOffset,
}
