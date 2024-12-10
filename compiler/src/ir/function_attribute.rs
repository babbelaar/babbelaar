// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum FunctionAttribute {
    ToRemove,

    VarArgs {
        /// After how many "normal" params does the var args start?
        after_n_normal_params: usize,
    },
}

pub trait FunctionAttributesExt {
    fn var_args_after_n_normal_params(&self) -> Option<usize>;
}

impl FunctionAttributesExt for &[FunctionAttribute] {
    fn var_args_after_n_normal_params(&self) -> Option<usize> {
        for attr in self.iter() {
            if let FunctionAttribute::VarArgs { after_n_normal_params } = attr {
                return Some(*after_n_normal_params);
            }
        }

        None
    }
}
