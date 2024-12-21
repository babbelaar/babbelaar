// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::ops::Range;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RegisterLifetime {
    first_use: usize,
    last_use: usize,
    times_used_as_return: usize,
    times_used_between_calls: usize,
    last_loop_index: Option<usize>,
}

impl RegisterLifetime {
    #[must_use]
    pub fn new(index: usize) -> Self {
        Self {
            first_use: index,
            last_use: index,
            times_used_as_return: 0,
            times_used_between_calls: 0,
            last_loop_index: None,
        }
    }

    #[must_use]
    #[allow(unused)]
    pub fn length(&self) -> usize {
        self.last_use - self.first_use
    }

    #[must_use]
    pub fn first_use(&self) -> usize {
        self.first_use
    }

    #[must_use]
    pub fn last_use(&self) -> usize {
        self.last_use
    }

    #[must_use]
    pub fn times_used_as_return(&self) -> usize {
        self.times_used_as_return
    }

    #[must_use]
    pub fn times_used_between_calls(&self) -> usize {
        self.times_used_between_calls
    }

    #[must_use]
    pub fn was_used_during_loops(&self) -> bool {
        self.last_loop_index.is_some()
    }

    #[must_use]
    pub fn last_loop_index(&self) -> Option<usize> {
        self.last_loop_index
    }

    pub fn did_use_for_return(&mut self) {
        self.times_used_as_return += 1;
    }

    pub fn did_use_between_calls(&mut self) {
        self.times_used_between_calls += 1;
    }

    pub fn did_use_during_loop(&mut self, range: Range<usize>) {
        self.last_loop_index = self.last_loop_index
            .map(|index| index.max(range.end))
            .or(Some(range.end));
    }

    pub fn did_use_at(&mut self, index: usize) {
        debug_assert!(self.last_use <= index);
        self.last_use = index;
    }

    #[must_use]
    pub fn is_active_at(&self, index: usize) -> bool {
        (self.first_use()..=self.last_use()).contains(&index)
    }
}
