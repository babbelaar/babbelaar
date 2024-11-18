// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

pub trait StrIterExt: Sized {
    fn join(self, str: &str) -> String;
}

impl<'s, T> StrIterExt for T
        where T: Iterator<Item = &'s str> + 's {
    fn join(mut self, str: &str) -> String {
        let mut s = String::new();

        while let Some(item) = self.next() {
            if !s.is_empty() {
                s += str;
            }

            s += item;
        }

        s
    }
}
