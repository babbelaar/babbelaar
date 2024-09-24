// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use log::{LevelFilter, Log, Metadata, Record};

pub(crate) struct Logger;

impl Logger {
    pub fn initialize() {
        let logger = Box::leak(Box::new(Logger));

        log::set_max_level(LevelFilter::Trace);
        log::set_logger(logger).expect("Kon logger niet instellen");
        log::info!("Logger is ingesteld");
    }
}

impl Log for Logger {
    fn enabled(&self, metadata: &Metadata<'_>) -> bool {
        _ = metadata;
        log::set_max_level(LevelFilter::Trace);
        true
    }

    fn log(&self, record: &Record<'_>) {
        eprintln!("[{}] [{}] {}: {}", record.level(), std::thread::current().id().as_u64(), record.file().unwrap_or_default(), record.args());
    }

    fn flush(&self) {}
}
