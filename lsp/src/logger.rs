// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use log::{Level, LevelFilter, Log, Metadata};
use tokio::spawn;
use tower_lsp::{lsp_types::MessageType, Client};

pub(crate) struct Logger {
    client: Client,
}

impl Logger {
    pub fn initialize(client: Client) {
        let logger = Box::leak(Box::new(Logger {
            client: client.clone(),
        }));

        log::set_logger(logger).expect("Kon logger niet instellen");
    }
}

impl Log for Logger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        _ = metadata;
        log::set_max_level(LevelFilter::Trace);
        true
    }

    fn log(&self, record: &log::Record) {
        let typ = match record.level() {
            Level::Error => MessageType::ERROR,
            Level::Warn => MessageType::WARNING,
            Level::Info => MessageType::INFO,
            Level::Debug => MessageType::INFO,
            Level::Trace => MessageType::INFO,
        };

        let client = self.client.clone();
        let message = format!("[{}] {}: {}", std::thread::current().id().as_u64(), record.file().unwrap_or_default(), record.args());

        spawn(async move {
            client.log_message(typ, message).await;
        });
    }

    fn flush(&self) {}
}
