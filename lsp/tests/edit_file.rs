// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{fs::{read_dir, read_to_string}, io::{stdout, BufRead, BufReader, Read, Write}, path::{Path, PathBuf}, process::{Child, ChildStdin, ChildStdout, Command, Stdio}, sync::{mpsc::{sync_channel, Receiver}, Arc}, thread::{spawn, yield_now}, time::Duration};

use anyhow::{ensure, Error, Result};
use babbelaar::Constants;
use dashmap::DashMap;
use tower_lsp::{jsonrpc::{self, Id}, lsp_types::{notification::*, request::*, *}};
use url::Url;

const LSP_EXECUTABLE_PATH: &str = env!("CARGO_BIN_EXE_babbelaar-lsp");

const TYPING_SPEED: Duration = Duration::from_millis(50);

#[test]
fn character_by_character() {
    let mut dir = std::env::current_dir().unwrap();
    dir.push("tests");
    dir.push("edit_file");

    for entry in read_dir(dir).unwrap().flatten() {
        if !entry.file_type().is_ok_and(|ty| ty.is_dir()) {
            continue;
        }

        character_by_character_for_dir(entry.path());
    }
}

fn character_by_character_for_dir(directory: PathBuf) {
    println!("Dir: {}", directory.display());
    let file = read_dir(&directory)
        .unwrap()
        .flatten()
        .map(|x| x.path())
        .filter(|x| {
            x.to_string_lossy().ends_with(".bab")
        })
        .next()
        .unwrap();

    let uri = file_uri(&file);
    let contents = read_to_string(&file).unwrap();

    let mut lsp = LspInterface::new(&directory);

    lsp.init();

    let mut version = 1;
    lsp.send_notification::<DidOpenTextDocument>(DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: Constants::LANGUAGE_ID.to_string(),
            version,
            text: String::new(),
        },
    });

    for (index, ch) in contents.char_indices() {
        let text = contents[..index].to_string();

        version += 1;

        lsp.send_notification::<DidChangeTextDocument>(DidChangeTextDocumentParams {
            content_changes: [
                TextDocumentContentChangeEvent {
                    range: None,
                    range_length: None,
                    text,
                }
            ].to_vec(),

            text_document: VersionedTextDocumentIdentifier {
                uri: uri.clone(),
                version,
            }
        });

        print!("{ch}");
        stdout().flush().unwrap();

        std::thread::sleep(TYPING_SPEED);
    }
}

struct LspInterface {
    process: Child,

    responses: Arc<DashMap<Id, jsonrpc::Response>>,
    input: ChildStdin,
    error_receiver: Receiver<Error>,

    state: LspState,
}

impl LspInterface {
    pub fn new(dir: &Path) -> Self {
        let mut process = Command::new(LSP_EXECUTABLE_PATH)
            .current_dir(dir)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .spawn()
            .unwrap();

        let output = process.stdout.take().unwrap();
        let output = BufReader::new(output);

        let responses = Arc::new(DashMap::new());

        let responses2 = Arc::clone(&responses);

        let (error_sink, error_receiver) = sync_channel(1);
        spawn(move || {
            let responses = responses2;
            let mut reader = LspReader {
                output,
                responses,
            };

            loop {
                if let Err(e) = reader.read() {
                    _ = error_sink.send(e);
                    return;
                }
            }
        });

        let input = process.stdin.take().unwrap();

        Self {
            process,
            error_receiver,
            responses,
            input,
            state: LspState::default(),
        }
    }

    pub fn init(&mut self) {
        self.send_request::<Initialize>(InitializeParams {
            ..Default::default()
        });

        self.send_notification::<Initialized>(InitializedParams {});
    }

    pub fn send_request<R: Request>(&mut self, body: R::Params) -> R::Result {
        let body = serde_json::to_value(body).unwrap();

        let id = self.state.request_id_counter;
        self.state.request_id_counter += 1;

        let request = jsonrpc::Request::build(R::METHOD)
            .id(id)
            .params(body)
            .finish();

        let request = serde_json::to_string(&request).unwrap();

        write!(&mut self.input, "Content-Length: {}\r\n\r\n{request}", request.len()).unwrap();

        self.input.flush().unwrap();

        let id = Id::Number(id);

        loop {
            if let Some(response) = self.responses.get(&id) {
                assert_eq!(response.error(), None);
                return serde_json::from_value(response.result().unwrap().clone()).unwrap();
            }

            if let Ok(error) = self.error_receiver.try_recv() {
                panic!("Receiver got an error: {error:#?}");
            }

            yield_now();
        }
    }

    pub fn send_notification<N: Notification>(&mut self, body: N::Params) {
        let body = serde_json::to_value(body).unwrap();

        let request = jsonrpc::Request::build(N::METHOD)
            .params(body)
            .finish();

        let request = serde_json::to_string(&request).unwrap();

        write!(&mut self.input, "Content-Length: {}\r\n\r\n{request}", request.len()).unwrap();

        self.input.flush().unwrap();
    }
}

impl Drop for LspInterface {
    fn drop(&mut self) {
        _ = self.process.kill();
    }
}

struct LspReader {
    output: BufReader<ChildStdout>,
    responses: Arc<DashMap<Id, jsonrpc::Response>>,
}

impl LspReader {
    fn read(&mut self) -> Result<()> {
        let length = self.read_content_length()?;
        self.read_empty_line()?;

        let response = self.read_exact_string(length)?;
        let response: serde_json::Value = serde_json::from_str(&response)?;

        if !response.as_object().unwrap().contains_key("id") {
            println!("Got notification...");
            return Ok(());
        }

        let response: jsonrpc::Response = serde_json::from_value(response)?;
        self.responses.insert(response.id().clone(), response);
        Ok(())
    }

    fn read_content_length(&mut self) -> Result<usize> {
        let mut length = String::new();
        self.output.read_line(&mut length)?;

        Ok(length.split(':')
                 .nth(1).ok_or_else(|| anyhow::anyhow!("Invalid line"))?
                 .trim()
                 .parse()?)
    }

    fn read_empty_line(&mut self) -> Result<()> {
        let mut empty = String::new();
        self.output.read_line(&mut empty)?;

        let empty = empty.trim();
        ensure!(empty.is_empty(), "{empty:?} is niet leeg");

        Ok(())
    }

    fn read_exact_string(&mut self, length: usize) -> Result<String> {
        let mut buf = Vec::new();
        buf.resize(length, 0);

        self.output.read_exact(&mut buf)?;

        Ok(String::from_utf8(buf)?)
    }
}

#[derive(Default)]
struct LspState {
    request_id_counter: i64,
}

fn file_uri<P: AsRef<Path>>(path: P) -> Uri {
    let url = Url::from_file_path(path.as_ref()).unwrap();
    url.to_string().parse().unwrap()
}
