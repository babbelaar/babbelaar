// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::io::{stdin, stdout, BufReader, BufWriter, Stdin, Stdout};
use std::path::Path;
use std::process::exit;
use std::sync::mpsc::{channel, Receiver, Sender};
use std::sync::{Arc, Mutex};

use babbelaar::{Expression, FileRange, Interpreter, Ranged, Statement};
use dap::server::ServerOutput;
use dap::{errors::ServerError, prelude::*};
use dap::requests::InitializeArguments;
use events::{ExitedEventBody, StoppedEventBody};
use responses::{ContinueResponse, SetExceptionBreakpointsResponse, SetFunctionBreakpointsResponse, StackTraceResponse, ThreadsResponse};
use types::{Capabilities, FunctionBreakpoint, PresentationHint, Source, StackFrame, StoppedEventReason, Thread};

use crate::{Debugger, DebuggerFunction, DebuggerFunctionType};

pub struct DebugAdapter {
    path: String,
    communicator: DebugCommunicator,
    mode: DebugMode,
    stack_frames: Vec<DebugStackFrame>,
}

impl DebugAdapter {
    #[must_use]
    pub fn new(path: String) -> Self {
        let name = match Path::new(&path).file_name() {
            Some(x) => x.to_string_lossy().to_string(),
            None => path.clone(),
        };

        let stack_frames = vec![
            DebugStackFrame {
                ty: DebuggerFunctionType::Program,
                name,
                current_range: FileRange::default(),
                callee_range: FileRange::default(),
            }
        ];

        Self {
            path,
            communicator: DebugServer::start(),
            mode: DebugMode::Initial,
            stack_frames,
        }
    }

    fn handle_requests(&mut self) {
        self.read_commands();

        while !self.mode.can_continue() {
            self.read_commands();
        }
    }

    fn read_commands(&mut self) {
        while let Ok(command) = self.communicator.command_receiver.try_recv() {
            match command {
                DebuggerCommand::Mode(mode) => self.update_mode(mode),

                DebuggerCommand::StackTrace => self.send_stack_frame(),
            }
        }
    }

    fn update_mode(&mut self, mode: DebugMode) {
        self.mode = mode;

        if mode == DebugMode::Paused {
            self.communicator.send(Event::Stopped(StoppedEventBody {
                reason: StoppedEventReason::Pause,
                description: Some("Gepauzeerd".to_string()),
                thread_id: Some(current_thread_id()),
                preserve_focus_hint: None,
                text: Some("Pauzeee".into()),
                all_threads_stopped: Some(true),
                hit_breakpoint_ids: None,
            }));
        }
    }

    fn finish_handling(&mut self) {
        if self.mode == DebugMode::Step {
            // Set mode to paused, so when we check the next time it, we are
            // paused again and we actually single step.
            self.update_mode(DebugMode::Paused);
        }
    }

    fn send_stack_frame(&mut self) {
        let frames = self.stack_frames.iter()
            .enumerate()
            .rev()
            .map(|(idx, frame)| {
                let mut origin = None;
                let mut path = None;
                let mut presentation_hint = if idx == 0 {
                    Some(PresentationHint::Emphasize)
                } else {
                    Some(PresentationHint::Normal)
                };

                match frame.ty {
                    DebuggerFunctionType::Normal => {
                        path = Some(self.path.clone());
                    }

                    DebuggerFunctionType::Native => {
                        origin = Some("Ingebouwd".to_string());
                        presentation_hint = Some(PresentationHint::DeEmphasize);
                    }

                    DebuggerFunctionType::Program => {
                        origin = Some("Programma".to_string());
                    }
                }

                StackFrame {
                    id: idx as _, // TODO
                    name: frame.name.clone(),
                    source: Some(Source {
                        name: path.clone(),
                        path,
                        source_reference: None,
                        presentation_hint,
                        origin,
                        sources: None,
                        adapter_data: None,
                        checksums: None,
                    }),
                    line: frame.current_range.start().line() as i64 + 1,
                    column: frame.current_range.start().column() as i64 + 1,
                    end_line: Some(frame.current_range.end().line() as i64 + 1),
                    end_column: Some(frame.current_range.end().column() as i64 + 1),
                    can_restart: Some(false),
                    instruction_pointer_reference: None,
                    module_id: None,
                    presentation_hint: None,
                }
            })
            .collect();

        self.communicator.response_sender.send(ResponseBody::StackTrace(StackTraceResponse {
            stack_frames: frames,
            total_frames: None
        })).unwrap();
    }
}

impl Debugger for DebugAdapter {
    fn initialize(&mut self, _: &dyn Interpreter) {
        self.handle_requests();
    }

    fn enter_function(&mut self, function: DebuggerFunction<'_>, args: &[babbelaar::Value]) {
        self.handle_requests();
        _ = function;
        _ = args;

        self.stack_frames.push(DebugStackFrame {
            ty: function.ty,
            name: function.name.to_string(),
            callee_range: function.callee_location.unwrap_or_default(),
            current_range: function.caller_location,
        });
    }

    fn leave_function(&mut self, _: DebuggerFunction<'_>) {
        self.handle_requests();
        self.stack_frames.pop();
    }

    fn on_statement(&mut self, statement: &Statement<'_>) {
        self.handle_requests();

        let Some(frame) = self.stack_frames.last_mut() else {
            debug_assert!(false);
            return;
        };

        frame.current_range = statement.range;
        self.finish_handling();
    }

    fn on_expression(&mut self, expression: &Ranged<Expression<'_>>) {
        self.handle_requests();

        let Some(frame) = self.stack_frames.last_mut() else {
            debug_assert!(false);
            return;
        };

        frame.current_range = expression.range();
        self.finish_handling();
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DebugMode {
    Initial,
    Ready,
    Paused,
    Step,
}

impl DebugMode {
    #[must_use]
    const fn can_continue(&self) -> bool {
        matches!(self, Self::Ready | Self::Step)
    }
}

struct DebugCommunicator {
    output: Arc<Mutex<ServerOutput<Stdout>>>,
    response_sender: Sender<ResponseBody>,
    command_receiver: Receiver<DebuggerCommand>,
}

impl DebugCommunicator {
    pub fn send(&mut self, event: Event) {
        self.output.lock().unwrap().send_event(event).unwrap();
    }
}

impl Drop for DebugCommunicator {
    fn drop(&mut self) {
        self.send(Event::Exited(ExitedEventBody {
            exit_code: 0,
        }));
        self.send(Event::Terminated(None));
        exit(0);
    }
}

struct DebugServer {
    server: Server<Stdin, Stdout>,
    initialize_arguments: Option<InitializeArguments>,
    breakpoints: Vec<FunctionBreakpoint>,
    command_sender: Sender<DebuggerCommand>,
    response_receiver: Receiver<ResponseBody>,
}

impl DebugServer {
    #[must_use]
    pub fn start() -> DebugCommunicator {
        let (command_sender, command_receiver) = channel();
        let (response_sender, response_receiver) = channel();

        let this = Self {
            server: Server::new(BufReader::new(stdin()), BufWriter::new(stdout())),
            initialize_arguments: None,
            breakpoints: Vec::new(),
            command_sender,
            response_receiver,
        };

        let output = this.server.output.clone();
        this.start_in_background();

        DebugCommunicator {
            output,
            command_receiver,
            response_sender,
        }
    }

    fn start_in_background(mut self) {
        std::thread::spawn(move || {
            loop {
                self.handle_request().unwrap();
            }
        });
    }

    fn handle_request(&mut self) -> Result<(), ServerError> {
        let Some(request) = self.server.poll_request()? else {
            return Ok(());
        };

        match &request.command {
            Command::Initialize(args) => {
                debug_assert!(self.initialize_arguments.is_none());
                self.initialize_arguments = Some(args.clone());

                let response = request.success(ResponseBody::Initialize(Capabilities {
                    supports_function_breakpoints: Some(true),
                    supports_configuration_done_request: Some(true),
                    ..Default::default()
                }));

                self.server.respond(response)?;

                self.server.send_event(Event::Initialized)?;
            }

            Command::ConfigurationDone => {
                self.server.respond(request.success(ResponseBody::ConfigurationDone))?;
                self.command_sender.send(DebuggerCommand::Mode(DebugMode::Ready)).unwrap();
            }

            Command::Pause(..) => {
                self.command_sender.send(DebuggerCommand::Mode(DebugMode::Paused)).unwrap();
                self.server.respond(request.success(ResponseBody::Pause))?;
            }

            Command::Continue(..) => {
                self.command_sender.send(DebuggerCommand::Mode(DebugMode::Ready)).unwrap();
                self.server.respond(request.success(ResponseBody::Continue(ContinueResponse {
                    all_threads_continued: Some(true),
                })))?;
            }

            Command::Launch(launch) => {
                _ = launch;
                self.server.respond(request.success(ResponseBody::Launch))?;
            }

            Command::SetFunctionBreakpoints(breakpoints) => {
                self.breakpoints = breakpoints.breakpoints.clone();
                self.server.respond(request.success(ResponseBody::SetFunctionBreakpoints(SetFunctionBreakpointsResponse {
                    breakpoints: Vec::new(),
                })))?;
            }

            Command::SetExceptionBreakpoints(..) => {
                self.server.respond(request.success(ResponseBody::SetExceptionBreakpoints(SetExceptionBreakpointsResponse {
                    breakpoints: None,
                })))?;
            }

            Command::Threads => {
                let thread = Thread {
                    id: current_thread_id(),
                    name: std::thread::current().name().unwrap_or_default().to_string(),
                };
                self.server.respond(request.success(ResponseBody::Threads(ThreadsResponse {
                    threads: vec![thread],
                })))?;
            }

            Command::StackTrace(args) => {
                _ = args; // TODO

                self.command_sender.send(DebuggerCommand::StackTrace).unwrap();

                let response = self.response_receiver.recv().unwrap();
                debug_assert!(matches!(response, ResponseBody::StackTrace(..)));

                self.server.respond(request.success(response))?;
            }

            Command::Next(..) => {
                self.command_sender.send(DebuggerCommand::Mode(DebugMode::Step)).unwrap();
                self.server.respond(request.success(ResponseBody::Next))?;
            }

            _ => {
                let msg = format!("Unknown: {request:#?}");
                self.server.respond(request.error(&msg))?;
            }
        }

        Ok(())
    }
}

enum DebuggerCommand {
    Mode(DebugMode),
    StackTrace,
}

fn current_thread_id() -> i64 {
    let thread = std::thread::current();
    let id: u64 = thread.id().as_u64().into();
    id as i64
}

struct DebugStackFrame {
    ty: DebuggerFunctionType,
    name: String,
    current_range: FileRange,

    #[allow(unused)] // TODO
    callee_range: FileRange,
}
