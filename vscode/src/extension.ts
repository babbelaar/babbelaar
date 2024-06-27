/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import {
	languages,
	workspace,
	EventEmitter,
	ExtensionContext,
	window,
	InlayHintsProvider,
	TextDocument,
	CancellationToken,
	Range,
	InlayHint,
	TextDocumentChangeEvent,
	ProviderResult,
	commands,
	WorkspaceEdit,
	TextEdit,
	Selection,
	Uri,
	tasks,
	Task,
	TaskProvider,
	ShellExecution,
	TaskScope,
	debug,
	DebugConfigurationProviderTriggerKind,
	DebugConfigurationProvider,
	WorkspaceFolder,
	DebugConfiguration,
	DebugAdapterDescriptorFactory,
	DebugSession,
	DebugAdapterExecutable,
	DebugAdapterDescriptor,
} from "vscode";

import {
	Disposable,
	Executable,
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient;
// type a = Parameters<>;


class BabbelaarTaskProvider implements TaskProvider<Task> {
	provideTasks(token: CancellationToken): ProviderResult<Task[]> {
		console.log(`Tasks token ${token}`);
		// throw new Error("Method not implemented.");
		return [];
	}

	createRun(path: string): Task {
		const task = new Task(
			{
				type: "babbelaar",
				path,
			},
			TaskScope.Workspace,
			path,
			"babbelaar",
		);
		return this.resolveTaskImpl(task);
	}

	resolveTask(task: Task, _token: CancellationToken): ProviderResult<Task> {
		return this.resolveTaskImpl(task);
	}

	resolveTaskImpl(task: Task): Task {
		const command = process.env.BABBELAAR || "babbelaar";

		const path = task.definition["path"] as string;
		const execution = new ShellExecution(`clear; ${command} uitvoeren ${path}`);
		console.log(JSON.stringify(task, null, ''));
		const definition = task.definition;

		return new Task(
			definition,
			task.scope ?? TaskScope.Workspace,
			path,
			'babbelaar',
			execution
		  );
	}
}

const taskProvider = new BabbelaarTaskProvider();
const taskProviderSubscription = tasks.registerTaskProvider("babbelaar", taskProvider);

export async function activate(context: ExtensionContext) {
	let disposable = commands.registerCommand("babbelaar.herstarten", async => {
		startClient();
	});

	debug.registerDebugAdapterTrackerFactory('*', {
		createDebugAdapterTracker(session: DebugSession) {
		  return {
			onWillReceiveMessage: m => console.log(`> ${JSON.stringify(m, undefined, 2)} session ${JSON.stringify(session, undefined, 2)}`),
			onDidSendMessage: m => console.log(`< ${JSON.stringify(m, undefined, 2)}`)
		  };
		}
	  });

	const configProvider = new BabbelaarDebugConfigurationProvider();
	const debugConfigProvider = debug.registerDebugConfigurationProvider("babbelaar", configProvider, DebugConfigurationProviderTriggerKind.Dynamic);
	context.subscriptions.push(debugConfigProvider);
	context.subscriptions.push(debug.registerDebugAdapterDescriptorFactory("babbelaar", new DebugAdapterExecutableFactory()));

	context.subscriptions.push(disposable);

	disposable = commands.registerCommand("babbelaar.uitvoeren", async uri => {
		const path = workspace.textDocuments[0].uri.fsPath;
		tasks.executeTask(taskProvider.createRun(path));
	});

	context.subscriptions.push(disposable);

	startClient();
}

function startClient() {
	if (client) {
		client.stop();
	}

	const traceOutputChannel = window.createOutputChannel("Babbelaar Taalserveerder trace");
	const command = process.env.SERVER_PATH || "babbelaar-lsp";
	const run: Executable = {
		command,
		options: {
			env: {
				...process.env,
				// eslint-disable-next-line @typescript-eslint/naming-convention
				RUST_LOG: "debug",
			},
		},
	};
	const serverOptions: ServerOptions = {
		run,
		debug: run,
	};
	// If the extension is launched in debug mode then the debug server options are used
	// Otherwise the run options are used
	// Options to control the language client
	let clientOptions: LanguageClientOptions = {
		// Register the server for plain text documents
		documentSelector: [{ scheme: "file", language: "babbelaar" }],
		synchronize: {
			// Notify the server about file changes to '.clientrc files contained in the workspace
			fileEvents: workspace.createFileSystemWatcher("**/.clientrc"),
		},
		traceOutputChannel,
	};

	// Create the language client and start the client.
	client = new LanguageClient("babbelaar-lsp", "babbelaar-lsp", serverOptions, clientOptions);
	// activateInlayHints(context);
	client.start();
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}

export function activateInlayHints(ctx: ExtensionContext) {
	const maybeUpdater = {
		hintsProvider: null as Disposable | null,
		updateHintsEventEmitter: new EventEmitter<void>(),

		async onConfigChange() {
			this.dispose();

			const event = this.updateHintsEventEmitter.event;
			this.hintsProvider = languages.registerInlayHintsProvider(
			  { scheme: "file", language: "babbelaar" },
			  new (class implements InlayHintsProvider {
			    onDidChangeInlayHints = event;
			    resolveInlayHint(hint: InlayHint, token: CancellationToken): ProviderResult<InlayHint> {
			      const ret = {
			        ...hint,
			        label: hint.label,
			      };
			      return ret;
			    }
			    async provideInlayHints(
			      document: TextDocument,
			      range: Range,
			      token: CancellationToken
			    ): Promise<InlayHint[]> {
			      const hints = (await client
			        .sendRequest("custom/inlay_hint", { path: document.uri.toString() })
			        .catch(err => null)) as [number, number, string][];
			      if (hints == null) {
			        return [];
			      } else {
			        return hints.map(item => {
			          const [start, end, label] = item;
			          let startPosition = document.positionAt(start);
			          let endPosition = document.positionAt(end);
			          return {
			            position: endPosition,
			            paddingLeft: true,
			            label: [
			              {
			                value: `${label}`,
			                // location: {
			                //   uri: document.uri,
			                //   range: new Range(1, 0, 1, 0)
			                // }
			                command: {
			                  title: "hello world",
			                  command: "helloworld.helloWorld",
			                  arguments: [document.uri],
			                },
			              },
			            ],
			          };
			        });
			      }
			    }
			  })()
			);
		},

		onDidChangeTextDocument({ contentChanges, document }: TextDocumentChangeEvent) {
			// debugger
			// this.updateHintsEventEmitter.fire();
		},

		dispose() {
			this.hintsProvider?.dispose();
			this.hintsProvider = null;
			this.updateHintsEventEmitter.dispose();
		},
	};

	workspace.onDidChangeConfiguration(maybeUpdater.onConfigChange, maybeUpdater, ctx.subscriptions);
	workspace.onDidChangeTextDocument(maybeUpdater.onDidChangeTextDocument, maybeUpdater, ctx.subscriptions);

	maybeUpdater.onConfigChange().catch(console.error);
}


class DebugAdapterExecutableFactory implements DebugAdapterDescriptorFactory {

	// The following use of a DebugAdapter factory shows how to control what debug adapter executable is used.
	// Since the code implements the default behavior, it is absolutely not neccessary and we show it here only for educational purpose.

	createDebugAdapterDescriptor(_session: DebugSession, executable: DebugAdapterExecutable | undefined): ProviderResult<DebugAdapterDescriptor> {
		// param "executable" contains the executable optionally specified in the package.json (if any)

		// use the executable specified in the package.json if it exists or determine it based on some other information (e.g. the session)
		// if (!executable) {
			const command = "bash";
			const args = [
				"-c",
				`/Users/tager/Developer/Public/Babbelaar/target/debug/babbelaar-interpreter debug ${window.activeTextEditor!.document.fileName} 2> /Users/tager/babout`,
			];
			const options = {
				// cwd: "working directory for executable",
				env: { "envVariable": "some value" }
			};
			executable = new DebugAdapterExecutable(command, args, options);
		// }

		// make VS Code launch the DA executable
		return executable;
	}
}


class BabbelaarDebugConfigurationProvider implements DebugConfigurationProvider {
	resolveDebugConfiguration(folder: WorkspaceFolder | undefined, config: DebugConfiguration, token?: CancellationToken): ProviderResult<DebugConfiguration> {

		// if launch.json is missing or empty
		if (!config.type && !config.request && !config.name) {
			const editor = window.activeTextEditor;
			if (editor && editor.document.languageId === 'babbelaar') {
				config.type = 'babbelaar';
				config.name = 'Launch';
				config.request = 'launch';
				config.program = "${file}";
				config.stopOnEntry = true;
			}
		}

		if (!config.program) {
			return window.showInformationMessage("Cannot find a program to debug: " + JSON.stringify(config)).then(_ => {
				return undefined;	// abort launch
			});
		}

		return config;
	}
}
