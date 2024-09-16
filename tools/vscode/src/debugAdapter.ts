// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

export { BabbelaarDebugAdapter };

import {
	window,
	CancellationToken,
	ProviderResult,
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

import { BabbelaarContext } from "./babbelaarContext";

const BabbelaarDebugAdapter = {

    register: async (context: BabbelaarContext) => {

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
        context.ext.subscriptions.push(debugConfigProvider);
        context.ext.subscriptions.push(debug.registerDebugAdapterDescriptorFactory("babbelaar", new DebugAdapterExecutableFactory()));
    }
};

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
