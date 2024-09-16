// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

import {
	ExtensionContext,
	window,
} from "vscode";

import { BabbelaarContext } from "./babbelaarContext";
import { BabbelaarLsp } from "./babbelaarLsp";
import { BabbelaarDebugAdapter } from "./debugAdapter";
import { BabbelaarCommands } from "./commands";

export async function activate(context: ExtensionContext) {
	const babbelaarContext: BabbelaarContext = {
		ext: context,
		version: context.extension.packageJSON.version?.trim(),
	};

	if (!babbelaarContext.version || `${babbelaarContext.version}`.length === 0) {
		window.showErrorMessage("Ongeldige Babbelaar-extensie: versie is niet ingesteld.");
	}

	await BabbelaarDebugAdapter.register(babbelaarContext);
	await BabbelaarCommands.register(babbelaarContext);
	await BabbelaarLsp.startClient(babbelaarContext);
}

export function deactivate(): Thenable<void> {
	return BabbelaarLsp.stopClient();
}
