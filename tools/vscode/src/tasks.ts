// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.


import { CancellationToken, ProviderResult, ShellExecution, Task, TaskProvider, tasks, TaskScope } from "vscode";
import { ensureCli } from "./downloadBabbelaar";
import { BabbelaarContext } from "./babbelaarContext";

class BabbelaarTaskProvider implements TaskProvider<Task> {
	private babbelaar: BabbelaarContext;

	constructor(babbelaar: BabbelaarContext) {
		this.babbelaar = babbelaar;
	}

	provideTasks(token: CancellationToken): ProviderResult<Task[]> {
		console.log(`Tasks token ${token}`);
		// throw new Error("Method not implemented.");
		return [];
	}

	async createBuild(path: string): Promise<Task> {
		const task = new Task(
			{
				type: "babbelaar",
				path,
			},
			TaskScope.Workspace,
			path,
			"babbelaar",
		);
		return this.resolveTaskImpl(task, "bouwen");
	}

	async createRun(path: string): Promise<Task> {
		const task = new Task(
			{
				type: "babbelaar",
				path,
			},
			TaskScope.Workspace,
			path,
			"babbelaar",
		);
		return this.resolveTaskImpl(task, "uitvoeren");
	}

	resolveTask(task: Task, _token: CancellationToken): ProviderResult<Task> {
		return this.resolveTaskImpl(task, "uitvoeren");
	}

	async resolveTaskImpl(task: Task, subcommand: string): Promise<Task> {
		const command = await ensureCli(this.babbelaar) ?? "babbelaar";

		const path = task.definition["path"] as string;
		const execution = new ShellExecution(`clear; \"${command.replaceAll(/(?<!\\)"/g, "\"")}\" ${subcommand} \"${path.replaceAll(/(?<!\\)"/g, "\"")}\"`, {
			env: {
				"RUST_BACKTRACE": "1"
			}
		});
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

async function registerTaskProvider(context: BabbelaarContext) {
	const taskProvider = new BabbelaarTaskProvider(context);
	const taskProviderSubscription = tasks.registerTaskProvider("babbelaar", taskProvider);
	context.taskProvider = taskProvider;
	await ensureCli(context);
}

export { BabbelaarTaskProvider, registerTaskProvider, };