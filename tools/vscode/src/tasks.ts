// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.


import { CancellationToken, ProviderResult, ShellExecution, Task, TaskProvider, tasks, TaskScope } from "vscode";

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

export { taskProvider };