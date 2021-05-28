import * as vscode from "vscode";
import {TaskGroup} from "vscode";
import {State} from "./extension";
import {MosTaskDefinition} from "./build-task-provider";

export class RunAllTestsTaskProvider implements vscode.TaskProvider {
    private state: State;

    constructor(state: State) {
        this.state = state;
    }

    public provideTasks(): vscode.Task[] {
        const definition: MosTaskDefinition = {
            type: "shell"
        };
        const exec = new vscode.ShellExecution(this.state.mosPath, ['test']);
        const task = new vscode.Task(definition, this.state.workspaceFolder, "Run all tests", "mos", exec, ['$mos']);
        task.group = TaskGroup.Test;
        return [task];
    }

    public resolveTask(_task: vscode.Task): vscode.Task | undefined {
        return undefined;
    }
}