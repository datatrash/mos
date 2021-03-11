import * as vscode from "vscode";
import {TaskGroup, TaskScope} from "vscode";
import {State} from "./extension";
import {log} from "./log";

interface MosTaskDefinition extends vscode.TaskDefinition {
    type: string
}

export class BuildTaskProvider implements vscode.TaskProvider {
    private state: State;

    constructor(state: State) {
        this.state = state;
    }

    public provideTasks(): vscode.Task[] {
        const definition: MosTaskDefinition = {
            type: "build"
        };
        const exec = new vscode.ShellExecution(`${this.state.mosPath} build`);
        const task = new vscode.Task(definition, this.state.workspaceFolder, "Build", "mos", exec, ['$mos']);
        task.group = TaskGroup.Build;
        return [task];
    }

    public resolveTask(_task: vscode.Task): vscode.Task | undefined {
        return undefined;
    }
}