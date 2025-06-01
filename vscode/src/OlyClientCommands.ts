import * as vscode from 'vscode';

export namespace OlyClientCommands {
	export const navigateToSyntaxNodeCommand = `oly.navigateToSyntaxNode`;
	export const getSyntaxTreeCommand = `oly.getSyntaxTree`;
	export const compileCommand = `oly.compile`;
	export const changeActiveProject = `oly.changeActiveProject`;
	export const changeActiveConfiguration = `oly.changeActiveConfiguration`;
	export const cleanWorkspace = `oly.cleanWorkspace`;
	export const debug = `workbench.action.debug.start`;
	export const run = `workbench.action.debug.run`;
	export const compileOutputChannel = vscode.window.createOutputChannel("Oly Compilation");
}
