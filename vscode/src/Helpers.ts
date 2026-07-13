import * as path from 'path';
import * as vscode from 'vscode';

export function sleep(ms: number) {
	return new Promise((resolve) => {
		setTimeout(resolve, ms);
	});
}

export function getActiveDocument() {
	return vscode.window.activeTextEditor?.document;
}

export function getActiveDocumentAndCursorPosition() {
	const textEditor = vscode.window.activeTextEditor;
	const document = textEditor?.document;
	const cursorPosition = textEditor?.selection.start;
	return { document, cursorPosition };
}

export async function autoCreateLaunchJson() {
	if (!vscode.workspace.workspaceFolders)
		throw "No workspace folders found."

	const olyLaunchPath = '.vscode/launch.json';
	const olyLaunchUri = vscode.Uri.file(path.join(vscode.workspace.workspaceFolders[0].uri.path, olyLaunchPath));

	const defaultLaunchJson =
		`{
	"version": "0.2.0",
	"configurations": [
		{
			"name": ".NET Core Launch (console)",
			"type": "coreclr",
			"request": "launch",
			"program": "\${command:oly.build}",
			"args": [],
			"cwd": "\${workspaceFolder}",
			"console": "internalConsole",
			"stopAtEntry": false
		},
		{
			"name": ".NET Core Attach",
			"type": "coreclr",
			"request": "attach"
		}
	]
}`

	try {
		await vscode.workspace.fs.stat(olyLaunchUri); // test if the file exists
	}
	catch {
		await vscode.workspace.fs.writeFile(olyLaunchUri, new TextEncoder().encode(defaultLaunchJson));
	}
}
