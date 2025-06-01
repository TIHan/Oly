import path = require('path');
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
	let textEditor = vscode.window.activeTextEditor;
	let document = textEditor?.document;
	let cursorPosition = textEditor?.selection.start;
	return { document, cursorPosition };
}

export async function autoCreateLaunchJson() {
	let olyLaunchPath = '.vscode/launch.json';
	let olyLaunchUri = vscode.Uri.file(path.join(vscode.workspace.workspaceFolders[0].uri.path, olyLaunchPath));

	let defaultLaunchJson =
	`{
	"version": "0.2.0",
	"configurations": [
		{
			"name": ".NET Core Launch (console)",
			"type": "coreclr",
			"request": "launch",
			"program": "\${command:oly.compile}",
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

	try
	{
		let _ = await vscode.workspace.fs.stat(olyLaunchUri); // test if the file exists
	}
	catch
	{
		await vscode.workspace.fs.writeFile(olyLaunchUri, new TextEncoder().encode(defaultLaunchJson));
	}
}
