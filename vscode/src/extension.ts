import * as path from 'path';
import * as vscode from 'vscode';
import { ExtensionContext, workspace } from 'vscode';
import {
	LanguageClientOptions,
	ServerOptions,
	Trace
} from 'vscode-languageclient/node';
import { ProgressToken, WorkDoneProgress, WorkDoneProgressBegin, WorkDoneProgressEnd, WorkDoneProgressReport } from 'vscode-languageserver-protocol';
import { OlyTextPosition } from './IOlySyntaxTreeViewModel';
import { OlyLanguageClient } from './OlyLanguageClient';
import { OlyClientCommands } from './OlyClientCommands';
import { OlySyntaxTreeView } from './OlySyntaxTreeView';

export let client: OlyLanguageClient;
export let isClientReady: boolean = false;

async function autoCreateLaunchJson() {
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
		let _ = await vscode.workspace.fs.stat(olyLaunchUri)
	}
	catch
	{
		await vscode.workspace.fs.writeFile(olyLaunchUri, new TextEncoder().encode(defaultLaunchJson));
	}
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

class DocumentRangeSemanticTokensProvider implements vscode.DocumentRangeSemanticTokensProvider {
	async provideDocumentRangeSemanticTokens(document: vscode.TextDocument, range: vscode.Range, token: vscode.CancellationToken): Promise<vscode.SemanticTokens> {
		return await client.getSemanticClassification(document, range, token);
	}
}

export function activate(context: ExtensionContext) {
	let serverModule = context.asAbsolutePath(
		path.join('out', 'net8', 'Oly.LanguageServer.dll')
	);

	// If the extension is launched in debug mode then the debug server options are used
	// Otherwise the run options are used
	let serverOptions: ServerOptions = {
		run: { command: 'dotnet', args: [serverModule] },
        debug: { command: 'dotnet', args: [serverModule] }
	};

	// Options to control the language client
	let clientOptions: LanguageClientOptions = {
		// Register the server for plain text documents
		documentSelector: [{ scheme: 'file', language: 'oly' }],
		synchronize: {
			fileEvents: [workspace.createFileSystemWatcher('**/*.oly'), workspace.createFileSystemWatcher('**/*.olyx')]
		}
	};

	// Create the language client and start the client.
	client = new OlyLanguageClient(
		'olyLanguageServer',
		'Oly Language Server',
		serverOptions,
		clientOptions
	);

	client.trace = Trace.Verbose;

	// View initializations
	let syntaxTreeView = OlySyntaxTreeView.createFromVscodeWindow();

	let olyWorkspaceStatusDefaultText = "Oly Workspace";
	let olyWorkspaceStatusSyncText = "$(sync~spin) Oly Workspace"
	let olyWorkspaceStatusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 100);
	olyWorkspaceStatusBarItem.text = olyWorkspaceStatusDefaultText;
	olyWorkspaceStatusBarItem.color = new vscode.ThemeColor("testing.iconPassed");

	function resetOlyWorkspaceStatusBarTooltip() {
		olyWorkspaceStatusBarItem.tooltip = new vscode.MarkdownString("", true);
		olyWorkspaceStatusBarItem.tooltip.isTrusted = true;
		olyWorkspaceStatusBarItem.tooltip.appendMarkdown('[$(clear-all) Clean](command:oly.cleanWorkspace "Clean workspace")\n\n');
	}

	resetOlyWorkspaceStatusBarTooltip();
	olyWorkspaceStatusBarItem.show();

	let olyBuildingStatusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 99);
	olyBuildingStatusBarItem.text = "$(loading~spin) Building...";

	let olyProjectStatusDefaultText = "Oly Project";
	let olyProjectStatusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 98);
	olyProjectStatusBarItem.text = olyProjectStatusDefaultText;
	olyProjectStatusBarItem.show();

	function handleProgressNotification(params: WorkDoneProgressBegin | WorkDoneProgressReport | WorkDoneProgressEnd) {
		switch (params.kind) {
			case 'begin':
				if (params.message == "building")
				{
					olyBuildingStatusBarItem.show();
				}
				else
				{
					olyWorkspaceStatusBarItem.text = olyWorkspaceStatusSyncText;
					olyWorkspaceStatusBarItem.tooltip = "Analyzing";
					olyWorkspaceStatusBarItem.color = new vscode.ThemeColor("testing.iconQueued");
				}
				break;
			case 'end':
				if (params.message == "building")
				{
					olyBuildingStatusBarItem.hide();
				}
				else
				{
					olyWorkspaceStatusBarItem.text = olyWorkspaceStatusDefaultText;
					olyWorkspaceStatusBarItem.color = new vscode.ThemeColor("testing.iconPassed");
					resetOlyWorkspaceStatusBarTooltip();
				}
				break;
		}
	}

	function registerStatusBarItems() {
		client.onProgress(WorkDoneProgress.type, 'oly/analysis', handleProgressNotification);
	}

	client.onReady().then(async () => {
		await autoCreateLaunchJson();

		isClientReady = true;
		registerStatusBarItems();
		let config = vscode.workspace.getConfiguration("olyLanguageServer");
		await client.didChangeWorkspaceConfiguration(config);
		vscode.workspace.onDidChangeConfiguration(async (_e) => {
			let config = vscode.workspace.getConfiguration("olyLanguageServer");
			client.didChangeWorkspaceConfiguration(config);		
		});

		vscode.window.onDidChangeActiveTextEditor(async (e) => {
			if (e?.document?.languageId === 'oly') {
				await syntaxTreeView.refresh(e.document, null);
			}
		});

		vscode.workspace.onDidChangeTextDocument(async (e) => {
			if (e?.document?.languageId === 'oly' && (getActiveDocument()) == e.document) {
				let cursorPosition = e.contentChanges[0].range.start;
				let pos = OlyTextPosition.fromVscodePosition(cursorPosition);
				await syntaxTreeView.refresh(e.document, pos);
			}
		});

		context.subscriptions.push(vscode.languages.registerDocumentRangeSemanticTokensProvider({ language: 'oly'}, new DocumentRangeSemanticTokensProvider(), OlyLanguageClient.legend));
		OlySyntaxTreeView.register(context, syntaxTreeView);

		async function compileCommandHandler() {
			let ch = OlyClientCommands.compileOutputChannel;
			
			ch.show(true);
			let result = await OlyClientCommands.compile(client, olyProjectStatusBarItem, ch);
			if (result == null)
			{
				throw new Error("Oly Compilation Failed");
			}
			else if (result.resultPath == null)
			{
				ch.appendLine("========================================================\n");
				ch.append(result.error);
				throw new Error("Oly Compilation Failed");
			}

			return result.resultPath;
		}

		context.subscriptions.push(vscode.commands.registerCommand('oly.compile', compileCommandHandler));

		// Oly Workspace Settings
		let olyWorkspaceSettingsPath = '.olyworkspace/settings.json';
		let olyWorkspaceSettingsUri = vscode.Uri.file(path.join(vscode.workspace.workspaceFolders[0].uri.path, olyWorkspaceSettingsPath));
		async function readOlyWorkspaceSettings() {
			// TODO: How do we handle a blank active project?
			try
			{
				let fileContents: Uint8Array = await vscode.workspace.fs.readFile(olyWorkspaceSettingsUri);
				let stringContents = new TextDecoder().decode(fileContents);
				return JSON.parse(stringContents);
			}
			catch
			{
				let olyWorkspaceSettings: any = {};
				olyWorkspaceSettings.activeProject = "";
				let projectNames = await client.getProjectList();
				if (projectNames.length > 0)
				{
					olyWorkspaceSettings.activeProject = projectNames[0];
				}

				await saveOlyWorkspaceSettings(olyWorkspaceSettings);
				return await readOlyWorkspaceSettings();
			}
		}
		async function saveOlyWorkspaceSettings(olyWorkspaceSettings) {
			await vscode.workspace.fs.writeFile(olyWorkspaceSettingsUri, new TextEncoder().encode(JSON.stringify(olyWorkspaceSettings)));
		}

		// Oly Workspace State
		let olyWorkspaceStatePath = '.olyworkspace/state.json';
		let olyWorkspaceStateUri = vscode.Uri.file(path.join(vscode.workspace.workspaceFolders[0].uri.path, olyWorkspaceStatePath));
		async function readOlyWorkspaceState() {
			try
			{
				let fileContents: Uint8Array = await vscode.workspace.fs.readFile(olyWorkspaceStateUri);
				let stringContents = new TextDecoder().decode(fileContents);
				return JSON.parse(stringContents);
			}
			catch
			{
				let olyWorkspaceState: any = {};
				olyWorkspaceState.activeConfiguration = "Debug";
				saveOlyWorkspaceState(olyWorkspaceState);
				return readOlyWorkspaceState();
			}
		}
		async function saveOlyWorkspaceState(olyWorkspaceState) {
			await vscode.workspace.fs.writeFile(olyWorkspaceStateUri, new TextEncoder().encode(JSON.stringify(olyWorkspaceState)));
		}

		async function refreshProjectStatusBarItemTooltip() {
			var workspaceVscode = await readOlyWorkspaceSettings();

			var projName = "(none selected)";
			if (workspaceVscode.activeProject !== null && workspaceVscode.activeProject !== undefined)
			{
				projName = workspaceVscode.activeProject;
			}
			
			var olyWorkspaceState = await readOlyWorkspaceState();
			var configName = "";
			if (olyWorkspaceState.activeConfiguration !== null && olyWorkspaceState.activeConfiguration !== undefined)
			{
				configName = olyWorkspaceState.activeConfiguration;
			}		

			olyProjectStatusBarItem.tooltip = new vscode.MarkdownString("", true);
			olyProjectStatusBarItem.tooltip.isTrusted = true;
			olyProjectStatusBarItem.tooltip.appendMarkdown("Active Project: " + projName + " (" + configName + ")\n\n");
			olyProjectStatusBarItem.tooltip.appendMarkdown(`[$(project) Compile](command:oly.compile "Compile active project")\n\n`);
			olyProjectStatusBarItem.tooltip.appendMarkdown(`[$(debug-alt) Debug](command:workbench.action.debug.start "Debug active project")\n\n`);
			olyProjectStatusBarItem.tooltip.appendMarkdown(`[$(debug-start) Run](command:workbench.action.debug.run "Run active project")\n\n`);
			olyProjectStatusBarItem.tooltip.appendMarkdown(`[$(settings-gear) Change Active Project](command:oly.changeActiveProject "Change active project")\n\n`);
			olyProjectStatusBarItem.tooltip.appendMarkdown(`[$(settings-gear) Change Active Configuration](command:oly.changeActiveConfiguration "Change active configuration")\n\n`);
			olyProjectStatusBarItem.text = olyProjectStatusDefaultText + ": " + projName + " (" + configName + ")";
		}
		refreshProjectStatusBarItemTooltip();

		let stateWatcher = workspace.createFileSystemWatcher('**/*.json');
		stateWatcher.onDidChange(function(event) {
			if (event.path.endsWith(olyWorkspaceSettingsPath) || event.path.endsWith(olyWorkspaceStatePath))
			{
				refreshProjectStatusBarItemTooltip();
			}
		});
		stateWatcher.onDidCreate(function(event) {
			if (event.path.endsWith(olyWorkspaceSettingsPath) || event.path.endsWith(olyWorkspaceStatePath))
			{
				refreshProjectStatusBarItemTooltip();
			}
		});

		context.subscriptions.push(vscode.commands.registerCommand('oly.changeActiveProject', async () => {
			let projectNames = await client.getProjectList();
			let result = await vscode.window.showQuickPick(projectNames);

			let workspaceSettings = await readOlyWorkspaceSettings();
			workspaceSettings.activeProject = result;
			await saveOlyWorkspaceSettings(workspaceSettings);
		}));

		context.subscriptions.push(vscode.commands.registerCommand('oly.changeActiveConfiguration', async () => {
			let configNames = await client.getActiveProjectConfigurationList();
			let result = await vscode.window.showQuickPick(configNames);

			let workspaceState = await readOlyWorkspaceState();
			if (workspaceState.activeConfiguration != result)
			{
				workspaceState.activeConfiguration = result;
				await saveOlyWorkspaceState(workspaceState);
			}
		}));

		context.subscriptions.push(vscode.commands.registerCommand('oly.cleanWorkspace', async () => {
			await client.cleanWorkspace();
		}));

		let active = getActiveDocument();
		if (active?.languageId === 'oly') {
			await syntaxTreeView.refresh(active, null);
		}
	});

	// Start the client. This will also launch the server
	client.start();
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
