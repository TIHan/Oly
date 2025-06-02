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
import { autoCreateLaunchJson, getActiveDocument } from './Helpers';

export let client: OlyLanguageClient;
export let isClientReady: boolean = false;

async function compile(client: OlyLanguageClient, olyProjectStatusBarItem: vscode.StatusBarItem, ch: vscode.OutputChannel) {
	let document = getActiveDocument();
	if (document != null && document.languageId == 'oly') {
		await document.save();
	}

	ch.appendLine("Compiling");
	let timeStart = new Date().getTime();
	let result = await client.compileActiveProject();
	let assemblyPath = result.resultPath;
	if (assemblyPath != null) {
		let timeEnd = new Date().getTime();
		let time = timeEnd - timeStart;
		ch.appendLine("Compiled successfully - " + time + "ms: " + assemblyPath);
		olyProjectStatusBarItem.color = undefined;
		olyProjectStatusBarItem.backgroundColor = undefined;
		return result;
	}

	else {
		let timeEnd = new Date().getTime();
		let time = timeEnd - timeStart;
		ch.appendLine("Compilation failed - " + time + "ms");
		olyProjectStatusBarItem.color = new vscode.ThemeColor("statusBarItem.errorForeground");
		olyProjectStatusBarItem.backgroundColor = new vscode.ThemeColor("statusBarItem.errorBackground");
		return result;
	}
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

	// Oly Workspace - status bar item
	let olyWorkspaceStatusDefaultText = "$(globe) Oly Workspace";
	let olyWorkspaceStatusSyncText = "$(sync~spin) Oly Workspace"
	let olyWorkspaceStatusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 100);
	olyWorkspaceStatusBarItem.text = olyWorkspaceStatusDefaultText;

	function beginOlyWorkspaceStatus() {
		olyWorkspaceStatusBarItem.text = olyWorkspaceStatusSyncText;
		olyWorkspaceStatusBarItem.tooltip = "Analyzing";
		olyWorkspaceStatusBarItem.color = new vscode.ThemeColor("statusBarItem.warningForeground");
		olyWorkspaceStatusBarItem.backgroundColor = new vscode.ThemeColor("statusBarItem.warningBackground");
	}

	function endOlyWorkspaceStatus() {
		olyWorkspaceStatusBarItem.text = olyWorkspaceStatusDefaultText;
		olyWorkspaceStatusBarItem.color = new vscode.ThemeColor("statusBarItem.prominentForeground");
		olyWorkspaceStatusBarItem.backgroundColor = new vscode.ThemeColor("statusBarItem.prominentBackground");
		
		olyWorkspaceStatusBarItem.tooltip = new vscode.MarkdownString("", true);
		olyWorkspaceStatusBarItem.tooltip.isTrusted = true;
		olyWorkspaceStatusBarItem.tooltip.appendMarkdown('[$(clear-all) Clean](command:oly.cleanWorkspace "Clean workspace")\n\n');
	}

	endOlyWorkspaceStatus();
	olyWorkspaceStatusBarItem.show();

	// Building... - status bar item
	let olyBuildingStatusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 99);
	olyBuildingStatusBarItem.text = "$(loading~spin) Building...";
	olyBuildingStatusBarItem.color = new vscode.ThemeColor("statusBarItem.warningForeground");
	olyBuildingStatusBarItem.backgroundColor = new vscode.ThemeColor("statusBarItem.warningBackground");

	// Oly Project: {active-project-name}({active-project-configuration-name}) - status bar item
	let olyProjectStatusDefaultText = "Oly Project";
	let olyProjectStatusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 98);

	function handleProgressNotification(params: WorkDoneProgressBegin | WorkDoneProgressReport | WorkDoneProgressEnd) {
		switch (params.kind) {
			case 'begin':
				if (params.message == "building")
				{
					olyBuildingStatusBarItem.show();
				}
				else
				{
					beginOlyWorkspaceStatus();
				}
				break;
			case 'end':
				if (params.message == "building")
				{
					olyBuildingStatusBarItem.hide();
				}
				else
				{
					endOlyWorkspaceStatus();
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
			let result = await compile(client, olyProjectStatusBarItem, ch);
			if (result == null)
			{
				throw new Error("Oly Compilation Failed\nSee output for details");
			}
			else if (result.resultPath == null)
			{
				ch.appendLine("========================================================\n");
				ch.append("ERROR:\n" + result.error);
				throw new Error("Oly Compilation Failed\nSee output for details");
			}

			return result.resultPath;
		}

		context.subscriptions.push(vscode.commands.registerCommand(OlyClientCommands.compileCommand, compileCommandHandler));

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
				olyWorkspaceState.activeConfiguration = "Debug"; // default - TODO: Technically a project may not have 'Debug' as default.
				saveOlyWorkspaceState(olyWorkspaceState);
				return readOlyWorkspaceState();
			}
		}
		async function saveOlyWorkspaceState(olyWorkspaceState) {
			await vscode.workspace.fs.writeFile(olyWorkspaceStateUri, new TextEncoder().encode(JSON.stringify(olyWorkspaceState)));
		}

		async function refreshProjectStatusBarItemTooltip() {
			var olyWorkspaceSettings = await readOlyWorkspaceSettings();

			var projText = "(NO PROJECT SELECTED)";
			var isValid = true;
			if (olyWorkspaceSettings.activeProject !== undefined && 
				olyWorkspaceSettings.activeProject !== null && 
				await client.doesProjectExist(olyWorkspaceSettings.activeProject))
			{
				projText = olyWorkspaceSettings.activeProject;
			}
			else
			{
				isValid = false;
			}
			
			var olyWorkspaceState = await readOlyWorkspaceState();
			var configText = "";
			if (isValid &&
				olyWorkspaceState.activeConfiguration !== undefined && 
				olyWorkspaceState.activeConfiguration !== null &&
				await client.doesActiveProjectConfigurationExist(olyWorkspaceState.activeConfiguration))
			{
				configText = ` (${olyWorkspaceState.activeConfiguration})`;
			}
			else if (isValid)
			{
				configText = await client.tryGetActiveProjectConfiguration();
				if (configText === null)
				{
					isValid = false;
				}
				else
				{
					configText = ` (${configText})`;
				}
			}

			if (isValid)
			{
				olyProjectStatusBarItem.color = new vscode.ThemeColor("statusBarItem.prominentForeground");
				olyProjectStatusBarItem.backgroundColor = new vscode.ThemeColor("statusBarItem.prominentBackground");
			}
			else
			{
				olyProjectStatusBarItem.color = new vscode.ThemeColor("statusBarItem.errorForeground");
				olyProjectStatusBarItem.backgroundColor = new vscode.ThemeColor("statusBarItem.errorBackground");
			}

			olyProjectStatusBarItem.tooltip = new vscode.MarkdownString("", true);
			olyProjectStatusBarItem.tooltip.isTrusted = true;

			if (isValid)
			{
				olyProjectStatusBarItem.tooltip.appendMarkdown(`Active Project: ${projText}${configText}\n\n`);
				olyProjectStatusBarItem.tooltip.appendMarkdown(`[$(project) Compile](command:${OlyClientCommands.compileCommand} "Compile active project")\n\n`);
				olyProjectStatusBarItem.tooltip.appendMarkdown(`[$(debug-alt) Debug](command:${OlyClientCommands.debug} "Debug active project")\n\n`);
				olyProjectStatusBarItem.tooltip.appendMarkdown(`[$(debug-start) Run](command:${OlyClientCommands.run} "Run active project")\n\n`);
				olyProjectStatusBarItem.tooltip.appendMarkdown(`[$(settings-gear) Change Active Configuration](command:${OlyClientCommands.changeActiveConfiguration} "Change active configuration")\n\n`);
			}
			olyProjectStatusBarItem.tooltip.appendMarkdown(`[$(settings-gear) Change Active Project](command:${OlyClientCommands.changeActiveProject} "Change active project")\n\n`);
			olyProjectStatusBarItem.text = `${olyProjectStatusDefaultText}: ${projText}${configText}`;
		}
		refreshProjectStatusBarItemTooltip();
		olyProjectStatusBarItem.show();

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

		context.subscriptions.push(vscode.commands.registerCommand(OlyClientCommands.changeActiveProject, async () => {
			let projectNames = await client.getProjectList();
			let options: vscode.QuickPickOptions = { ignoreFocusOut: true, placeHolder: "Select a project..." }
			let result = await vscode.window.showQuickPick(projectNames, options);

			let workspaceSettings = await readOlyWorkspaceSettings();
			workspaceSettings.activeProject = result;
			await saveOlyWorkspaceSettings(workspaceSettings);
		}));

		context.subscriptions.push(vscode.commands.registerCommand(OlyClientCommands.changeActiveConfiguration, async () => {
			let configNames = await client.getActiveProjectConfigurationList();
			let options: vscode.QuickPickOptions = { ignoreFocusOut: true, placeHolder: "Select a configuration..." }
			let result = await vscode.window.showQuickPick(configNames, options);

			let workspaceState = await readOlyWorkspaceState();
			if (workspaceState.activeConfiguration != result && 
				result !== undefined && 
				result !== null && 
				result != '')
			{
				workspaceState.activeConfiguration = result;
				await saveOlyWorkspaceState(workspaceState);

				// HACK: This is a hack to refresh open editors to reflect the new configuration.
				vscode.window.visibleTextEditors.forEach(async x => {
					if (x.document.uri.path.toLowerCase().endsWith('.olyx')) {
						let edit = new vscode.WorkspaceEdit();
						var text = x.document.getText();
						var endPos = x.document.positionAt(text.length);
						edit.delete(x.document.uri, new vscode.Range(new vscode.Position(0, 0), endPos));
						await vscode.workspace.applyEdit(edit);

						let edit2 = new vscode.WorkspaceEdit();
						edit2.insert(x.document.uri, new vscode.Position(0, 0), text);
						await vscode.workspace.applyEdit(edit2);

						await x.document.save();
					}
				});

			}
		}));

		context.subscriptions.push(vscode.commands.registerCommand(OlyClientCommands.cleanWorkspace, async () => {
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
