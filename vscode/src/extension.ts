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
import { autoCreateLaunchJson, getActiveDocument, sleep } from './Helpers';
import { OlySolutionExplorerView } from './OlySolutionExplorerView';

export let client: OlyLanguageClient;
export let isClientReady: boolean = false;

async function build(client: OlyLanguageClient, olyProjectStatusBarItem: vscode.StatusBarItem, ch: vscode.OutputChannel) {
	let document = getActiveDocument();
	if (document != null && document.languageId == 'oly') {
		await document.save();
	}

	ch.appendLine("Building");
	let timeStart = new Date().getTime();
	let result = await client.buildActiveProject();
	let assemblyPath = result.resultPath;
	if (assemblyPath != null) {
		let timeEnd = new Date().getTime();
		let time = timeEnd - timeStart;
		ch.appendLine("Build successful - " + time + "ms: " + assemblyPath);
		olyProjectStatusBarItem.color = undefined;
		olyProjectStatusBarItem.backgroundColor = undefined;
		return result;
	}

	else {
		let timeEnd = new Date().getTime();
		let time = timeEnd - timeStart;
		ch.appendLine("Build failed - " + time + "ms");
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
		path.join('out', 'net10.0', 'Oly.LanguageServer.dll')
	);

	// If the extension is launched in debug mode then the debug server options are used
	// Otherwise the run options are used
	let serverOptions: ServerOptions = {
		run: { command: 'dotnet', args: [serverModule] },
		debug: { command: 'dotnet', args: [serverModule] }
	};

	let olyFileWatcher = workspace.createFileSystemWatcher('**/*.oly');
	let olyxFileWatcher = workspace.createFileSystemWatcher('**/*.olyx');
	let jsonFileWatcher = workspace.createFileSystemWatcher('**/**.json');

	// Options to control the language client
	let clientOptions: LanguageClientOptions = {
		// Register the server for plain text documents
		documentSelector: [{ scheme: 'file', language: 'oly' }],
		synchronize: {
			fileEvents: [olyFileWatcher, olyxFileWatcher, jsonFileWatcher]
		}
	};

	// Create the language client and start the client.
	client = new OlyLanguageClient(
		'oly.languageServer',
		'Oly Language Server',
		serverOptions,
		clientOptions
	);

	// Clear diagnostics on files that were deleted or renamed
	olyFileWatcher.onDidDelete(uri => client.diagnostics.delete(uri));
	olyxFileWatcher.onDidDelete(uri => client.diagnostics.delete(uri));
	vscode.workspace.onDidRenameFiles(e => {
		e.files.forEach(args => {
			let path = args.oldUri.path.toLowerCase();
			if (path.endsWith('.oly') || path.endsWith('.olyx')) {
				client.diagnostics.delete(args.oldUri);
			}
		})
	});

	// View initializations
	let syntaxTreeView = OlySyntaxTreeView.createFromVscodeWindow();
	let solutionExplorerView = OlySolutionExplorerView.createFromVscodeWindow(context);

	// Oly Workspace - status bar item
	let olyWorkspaceStatusDefaultText = "$(globe) Oly Workspace";
	let olyWorkspaceStatusSyncText = "$(loading~spin) Oly Workspace"
	let olyWorkspaceStatusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 100);
	olyWorkspaceStatusBarItem.text = olyWorkspaceStatusDefaultText;

	function showOlyWorkspaceStatusCleanTooltip() {
		olyWorkspaceStatusBarItem.tooltip = new vscode.MarkdownString("", true);
		olyWorkspaceStatusBarItem.tooltip.isTrusted = true;
		olyWorkspaceStatusBarItem.tooltip.appendMarkdown('[$(clear-all) Clean](command:oly.cleanWorkspace "Clean workspace")\n\n');
	}

	function beginOlyWorkspaceStatus(title: string) {
		olyWorkspaceStatusBarItem.text = olyWorkspaceStatusSyncText;
		olyWorkspaceStatusBarItem.color = new vscode.ThemeColor("statusBarItem.prominentForeground");
		olyWorkspaceStatusBarItem.backgroundColor = new vscode.ThemeColor("statusBarItem.prominentBackground");
		olyWorkspaceStatusBarItem.tooltip = title;
	}

	function endOlyWorkspaceStatus() {
		olyWorkspaceStatusBarItem.text = olyWorkspaceStatusDefaultText;
		olyWorkspaceStatusBarItem.color = new vscode.ThemeColor("statusBarItem.prominentForeground");
		olyWorkspaceStatusBarItem.backgroundColor = new vscode.ThemeColor("statusBarItem.prominentBackground");
		showOlyWorkspaceStatusCleanTooltip();
	}

	showOlyWorkspaceStatusCleanTooltip();

	endOlyWorkspaceStatus();
	olyWorkspaceStatusBarItem.show();

	// Building... - status bar item
	let olyBuildingStatusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 98);
	olyBuildingStatusBarItem.text = "$(loading~spin) Oly Building...";
	olyBuildingStatusBarItem.color = new vscode.ThemeColor("statusBarItem.prominentForeground");
	olyBuildingStatusBarItem.backgroundColor = new vscode.ThemeColor("statusBarItem.prominentBackground");

	// Oly Project: {active-project-name}({active-project-configuration-name}) - status bar item
	let olyProjectStatusDefaultText = "Oly Project";
	let olyProjectStatusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 99);

	function handleProgressNotification(params: WorkDoneProgressBegin | WorkDoneProgressReport | WorkDoneProgressEnd) {
		switch (params.kind) {
			case 'begin':
				if (params.message == "building") {
					olyBuildingStatusBarItem.show();
				}
				else {
					beginOlyWorkspaceStatus(params.title);
				}
				break;
			case 'end':
				if (params.message == "building") {
					olyBuildingStatusBarItem.hide();
				}
				else {
					endOlyWorkspaceStatus();
				}
				break;
		}
	}

	function registerStatusBarItems() {
		client.onProgress(WorkDoneProgress.type, 'oly/progress', handleProgressNotification);
	}

	client.onReady().then(async () => {
		await autoCreateLaunchJson();

		isClientReady = true;
		registerStatusBarItems();
		let config = vscode.workspace.getConfiguration("oly.languageServer");
		await client.didChangeWorkspaceConfiguration(config);
		vscode.workspace.onDidChangeConfiguration(async (_e) => {
			let config = vscode.workspace.getConfiguration("oly.languageServer");
			client.didChangeWorkspaceConfiguration(config);
		});

		vscode.workspace.onDidChangeTextDocument(async (e) => {
			if (e?.document?.languageId === 'oly' && (getActiveDocument()) == e.document) {
				let cursorPosition = e.contentChanges[0].range.start;
				let pos = OlyTextPosition.fromVscodePosition(cursorPosition);
				await syntaxTreeView.refresh(e.document, pos);
			}
		});

		context.subscriptions.push(vscode.languages.registerDocumentRangeSemanticTokensProvider({ language: 'oly' }, new DocumentRangeSemanticTokensProvider(), OlyLanguageClient.legend));
		OlySyntaxTreeView.register(context, syntaxTreeView);

		let buildOutputChannel = vscode.window.createOutputChannel("Oly Build");
		async function buildCommandHandler() {
			let ch = buildOutputChannel;

			ch.show(true);
			let result = await build(client, olyProjectStatusBarItem, ch);
			if (result == null) {
				throw new Error("Oly Build Failed\nSee output for details");
			}
			else if (result.resultPath == null) {
				ch.appendLine("========================================================\n");
				ch.append("ERROR:\n" + result.error);
				throw new Error("Oly Build Failed\nSee output for details");
			}

			return result.resultPath;
		}

		context.subscriptions.push(vscode.commands.registerCommand(OlyClientCommands.build, buildCommandHandler));

		// Oly Workspace Settings
		let olyWorkspaceSettingsPath = '.oly/workspace/settings.json';
		let olyWorkspaceSettingsUri = vscode.Uri.file(path.join(vscode.workspace.workspaceFolders[0].uri.path, olyWorkspaceSettingsPath));
		async function readOlyWorkspaceSettings() {
			// TODO: How do we handle a blank active project?
			try {
				let fileContents: Uint8Array = await vscode.workspace.fs.readFile(olyWorkspaceSettingsUri);
				let stringContents = new TextDecoder().decode(fileContents);
				return JSON.parse(stringContents);
			}
			catch {
				let olyWorkspaceSettings: any = {};
				olyWorkspaceSettings.activeProject = "";
				let projectNames = await client.getProjectList();
				if (projectNames.length > 0) {
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
		let olyWorkspaceStatePath = '.oly/workspace/state.json';
		let olyWorkspaceStateUri = vscode.Uri.file(path.join(vscode.workspace.workspaceFolders[0].uri.path, olyWorkspaceStatePath));
		async function readOlyWorkspaceState() {
			try {
				let fileContents: Uint8Array = await vscode.workspace.fs.readFile(olyWorkspaceStateUri);
				let stringContents = new TextDecoder().decode(fileContents);
				return JSON.parse(stringContents);
			}
			catch {
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
				await client.doesProjectExist(olyWorkspaceSettings.activeProject)) {
				projText = olyWorkspaceSettings.activeProject;
			}
			else {
				isValid = false;
			}

			var isDebuggable = false;

			var olyWorkspaceState = await readOlyWorkspaceState();
			let proj = await client.tryGetActiveProjectInfo();
			var configText = "";
			if (isValid &&
				olyWorkspaceState.activeConfiguration !== undefined &&
				olyWorkspaceState.activeConfiguration !== null &&
				await client.doesActiveProjectConfigurationExist(olyWorkspaceState.activeConfiguration)) {
				configText = ` (${olyWorkspaceState.activeConfiguration})`;
			}
			else if (isValid && proj !== null) {
				configText = proj.configurationName;
				if (configText === null) {
					isValid = false;
				}
				else {
					configText = ` (${configText})`;
				}
			}

			if (proj !== null)
				isDebuggable = proj.isDebuggable;

			if (isValid) {
				olyProjectStatusBarItem.color = new vscode.ThemeColor("statusBarItem.prominentForeground");
				olyProjectStatusBarItem.backgroundColor = new vscode.ThemeColor("statusBarItem.prominentBackground");
			}
			else {
				olyProjectStatusBarItem.color = new vscode.ThemeColor("statusBarItem.errorForeground");
				olyProjectStatusBarItem.backgroundColor = new vscode.ThemeColor("statusBarItem.errorBackground");
			}

			olyProjectStatusBarItem.tooltip = new vscode.MarkdownString("", true);
			olyProjectStatusBarItem.tooltip.isTrusted = true;

			if (isValid) {
				olyProjectStatusBarItem.tooltip.appendMarkdown(`Active Project: ${projText}${configText}\n\n`);
				olyProjectStatusBarItem.tooltip.appendMarkdown(`[$(project) Build](command:${OlyClientCommands.build} "Build active project")\n\n`);
				if (isDebuggable)
					olyProjectStatusBarItem.tooltip.appendMarkdown(`[$(debug-alt) Debug](command:${OlyClientCommands.debug} "Debug active project")\n\n`);
				olyProjectStatusBarItem.tooltip.appendMarkdown(`[$(debug-start) Run](command:${OlyClientCommands.run} "Run active project")\n\n`);
				olyProjectStatusBarItem.tooltip.appendMarkdown(`[$(settings-gear) Change Active Configuration](command:${OlyClientCommands.changeActiveConfiguration} "Change active configuration")\n\n`);
				olyProjectStatusBarItem.command = { title: "", command: "vscode.open", arguments: [proj.uri] };
			}
			else {
				olyProjectStatusBarItem.command = null;
			}
			olyProjectStatusBarItem.tooltip.appendMarkdown(`[$(settings-gear) Change Active Project](command:${OlyClientCommands.changeActiveProject} "Change active project")\n\n`);
			olyProjectStatusBarItem.text = `${olyProjectStatusDefaultText}: ${projText}${configText}`;
		}
		refreshProjectStatusBarItemTooltip();
		olyProjectStatusBarItem.show();

		let stateWatcher = workspace.createFileSystemWatcher('**/*.json');
		stateWatcher.onDidChange(function (event) {
			if (event.path.endsWith(olyWorkspaceSettingsPath) || event.path.endsWith(olyWorkspaceStatePath)) {
				refreshProjectStatusBarItemTooltip();
			}
		});
		stateWatcher.onDidCreate(function (event) {
			if (event.path.endsWith(olyWorkspaceSettingsPath) || event.path.endsWith(olyWorkspaceStatePath)) {
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
			let proj = await client.tryGetActiveProjectInfo();
			let configNames = proj.configurationList;
			let options: vscode.QuickPickOptions = { ignoreFocusOut: true, placeHolder: "Select a configuration..." }
			let result = await vscode.window.showQuickPick(configNames, options);

			let workspaceState = await readOlyWorkspaceState();
			if (workspaceState.activeConfiguration != result &&
				result !== undefined &&
				result !== null &&
				result != '') {
				workspaceState.activeConfiguration = result;
				await saveOlyWorkspaceState(workspaceState);
			}
		}));

		context.subscriptions.push(vscode.commands.registerCommand(OlyClientCommands.cleanWorkspace, async () => {
			await client.cleanWorkspace();
		}));

		context.subscriptions.push(vscode.commands.registerCommand(OlyClientCommands.createFile, async () => {
			var projectUri = solutionExplorerView.getSelectedProject();
			if (projectUri !== undefined) {
				async function validateInput(newFileName: string): Promise<string> {
					if (newFileName.indexOf('\\') != -1 || newFileName.indexOf('/') != -1) {
						return "Invalid file name";
					}
					if (!newFileName.toLocaleLowerCase().endsWith('.oly')) {
						return "'.oly' extension required"
					}
					let fileUri = vscode.Uri.file(path.join(path.dirname(projectUri.path), newFileName));
					try {
						let _ = await vscode.workspace.fs.stat(fileUri); // test if the file exists
						return "File already exists";
					}
					catch {
						return undefined;
					}
				}
				let newFileName = await vscode.window.showInputBox({ title: "File Name", validateInput: validateInput, value: ".oly", valueSelection: [0, 0] });
				if (newFileName !== undefined && newFileName !== null) {
					let projectTextDocument = await vscode.workspace.openTextDocument(projectUri);
					let projectText = new TextDecoder().decode(await vscode.workspace.fs.readFile(projectUri));

					let fileUri = vscode.Uri.file(path.join(path.dirname(projectUri.path), newFileName));

					let r = new RegExp(`#target.*(\\n|\\r\\n)`);
					let results = r.exec(projectTextDocument.getText());
					if (results !== null) {
						let result = results[0];
						let posIndex = projectText.indexOf(result) + result.length;
						let pos = projectTextDocument.positionAt(posIndex);

						let encoder = new TextEncoder();

						let namespaceText = path.basename(projectUri.path).replace(path.extname(projectUri.path), '');
						let namespaceText2 = path.basename(fileUri.path).replace(path.extname(fileUri.path), '');
						await vscode.workspace.fs.writeFile(fileUri, encoder.encode(`namespace ${namespaceText}.${namespaceText2}`));

						let edit = new vscode.WorkspaceEdit();
						edit.insert(projectUri, pos, `#load "${path.basename(fileUri.path)}"\n`);
						await vscode.workspace.applyEdit(edit);
						await vscode.workspace.saveAll();
						vscode.commands.executeCommand("vscode.open", fileUri);
					}
					else {
						await vscode.window.showErrorMessage(`Unable to create file: ${fileUri.path}`)
					}
				}
			}
		}));

		context.subscriptions.push(vscode.commands.registerCommand(OlyClientCommands.deleteFile, async () => {
			var fileUri = solutionExplorerView.getSelectedFile();
			var projectUri = solutionExplorerView.getSelectedProject();

			if (fileUri !== undefined && projectUri !== undefined) {
				let projectTextDocument = await vscode.workspace.openTextDocument(projectUri);
				let projectText = new TextDecoder().decode(await vscode.workspace.fs.readFile(projectUri));

				let filePath = path.relative(path.dirname(projectUri.path), fileUri.path);

				let r = new RegExp(`#load.*"${filePath}".*(\\n|\\r\\n)`);
				let results = r.exec(projectTextDocument.getText());
				if (results !== null) {
					let result = results[0];
					let startIndex = projectText.indexOf(result);
					let endIndex = startIndex + result.length;
					let startPos = projectTextDocument.positionAt(startIndex);
					let endPos = projectTextDocument.positionAt(endIndex);

					let range = new vscode.Range(startPos, endPos);

					let edit = new vscode.WorkspaceEdit();
					edit.delete(projectUri, range)
					await vscode.workspace.applyEdit(edit);
					await vscode.workspace.saveAll();
				}
				await vscode.workspace.fs.delete(fileUri);
			}
		}));

		async function refreshSolutionExplorer(doc: vscode.TextDocument) {
			if (doc?.languageId === 'oly') {
				await solutionExplorerView.refresh();
				await solutionExplorerView.goTo(doc.uri);
			} else {
				await solutionExplorerView.refresh();
			}
		}

		async function refreshSyntaxTree(doc: vscode.TextDocument) {
			syntaxTreeView.clear();
			if (doc?.languageId === 'oly') {
				await syntaxTreeView.refresh(doc, null);
			}
		}

		olyFileWatcher.onDidCreate(async _ => {
			await refreshSolutionExplorer(getActiveDocument());
		});

		olyxFileWatcher.onDidCreate(async _ => {
			await refreshSolutionExplorer(getActiveDocument());
		});

		olyFileWatcher.onDidDelete(async _ => {
			await refreshSolutionExplorer(getActiveDocument());
		});

		olyxFileWatcher.onDidDelete(async _ => {
			await refreshSolutionExplorer(getActiveDocument());
		});

		vscode.window.onDidChangeActiveTextEditor(async e => {
			if (e?.document?.languageId === 'oly') {
				await refreshSyntaxTree(e.document);
				await solutionExplorerView.goTo(e.document.uri)
			}
		});

		solutionExplorerView.onDidChangeVisibility(async e => {
			if (e.visible) {
				await refreshSolutionExplorer(getActiveDocument());
			}
		})

		syntaxTreeView.onDidChangeVisibility(async e => {
			if (e.visible) {
				await refreshSyntaxTree(getActiveDocument());
			}
		});

		vscode.workspace.onDidSaveTextDocument(async e => {
			if (e?.languageId === 'oly' && e.uri.path.toLowerCase().endsWith('.olyx')) {
				await refreshSolutionExplorer(getActiveDocument());
			}
		});
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
