import * as path from 'path';
import * as vscode from 'vscode';
import { ExtensionContext, workspace } from 'vscode';
import {
	LanguageClientOptions,
	ServerOptions
} from 'vscode-languageclient/node';
import { WorkDoneProgress, WorkDoneProgressBegin, WorkDoneProgressEnd, WorkDoneProgressReport } from 'vscode-languageserver-protocol';
import { OlyTextPosition } from './IOlySyntaxTreeViewModel';
import { OlyLanguageClient } from './OlyLanguageClient';
import * as OlyClientCommands from './OlyClientCommands';
import { OlySyntaxTreeView } from './OlySyntaxTreeView';
import { autoCreateLaunchJson, getActiveDocument } from './Helpers';
import { OlySolutionExplorerView } from './OlySolutionExplorerView';
import * as os from 'os';

interface IOlyWorkspaceSettings {
	activeProject: string | undefined
}

interface IOlyWorkspaceState {
	activeConfiguration: string
}

export let client: OlyLanguageClient;
export let isClientReady: boolean = false;

async function build(client: OlyLanguageClient, olyProjectStatusBarItem: vscode.StatusBarItem, ch: vscode.OutputChannel) {
	const document = getActiveDocument();
	if (document != null && document.languageId == 'oly') {
		await document.save();
	}

	ch.appendLine("Building");
	const timeStart = new Date().getTime();
	const result = await client.buildActiveProject();
	const assemblyPath = result.resultPath;
	if (assemblyPath != null) {
		const timeEnd = new Date().getTime();
		const time = timeEnd - timeStart;
		ch.appendLine("Build successful - " + time + "ms: " + assemblyPath);
		olyProjectStatusBarItem.color = undefined;
		olyProjectStatusBarItem.backgroundColor = undefined;
		return result;
	} else if (result.error != null) {
		const timeEnd = new Date().getTime();
		const time = timeEnd - timeStart;
		ch.appendLine("Build failed - " + time + "ms");
		olyProjectStatusBarItem.color = new vscode.ThemeColor("statusBarItem.errorForeground");
		olyProjectStatusBarItem.backgroundColor = new vscode.ThemeColor("statusBarItem.errorBackground");
		return result;
	} else {
		return result;
	}
}

export async function activate(context: ExtensionContext) {
	const workspaceFolders = vscode.workspace.workspaceFolders ?? [];
	if (workspaceFolders.length == 0) {
		vscode.window.showErrorMessage("Oly: Unable to load extension. No workspaces were found.");
		return;
	}
	if (workspaceFolders.length > 1) {
		vscode.window.showErrorMessage("Oly: Unable to load extension. Multiple workspaces not supported.");
		return;
	}

	const workspaceFolder = workspaceFolders[0];

	const olyExe = path.join(os.homedir(), ".oly/", "bin", "oly");

	const lspConfig = vscode.workspace.getConfiguration("oly.languageServer.process");

	let gcServer = "0";
	if (lspConfig.get("gcServer")) {
		gcServer = "1";
	}
	let env = process.env;
	env["DOTNET_gcServer"] = gcServer;

	// If the extension is launched in debug mode then the debug server options are used
	// Otherwise the run options are used
	const serverOptions: ServerOptions = {
		run: { command: olyExe, args: ["lsp"], options: { env: env } },
		debug: { command: olyExe, args: ["lsp"], options: { env: env } }
	};

	// TODO: Handle this in the language server instead.
	const olyFileWatcher = workspace.createFileSystemWatcher('**/*');

	// Options to control the language client
	const clientOptions: LanguageClientOptions = {
		// Register the server for plain text documents
		documentSelector: [{ scheme: 'file', language: 'oly' }],
		synchronize: {
			fileEvents: [olyFileWatcher]
		},
	};

	// Create the language client and start the client.
	client = new OlyLanguageClient(
		'oly.languageServer',
		'Oly Language Server',
		serverOptions,
		clientOptions
	);

	// Clear diagnostics on files that were deleted or renamed
	olyFileWatcher.onDidDelete(uri => {
		const path = uri.path.toLowerCase();
		if (path.endsWith('.oly') || path.endsWith('.olyx')) {
			client.diagnostics?.delete(uri);
		}
	});
	vscode.workspace.onDidRenameFiles(e => {
		e.files.forEach(args => {
			const path = args.oldUri.path.toLowerCase();
			if (path.endsWith('.oly') || path.endsWith('.olyx')) {
				client.diagnostics?.delete(args.oldUri);
			}
		})
	});

	// View initializations
	const syntaxTreeView = OlySyntaxTreeView.createFromVscodeWindow();
	const solutionExplorerView = OlySolutionExplorerView.createFromVscodeWindow(context);

	// Building... - status bar item
	const olyBuildingStatusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 98);
	olyBuildingStatusBarItem.text = "$(loading~spin) Oly Building...";
	olyBuildingStatusBarItem.color = new vscode.ThemeColor("statusBarItem.prominentForeground");
	olyBuildingStatusBarItem.backgroundColor = new vscode.ThemeColor("statusBarItem.prominentBackground");

	// Analyzing... - status bar item
	const olyProgressStatusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 99);
	olyProgressStatusBarItem.text = "$(loading~spin) Oly Analyzing...";
	olyProgressStatusBarItem.color = new vscode.ThemeColor("statusBarItem.prominentForeground");
	olyProgressStatusBarItem.backgroundColor = new vscode.ThemeColor("statusBarItem.prominentBackground");

	// Oly Project: {active-project-name}({active-project-configuration-name}) - status bar item
	const olyProjectStatusDefaultText = "Oly Project";
	const olyProjectStatusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 100);
	function beginLoadingProject() {
		olyProjectStatusBarItem.text = `${olyProjectStatusDefaultText}: $(loading~spin)`;
	}
	beginLoadingProject();
	olyProjectStatusBarItem.show();

	function handleProgressNotification(params: WorkDoneProgressBegin | WorkDoneProgressReport | WorkDoneProgressEnd) {
		switch (params.kind) {
			case 'begin':
				if (params.message == "Building") {
					olyBuildingStatusBarItem.show();
				}
				else if (params.message == "Analyzing") {
					olyProgressStatusBarItem.show()
				}
				break;
			case 'end':
				if (params.message == "Building") {
					olyBuildingStatusBarItem.hide();
				}
				else if (params.message == "Analyzing") {
					olyProgressStatusBarItem.hide();
				}
				break;
		}
	}

	function registerStatusBarItems() {
		client.onProgress(WorkDoneProgress.type, 'oly/progress', handleProgressNotification);
	}

	// Start the client. This will also launch the server
	client.start();

	await autoCreateLaunchJson();

	isClientReady = true;
	registerStatusBarItems();
	const config = vscode.workspace.getConfiguration("oly.languageServer");
	await client.didChangeWorkspaceConfiguration(config);
	vscode.workspace.onDidChangeConfiguration(async (e) => {
		if (e.affectsConfiguration("oly.languageServer.process")) {
			const result = await vscode.window.showWarningMessage("Oly language server process configuration changed and requires to reload the window. Reload?", "Yes", "No");
			if (result == "Yes") {
				vscode.commands.executeCommand("workbench.action.reloadWindow");
			}
		} else if (e.affectsConfiguration("oly.languageServer")) {
			const config = vscode.workspace.getConfiguration("oly.languageServer");
			client.didChangeWorkspaceConfiguration(config);
		}
	});

	vscode.workspace.onDidChangeTextDocument(async (e) => {
		if (e?.document?.languageId === 'oly' && (getActiveDocument()) == e.document) {
			const cursorPosition = e.contentChanges[0].range.start;
			const pos = OlyTextPosition.fromVscodePosition(cursorPosition);
			await syntaxTreeView.refresh(e.document, pos);
		}
	});

	OlySyntaxTreeView.register(context, syntaxTreeView);

	const buildOutputChannel = vscode.window.createOutputChannel("Oly Build");
	async function buildCommandHandler() {
		const ch = buildOutputChannel;

		ch.show(true);
		const result = await build(client, olyProjectStatusBarItem, ch);

		if (result.resultPath == null) {
			ch.appendLine("========================================================\n");
			if (result.error == null) {
				ch.append("Build cancelled.");
				throw new Error("Oly Build Cancelled");
			} else {
				ch.append("ERROR:\n" + result.error);
				throw new Error("Oly Build Failed\nSee output for details");
			}
		}

		return result.resultPath;
	}

	context.subscriptions.push(vscode.commands.registerCommand(OlyClientCommands.build, buildCommandHandler));

	// Oly Workspace Settings
	const olyWorkspaceSettingsPath = '.oly/workspace/settings.json';
	const olyWorkspaceSettingsUri = vscode.Uri.file(path.join(workspaceFolder.uri.path, olyWorkspaceSettingsPath));
	async function readOlyWorkspaceSettings(): Promise<IOlyWorkspaceSettings> {
		// TODO: How do we handle a blank active project?
		try {
			const fileContents: Uint8Array = await vscode.workspace.fs.readFile(olyWorkspaceSettingsUri);
			const stringContents = new TextDecoder().decode(fileContents);
			return JSON.parse(stringContents);
		}
		catch {
			const olyWorkspaceSettings: IOlyWorkspaceSettings = { activeProject: '' };
			const projectNames = await client.getProjectList();
			if (projectNames.length > 0) {
				olyWorkspaceSettings.activeProject = projectNames[0];
			}

			await saveOlyWorkspaceSettings(olyWorkspaceSettings);
			return await readOlyWorkspaceSettings();
		}
	}
	async function saveOlyWorkspaceSettings(olyWorkspaceSettings: IOlyWorkspaceSettings) {
		await vscode.workspace.fs.writeFile(olyWorkspaceSettingsUri, new TextEncoder().encode(JSON.stringify(olyWorkspaceSettings)));
	}

	// Oly Workspace State
	const olyWorkspaceStatePath = '.oly_target/workspace/state.json';
	const olyWorkspaceStateUri = vscode.Uri.file(path.join(workspaceFolder.uri.path, olyWorkspaceStatePath));
	async function readOlyWorkspaceState(): Promise<IOlyWorkspaceState> {
		try {
			const fileContents: Uint8Array = await vscode.workspace.fs.readFile(olyWorkspaceStateUri);
			const stringContents = new TextDecoder().decode(fileContents);
			return JSON.parse(stringContents);
		}
		catch {
			// default - TODO: Technically a project may not have 'Debug' as default.
			const olyWorkspaceState: IOlyWorkspaceState = { activeConfiguration: "Debug" };
			saveOlyWorkspaceState(olyWorkspaceState);
			return readOlyWorkspaceState();
		}
	}
	async function saveOlyWorkspaceState(olyWorkspaceState: IOlyWorkspaceState) {
		await vscode.workspace.fs.writeFile(olyWorkspaceStateUri, new TextEncoder().encode(JSON.stringify(olyWorkspaceState)));
	}

	async function refreshProjectStatusBarItemTooltip() {
		beginLoadingProject();
		const olyWorkspaceSettings = await readOlyWorkspaceSettings();

		let projText = "(NO PROJECT SELECTED)";
		let isValid = true;
		if (olyWorkspaceSettings.activeProject !== undefined &&
			olyWorkspaceSettings.activeProject !== null &&
			await client.doesProjectExist(olyWorkspaceSettings.activeProject)) {
			projText = olyWorkspaceSettings.activeProject;
		}
		else {
			isValid = false;
		}

		let isDebuggable = false;

		const olyWorkspaceState = await readOlyWorkspaceState();
		const proj = await client.tryGetActiveProjectInfo();
		let configText = "";
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
			olyProjectStatusBarItem.command = undefined;
		}
		olyProjectStatusBarItem.tooltip.appendMarkdown(`[$(settings-gear) Change Active Project](command:${OlyClientCommands.changeActiveProject} "Change active project")\n\n`);
		olyProjectStatusBarItem.tooltip.appendMarkdown('[$(clear-all) Clean Workspace](command:oly.cleanWorkspace "Clean workspace")\n\n');
		olyProjectStatusBarItem.text = `${olyProjectStatusDefaultText}: ${projText}${configText}`;
	}
	refreshProjectStatusBarItemTooltip();
	refreshSolutionExplorer(undefined);

	const stateWatcher = workspace.createFileSystemWatcher('**/*.json');
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
		const projectNames = await client.getProjectList();
		const options: vscode.QuickPickOptions = { ignoreFocusOut: true, placeHolder: "Select a project..." }
		const result = await vscode.window.showQuickPick(projectNames, options);

		const workspaceSettings = await readOlyWorkspaceSettings();
		workspaceSettings.activeProject = result;
		await saveOlyWorkspaceSettings(workspaceSettings);
	}));

	context.subscriptions.push(vscode.commands.registerCommand(OlyClientCommands.changeActiveConfiguration, async () => {
		const proj = await client.tryGetActiveProjectInfo();
		const configNames = proj.configurationList;
		const options: vscode.QuickPickOptions = { ignoreFocusOut: true, placeHolder: "Select a configuration..." }
		const result = await vscode.window.showQuickPick(configNames, options);

		const workspaceState = await readOlyWorkspaceState();
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
		const projectUri = solutionExplorerView.getSelectedProject();
		if (projectUri !== undefined) {
			async function validateInput(newFileName: string): Promise<string> {
				if (newFileName.indexOf('\\') != -1 || newFileName.indexOf('/') != -1) {
					return "Invalid file name";
				}
				if (!newFileName.toLocaleLowerCase().endsWith('.oly')) {
					return "'.oly' extension required"
				}
				if (!projectUri) { 
					return "Project not found"
				}
				const fileUri = vscode.Uri.file(path.join(path.dirname(projectUri.path), newFileName));
				try {
					const _ = await vscode.workspace.fs.stat(fileUri); // test if the file exists
					return "File already exists";
				}
				catch (ex) {
					if (ex instanceof Error) {
						return ex.message;
					} else {
						return String(ex);
					}
				}
			}
			const newFileName = await vscode.window.showInputBox({ title: "File Name", validateInput: validateInput, value: ".oly", valueSelection: [0, 0] });
			if (newFileName !== undefined && newFileName !== null) {
				const projectTextDocument = await vscode.workspace.openTextDocument(projectUri);
				const projectText = new TextDecoder().decode(await vscode.workspace.fs.readFile(projectUri));

				const fileUri = vscode.Uri.file(path.join(path.dirname(projectUri.path), newFileName));

				const r = new RegExp(`#target.*(\\n|\\r\\n)`);
				const results = r.exec(projectTextDocument.getText());
				if (results !== null) {
					const result = results[0];
					const posIndex = projectText.indexOf(result) + result.length;
					const pos = projectTextDocument.positionAt(posIndex);

					const encoder = new TextEncoder();

					const namespaceText = path.basename(projectUri.path).replace(path.extname(projectUri.path), '');
					const namespaceText2 = path.basename(fileUri.path).replace(path.extname(fileUri.path), '');
					await vscode.workspace.fs.writeFile(fileUri, encoder.encode(`namespace ${namespaceText}.${namespaceText2}`));

					const edit = new vscode.WorkspaceEdit();
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
		const fileUri = solutionExplorerView.getSelectedFile();
		const projectUri = solutionExplorerView.getSelectedProject();

		if (fileUri !== undefined && projectUri !== undefined) {
			const projectTextDocument = await vscode.workspace.openTextDocument(projectUri);
			const projectText = new TextDecoder().decode(await vscode.workspace.fs.readFile(projectUri));

			const filePath = path.relative(path.dirname(projectUri.path), fileUri.path);

			const r = new RegExp(`#load.*"${filePath}".*(\\n|\\r\\n)`);
			const results = r.exec(projectTextDocument.getText());
			if (results !== null) {
				const result = results[0];
				const startIndex = projectText.indexOf(result);
				const endIndex = startIndex + result.length;
				const startPos = projectTextDocument.positionAt(startIndex);
				const endPos = projectTextDocument.positionAt(endIndex);

				const range = new vscode.Range(startPos, endPos);

				const edit = new vscode.WorkspaceEdit();
				edit.delete(projectUri, range)
				await vscode.workspace.applyEdit(edit);
				await vscode.workspace.saveAll();
			}
			await vscode.workspace.fs.delete(fileUri);
		}
	}));

	async function refreshSolutionExplorer(doc: vscode.TextDocument | undefined) {
		if (doc?.languageId === 'oly') {
			await solutionExplorerView.refresh();
			await solutionExplorerView.goTo(doc.uri);
		} else {
			await solutionExplorerView.refresh();
		}
	}

	async function refreshSyntaxTree(doc: vscode.TextDocument | undefined) {
		syntaxTreeView.clear();
		if (doc?.languageId === 'oly') {
			await syntaxTreeView.refresh(doc, null);
		}
	}

	olyFileWatcher.onDidCreate(async uri => {
		const path = uri.path.toLowerCase();
		if (path.endsWith('.oly') || path.endsWith('.olyx')) {
			await refreshSolutionExplorer(getActiveDocument());
		}
	});

	olyFileWatcher.onDidDelete(async uri => {
		const path = uri.path.toLowerCase();
		if (path.endsWith('.oly') || path.endsWith('.olyx')) {
			await refreshSolutionExplorer(getActiveDocument());
		}
	});

	vscode.window.onDidChangeActiveTextEditor(async e => {
		if (e?.document?.languageId === 'oly') {
			await refreshSyntaxTree(e.document);
			await solutionExplorerView.goTo(e.document.uri)
		}
	});

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
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
