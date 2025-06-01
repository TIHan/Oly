import * as path from 'path';
import { parseJsonText } from 'typescript';
import { workspace, ExtensionContext } from 'vscode';
import * as vscode from 'vscode';
import {
	CancellationToken,
	CancellationTokenSource,
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	StateChangeEvent,
	Trace
} from 'vscode-languageclient/node';
import { WorkDoneProgressBegin, WorkDoneProgressEnd, WorkDoneProgressReport, WorkDoneProgress, ProgressToken, InitializeParams } from 'vscode-languageserver-protocol';

let client: LanguageClient;
let isClientReady: boolean = false;

const tokenTypes = new Map<string, number>();
const tokenModifiers = new Map<string, number>();

const legend = (function () {
	const tokenTypesLegend = [
		'comment', 'string', 'keyword', 'number', 'regexp', 'operator', 'namespace',
		'type', 'struct', 'class', 'interface', 'enum', 'enumMember', 'typeParameter', 'function',
		'member', 'macro', 'variable', 'parameter', 'property', 'label', 'field', 'conditionalDirectiveBody'
	];
	tokenTypesLegend.forEach((tokenType, index) => tokenTypes.set(tokenType, index));

	const tokenModifiersLegend = [
		'declaration', 'documentation', 'readonly', 'static', 'abstract', 'deprecated',
		'modification', 'async'
	];
	tokenModifiersLegend.forEach((tokenModifier, index) => tokenModifiers.set(tokenModifier, index));

	return new vscode.SemanticTokensLegend(tokenTypesLegend, tokenModifiersLegend);
})();

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

interface IOlyToken {
	line: number;
	startCharacter: number;
	length: number;
	tokenType: string;
	tokenModifiers: string[];
}

class OlyTextPosition {
	constructor(line: number, column: number) {
		this.line = line;
		this.column = column;
	}

	line: number;
	column: number;

	static toVscodePosition(olyPos: OlyTextPosition) {
		return new vscode.Position(olyPos.line, olyPos.column);
	}

	static fromVscodePosition(pos: vscode.Position) {
		return new OlyTextPosition(pos.line, pos.character)
	}
}

class OlyTextRange {
	constructor(start: OlyTextPosition, end: OlyTextPosition) {
		this.start = start;
		this.end = end;
	}

	start: OlyTextPosition
	end: OlyTextPosition

	static toVscodeRange(olyRange: OlyTextRange) {
		return new vscode.Range(OlyTextPosition.toVscodePosition(olyRange.start), OlyTextPosition.toVscodePosition(olyRange.end));
	}

	static fromVscodeRange(range: vscode.Range) {
		return new OlyTextRange(OlyTextPosition.fromVscodePosition(range.start), OlyTextPosition.fromVscodePosition(range.end));
	}
}

interface IOlySyntaxNodeViewModel extends vscode.TreeItem {
	id: string
	parent: IOlySyntaxNodeViewModel;
	color: string
	range: OlyTextRange;
	label: string;
	description: string;
	tooltip: vscode.MarkdownString;
	children: IOlySyntaxNodeViewModel[]
	collapsibleState: vscode.TreeItemCollapsibleState;
	icon: string;
	iconPath: vscode.ThemeIcon;
	isToken: boolean;
}

interface IOlySyntaxTreeViewModel {
	nodes: IOlySyntaxNodeViewModel[]
}

function sleep(ms: number) {
	return new Promise((resolve) => {
	  setTimeout(resolve, ms);
	});
  }

function getActiveDocument() {
	return vscode.window.activeTextEditor?.document;
}

function getActiveDocumentAndCursorPosition() {
	let textEditor = vscode.window.activeTextEditor;
	let document = textEditor?.document;
	let cursorPosition = textEditor?.selection.start;
	return { document, cursorPosition };
}

function getActiveDocumentVisibleRanges() {
	return vscode.window.activeTextEditor?.visibleRanges;
}

function findSyntaxNodeTokenViewModelByPosition(pos: OlyTextPosition, viewModel: IOlySyntaxNodeViewModel, ct: CancellationToken): IOlySyntaxNodeViewModel {
	if (ct.isCancellationRequested)
	{
		return null;
	}

	let range = viewModel.range;
	let start = range.start;
	let end = range.end;
	if (viewModel.isToken)
	{
		if (pos.line >= start.line && pos.line <= end.line && pos.column >= start.column && pos.column <= end.column)
		{
			return viewModel;
		}

		if (pos.line > start.line && pos.line < end.line)
		{
			return viewModel;
		}
	}
	else if (viewModel.children.length > 0)
	{
		for(var i = 0; i < viewModel.children.length; i++)
		{
			if (ct.isCancellationRequested)
			{
				return null;
			}

			var child = viewModel.children[i];
			child.parent = viewModel;
			var result = findSyntaxNodeTokenViewModelByPosition(pos, child, ct);
			if (result != null)
			{
				return result;
			}
		}
	}

	return null;
}

export class OlySyntaxTreeDataProvider implements vscode.TreeDataProvider<IOlySyntaxNodeViewModel> {
	constructor(private workspaceRoot: string) {}

	private viewModel: IOlySyntaxTreeViewModel = { nodes: [] }

	private _onDidChangeTreeData: vscode.EventEmitter<IOlySyntaxNodeViewModel | undefined | null | void> = new vscode.EventEmitter<IOlySyntaxNodeViewModel | undefined | null | void>();
  	readonly onDidChangeTreeData: vscode.Event<IOlySyntaxNodeViewModel | undefined | null | void> = this._onDidChangeTreeData.event;

	getViewModel() {
		return this.viewModel;
	}

	getTreeItem(element: IOlySyntaxNodeViewModel): vscode.TreeItem | Thenable<vscode.TreeItem> {
		let document = getActiveDocument();
		element.tooltip = new vscode.MarkdownString("```oly\n" + document.getText(OlyTextRange.toVscodeRange(element.range)) + "\n```");
		if (element.icon != null)
		{
			element.iconPath = new vscode.ThemeIcon(element.icon, new vscode.ThemeColor(element.color));
		}
		element.command = { title: "Navigate to Syntax Node", command: OlyClientCommands.navigateToSyntaxNodeCommand };
		return element;
	}

	getChildren(element?: IOlySyntaxNodeViewModel): vscode.ProviderResult<IOlySyntaxNodeViewModel[]> {
		if (element == null)
		{
			// root
			return this.viewModel.nodes;
		}
		else
		{
			for(var i = 0; i < element.children.length; i++)
			{
				let child = element.children[i];
				child.parent = element;
			}
			return element.children;
		}
	}

	getParent?(element: IOlySyntaxNodeViewModel): vscode.ProviderResult<IOlySyntaxNodeViewModel> {
		return element.parent;
	}

	async refresh(document: vscode.TextDocument, token: vscode.CancellationToken, onSucceed: any, onUpdate: any) {

		if (isClientReady && document?.languageId === 'oly')
		{
			return client.sendRequest("oly/getSyntaxTree", { documentPath: document.uri.path }, token).then((viewModel: IOlySyntaxTreeViewModel) => {
				if (!token.isCancellationRequested)
				{
					onSucceed();
					var callback = _viewModel => {};
					let sub = this.onDidChangeTreeData(() => callback(viewModel));
					callback = 
						viewModel => 
						{ 
							sub.dispose(); 
							onUpdate(viewModel); 
						};
					this.viewModel = viewModel;
					this._onDidChangeTreeData.fire();
				}
			});
		}
	}

	clear() {
		this.viewModel = { nodes: [] };
		this._onDidChangeTreeData.fire();
	}

	isEmpty() {
		return this.viewModel.nodes.length === 0;
	}
}

class DocumentRangeSemanticTokensProvider implements vscode.DocumentRangeSemanticTokensProvider {
	async provideDocumentRangeSemanticTokens(document: vscode.TextDocument, range: vscode.Range, token: vscode.CancellationToken): Promise<vscode.SemanticTokens> {
		return client.sendRequest("oly/getSemanticClassification", { Range: OlyTextRange.fromVscodeRange(range), documentPath: document.uri.path, version: document.version }, token).then((tokens: IOlyToken []) => {
			const builder = new vscode.SemanticTokensBuilder();
			tokens.forEach((token) => {
				builder.push(token.line, token.startCharacter, token.length, this._encodeTokenType(token.tokenType), this._encodeTokenModifiers(token.tokenModifiers));
			});
			return builder.build();
		});
	}

	private _encodeTokenType(tokenType: string): number {
		if (tokenTypes.has(tokenType)) {
			return tokenTypes.get(tokenType)!;
		} else if (tokenType === 'notInLegend') {
			return tokenTypes.size + 2;
		}
		return 0;
	}

	private _encodeTokenModifiers(strTokenModifiers: string[]): number {
		let result = 0;
		for (let i = 0; i < strTokenModifiers.length; i++) {
			const tokenModifier = strTokenModifiers[i];
			if (tokenModifiers.has(tokenModifier)) {
				result = result | (1 << tokenModifiers.get(tokenModifier)!);
			} else if (tokenModifier === 'notInLegend') {
				result = result | (1 << tokenModifiers.size + 2);
			}
		}
		return result;
	}
}

module OlyClientCommands {
	export const navigateToSyntaxNodeCommand = `oly.navigateToSyntaxNode`;
	export const navigateToSyntaxNodeCommandHandler = () => {
		syntaxTreeView.selection
		let textEditor = vscode.window.activeTextEditor;
		let document = textEditor.document;

		if (document.languageId === 'oly')
		{
			let items = syntaxTreeView.selection;
			if (items?.length > 0)
			{
				let item = items[0];
				let range = OlyTextRange.toVscodeRange(item.range);
				vscode.window.activeTextEditor.revealRange(range);
				vscode.window.activeTextEditor.selection = new vscode.Selection(range.start, range.end);
			}
		}
	};

	export const syntaxTreeDataProvider = new OlySyntaxTreeDataProvider(vscode.workspace.workspaceFolders[0].name);
	export const getSyntaxTreeCommand = `oly.getSyntaxTree`;
	export let syntaxTreeView: vscode.TreeView<IOlySyntaxNodeViewModel> = null;
	export const getSyntaxTreeCommandHandler = async () => {
		if (syntaxTreeView != null)
		{
			let active = getActiveDocumentAndCursorPosition()
			if (active?.document?.languageId === 'oly')
			{
				let ct = CancellationToken.None
				let pos = OlyTextPosition.fromVscodePosition(active.cursorPosition);

				let find = viewModel => {
					if (viewModel.nodes.length > 0)
					{
						let node = findSyntaxNodeTokenViewModelByPosition(pos, viewModel.nodes[0], ct);
						if (node != null)
						{
							OlyClientCommands.syntaxTreeView.reveal(node, { focus: false, select: true });
						}
					}
				};

				if (syntaxTreeDataProvider.isEmpty())
				{
					await syntaxTreeDataProvider.refresh(active.document, ct, () => { }, find);
				}
				else
				{
					find(syntaxTreeDataProvider.getViewModel());
				}
			}	
		}	
	}

	interface OlyLspCompilationResult {
		resultPath: string,
		error: string
	}

	export const compile = async (olyProjectStatusBarItem: vscode.StatusBarItem, ch: vscode.OutputChannel) => {
		let document = getActiveDocument()
		if (document != null && document.languageId == 'oly')
		{
			await document.save();
		}

		ch.appendLine("Compiling");
		let timeStart = new Date().getTime();
		let result: OlyLspCompilationResult = await client.sendRequest("oly/compile", { documentPath: "" });
		let assemblyPath = result.resultPath;
		if (assemblyPath != null)
		{
			let timeEnd = new Date().getTime();
			let time = timeEnd - timeStart
			ch.appendLine("Compiled successfully - " + time + "ms: " + assemblyPath);
			olyProjectStatusBarItem.color = undefined;
			olyProjectStatusBarItem.backgroundColor = undefined;
			return result;
		}
		else
		{
			let timeEnd = new Date().getTime();
			let time = timeEnd - timeStart
			ch.appendLine("Compilation failed - " + time + "ms");
			olyProjectStatusBarItem.color = new vscode.ThemeColor("statusBarItem.errorForeground");
			olyProjectStatusBarItem.backgroundColor = new vscode.ThemeColor("statusBarItem.errorBackground");
			return result;
		}
	};

	export const compileOutputChannel = vscode.window.createOutputChannel("Oly Compile");
}

class OlyLanguageClient extends LanguageClient {
	protected fillInitializeParams(params: InitializeParams): void {
		super.fillInitializeParams(params);
		params.workDoneToken = "oly/analysis";
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
	OlyClientCommands.syntaxTreeView = vscode.window.createTreeView('oly.syntaxTree', {
		treeDataProvider: OlyClientCommands.syntaxTreeDataProvider
	});

	let cts = new CancellationTokenSource();
	let isRefreshing = false;
	let refreshSyntaxTree = async (document: vscode.TextDocument, pos) => {
		if (isRefreshing)
		{	
			isRefreshing = false;
			cts.cancel();
			cts.dispose();
			cts = new CancellationTokenSource();
		}

		if (cts.token.isCancellationRequested)
		{
			cts = new CancellationTokenSource();
		}

		if (!OlyClientCommands.syntaxTreeView.visible)
		{
			return;	
		}

		let token = cts.token;
		let reveal = viewModel => {
			if (viewModel.nodes.length > 0 && !token.isCancellationRequested)
			{
				if (pos)
				{
					let node = findSyntaxNodeTokenViewModelByPosition(pos, viewModel.nodes[0], token);
					if (node != null)
					{
						OlyClientCommands.syntaxTreeView.reveal(node, { focus: false, select: true });
					}	
				}
			}
		};

		isRefreshing = true;
		await sleep(300);
		if (!token.isCancellationRequested)
		{
			OlyClientCommands.syntaxTreeView.description = "Loading...";
			await OlyClientCommands.syntaxTreeDataProvider.refresh(document, token, () => { OlyClientCommands.syntaxTreeView.message = null; }, (viewModel) => { 
				OlyClientCommands.syntaxTreeView.description = null;
				reveal(viewModel);
			});
		}
	};

	OlyClientCommands.syntaxTreeView.onDidChangeVisibility(async e => {
		if (e?.visible)
		{
			let doc = getActiveDocument();
			if (doc != null)
			{
				await refreshSyntaxTree(doc, null);
			}
			else
			{
				OlyClientCommands.syntaxTreeDataProvider.clear();
			}
		}
		else
		{
			OlyClientCommands.syntaxTreeDataProvider.clear();
		}
	});

	let olyWorkspaceStatusDefaultText = "Oly Workspace";
	let olyWorkspaceStatusSyncText = "$(sync~spin) Oly Workspace"
	let olyWorkspaceStatusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 100);
	olyWorkspaceStatusBarItem.text = olyWorkspaceStatusDefaultText;
	olyWorkspaceStatusBarItem.color = new vscode.ThemeColor("testing.iconPassed");
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
					olyWorkspaceStatusBarItem.tooltip = "";
					olyWorkspaceStatusBarItem.color = new vscode.ThemeColor("testing.iconPassed");
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
		await client.sendRequest("workspace/didChangeConfiguration", { settings: config });
		vscode.workspace.onDidChangeConfiguration(async (e) => {
			let config = vscode.workspace.getConfiguration("olyLanguageServer");
			await client.sendRequest("workspace/didChangeConfiguration", { settings: config });			
		});

		vscode.window.onDidChangeActiveTextEditor(async (e) => {
			if (e?.document?.languageId === 'oly') {
				await refreshSyntaxTree(e.document, null);
			}
		});

		vscode.workspace.onDidChangeTextDocument(async (e) => {
			if (e?.document?.languageId === 'oly' && (getActiveDocument()) == e.document) {
				let cursorPosition = e.contentChanges[0].range.start;
				let pos = OlyTextPosition.fromVscodePosition(cursorPosition);
				await refreshSyntaxTree(e.document, pos);
			}
		});

		context.subscriptions.push(vscode.languages.registerDocumentRangeSemanticTokensProvider({ language: 'oly'}, new DocumentRangeSemanticTokensProvider(), legend));
		context.subscriptions.push(vscode.commands.registerCommand(OlyClientCommands.getSyntaxTreeCommand, OlyClientCommands.getSyntaxTreeCommandHandler));
		context.subscriptions.push(vscode.commands.registerCommand(OlyClientCommands.navigateToSyntaxNodeCommand, OlyClientCommands.navigateToSyntaxNodeCommandHandler));

		async function compileCommandHandler() {
			let ch = OlyClientCommands.compileOutputChannel;
			
			ch.show(true);
			let result = await OlyClientCommands.compile(olyProjectStatusBarItem, ch);
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

		// Oly Workspace Vscode Settings
		let olyWorkspaceSettingsPath = '.olyworkspace/settings.json';
		let olyWorkspaceSettingsUri = vscode.Uri.file(path.join(vscode.workspace.workspaceFolders[0].uri.path, olyWorkspaceSettingsPath));
		async function readOlyWorkspaceVscode() {
			// TODO: How do we handle a blank active project?
			try
			{
				let fileContents: Uint8Array = await vscode.workspace.fs.readFile(olyWorkspaceSettingsUri);
				let stringContents = new TextDecoder().decode(fileContents);
				return JSON.parse(stringContents);
			}
			catch
			{
				let workspaceSettings: any = {};
				workspaceSettings.activeProject = "";
				var projectNames: string[] = await client.sendRequest("oly/getProjectList", { documentPath: "" });
				if (projectNames.length > 0)
				{
					workspaceSettings.activeProject = projectNames[0];
				}

				await saveOlyWorkspaceVscode(workspaceSettings);
				return await readOlyWorkspaceVscode();
			}
		}
		async function saveOlyWorkspaceVscode(workspaceVscode) {
			await vscode.workspace.fs.writeFile(olyWorkspaceSettingsUri, new TextEncoder().encode(JSON.stringify(workspaceVscode)));
		}

		async function refreshProjectStatusBarItemTooltip() {
			var workspaceVscode = await readOlyWorkspaceVscode();

			var projName = "(none selected)";
			if (workspaceVscode.activeProject !== null && workspaceVscode.activeProject !== undefined)
			{
				projName = workspaceVscode.activeProject;
			}			

			olyProjectStatusBarItem.tooltip = new vscode.MarkdownString("", true);
			olyProjectStatusBarItem.tooltip.isTrusted = true;
			olyProjectStatusBarItem.tooltip.appendMarkdown("Active Project: " + projName + "\n\n");
			olyProjectStatusBarItem.tooltip.appendMarkdown(`[$(project) Compile](command:oly.compile "Compile active project")\n\n`);
			olyProjectStatusBarItem.tooltip.appendMarkdown(`[$(debug-alt) Debug](command:workbench.action.debug.start "Debug active project")\n\n`);
			olyProjectStatusBarItem.tooltip.appendMarkdown(`[$(debug-start) Run](command:workbench.action.debug.run "Run active project")\n\n`);
			olyProjectStatusBarItem.tooltip.appendMarkdown(`[$(settings-gear) Change Active Project](command:oly.changeActiveProject "Change active project")\n\n`);
			olyProjectStatusBarItem.text = olyProjectStatusDefaultText + ": " + projName;
		}
		refreshProjectStatusBarItemTooltip();

		let stateWatcher = workspace.createFileSystemWatcher('**/*.json');
		stateWatcher.onDidChange(function(event) {
			if (event.path.endsWith(olyWorkspaceSettingsPath))
			{
				refreshProjectStatusBarItemTooltip();
			}
		});
		stateWatcher.onDidCreate(function(event) {
			if (event.path.endsWith(olyWorkspaceSettingsPath))
			{
				refreshProjectStatusBarItemTooltip();
			}
		});
		// End - Oly Workspace Vscode Settings

		context.subscriptions.push(vscode.commands.registerCommand('oly.changeActiveProject', async () => {
			let projectNames: string[] = await client.sendRequest("oly/getProjectList", { documentPath: "" });
			let result = await vscode.window.showQuickPick(projectNames);

			let workspaceVscode = await readOlyWorkspaceVscode();
			workspaceVscode.activeProject = result;
			await saveOlyWorkspaceVscode(workspaceVscode);
		}));

		let active = getActiveDocument();
		if (active?.languageId === 'oly') {
			await refreshSyntaxTree(active, null);
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
