import * as vscode from 'vscode';
import { CancellationToken, CancellationTokenSource } from 'vscode-languageclient';
import { IOlySyntaxNodeViewModel, IOlySyntaxTreeViewModel, OlyTextPosition, OlyTextRange } from './IOlySyntaxTreeViewModel';
import { getActiveDocument, getActiveDocumentAndCursorPosition, sleep } from './Helpers';
import * as OlyClientCommands from './OlyClientCommands';
import { client, isClientReady } from './extension';

class OlySyntaxTreeDataProvider implements vscode.TreeDataProvider<IOlySyntaxNodeViewModel> {

	private viewModel: IOlySyntaxTreeViewModel = { nodes: [] };

	private _onDidChangeTreeData: vscode.EventEmitter<IOlySyntaxNodeViewModel | undefined | null | void> = new vscode.EventEmitter<IOlySyntaxNodeViewModel | undefined | null | void>();
	readonly onDidChangeTreeData: vscode.Event<IOlySyntaxNodeViewModel | undefined | null | void> = this._onDidChangeTreeData.event;

	getViewModel() {
		return this.viewModel;
	}

	getTreeItem(element: IOlySyntaxNodeViewModel): vscode.TreeItem | Thenable<vscode.TreeItem> {
		const document = getActiveDocument();
		if (document) {
			element.tooltip = new vscode.MarkdownString("```oly\n" + document.getText(OlyTextRange.toVscodeRange(element.range)) + "\n```");
			if (element.icon != null) {
				element.iconPath = new vscode.ThemeIcon(element.icon, new vscode.ThemeColor(element.color));
			}
			element.command = { title: "Navigate to Syntax Node", command: OlyClientCommands.navigateToSyntaxNode };
		}
		return element;
	}

	getChildren(element?: IOlySyntaxNodeViewModel): vscode.ProviderResult<IOlySyntaxNodeViewModel[]> {
		if (element == null) {
			// root
			return this.viewModel.nodes;
		}

		else {
			for (let i = 0; i < element.children.length; i++) {
				const child = element.children[i];
				child.parent = element;
			}
			return element.children;
		}
	}

	getParent?(element: IOlySyntaxNodeViewModel): vscode.ProviderResult<IOlySyntaxNodeViewModel> {
		return element.parent;
	}

	// TODO: This has a dependency on 'extension.ts', we should try to fix this.
	async refresh(document: vscode.TextDocument, token: vscode.CancellationToken, onSucceed: any, onUpdate: any) {
		if (isClientReady && document?.languageId === 'oly') {
			return client.getSyntaxTree(document, token).then(viewModel => {
				if (!token.isCancellationRequested) {
					onSucceed();
					let callback = (_viewModel: any) => { };
					const sub = this.onDidChangeTreeData(() => callback(viewModel));
					callback =
						viewModel => {
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

function findOlySyntaxNodeTokenViewModelByPosition(pos: OlyTextPosition, viewModel: IOlySyntaxNodeViewModel, ct: CancellationToken): IOlySyntaxNodeViewModel | undefined {
	if (ct.isCancellationRequested)
	{
		return undefined;
	}

	const range = viewModel.range;
	const start = range.start;
	const end = range.end;
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
		for(let i = 0; i < viewModel.children.length; i++)
		{
			if (ct.isCancellationRequested)
			{
				return undefined;
			}

			const child = viewModel.children[i];
			child.parent = viewModel;
			const result = findOlySyntaxNodeTokenViewModelByPosition(pos, child, ct);
			if (result != null)
			{
				return result;
			}
		}
	}

	return undefined;
}

export class OlySyntaxTreeView {
	private cts: CancellationTokenSource = new CancellationTokenSource();
	private isRefreshing: boolean = false;
	private view: vscode.TreeView<IOlySyntaxNodeViewModel>;
	private dataProvider: OlySyntaxTreeDataProvider;

	private constructor(view: vscode.TreeView<IOlySyntaxNodeViewModel>, dataProvider: OlySyntaxTreeDataProvider) {
		this.view = view;
		this.dataProvider = dataProvider;
		this.view.message = "Loading...";
	}

	public static createFromVscodeWindow(): OlySyntaxTreeView {
		const dataProvider = new OlySyntaxTreeDataProvider();
		const view = vscode.window.createTreeView('oly.syntaxTree', {
			treeDataProvider: dataProvider
		});
		return new OlySyntaxTreeView(view, dataProvider);
	}

	public async refresh(document: vscode.TextDocument, pos: any) {
		if (this.isRefreshing) {
			this.isRefreshing = false;
			this.cts.cancel();
			this.cts.dispose();
			this.cts = new CancellationTokenSource();
		}

		if (this.cts.token.isCancellationRequested) {
			this.cts = new CancellationTokenSource();
		}

		if (!this.view.visible) {
			return;
		}

		const token = this.cts.token;
		const reveal = (viewModel: any) => {
			if (viewModel.nodes.length > 0 && !token.isCancellationRequested) {
				if (pos) {
					const node = findOlySyntaxNodeTokenViewModelByPosition(pos, viewModel.nodes[0], token);
					if (node != null) {
						this.view.reveal(node, { focus: false, select: true });
					}
				}
			}
		};

		this.isRefreshing = true;
		await sleep(300);
		if (!token.isCancellationRequested) {
			this.view.message = "Loading...";
			await this.dataProvider.refresh(document, token, () => { this.view.message = undefined; }, (viewModel: any) => {
				reveal(viewModel);
			});
		}
	}

	public get onDidChangeVisibility() {
        return this.view.onDidChangeVisibility;
    }

	public clear() {
		this.dataProvider.clear();
	}

	public static register(context: vscode.ExtensionContext, syntaxTreeView: OlySyntaxTreeView) {
		const getSyntaxTreeCommandHandler = async () => {
			if (syntaxTreeView != null) {
				const active = getActiveDocumentAndCursorPosition();
				if (active?.document?.languageId === 'oly' && active.cursorPosition) {
					const ct = CancellationToken.None;
					const pos = OlyTextPosition.fromVscodePosition(active.cursorPosition);

					const find = (viewModel: any) => {
						if (viewModel.nodes.length > 0) {
							const node = findOlySyntaxNodeTokenViewModelByPosition(pos, viewModel.nodes[0], ct);
							if (node != null) {
								syntaxTreeView.view.reveal(node, { focus: false, select: true });
							}
						}
					};

					if (syntaxTreeView.dataProvider.isEmpty()) {
						await syntaxTreeView.dataProvider.refresh(active.document, ct, () => { }, find);
					}

					else {
						find(syntaxTreeView.dataProvider.getViewModel());
					}
				}
			}
		};

		const navigateToSyntaxNodeCommandHandler = () => {
			const textEditor = vscode.window.activeTextEditor;
			if (!textEditor) {
				return;
			}

			const document = textEditor.document;
	
			if (document.languageId === 'oly') {
				const items = syntaxTreeView.view.selection;
				if (items?.length > 0) {
					const item = items[0];
					const range = OlyTextRange.toVscodeRange(item.range);
					textEditor.revealRange(range);
					textEditor.selection = new vscode.Selection(range.start, range.end);
				}
			}
		};

		context.subscriptions.push(vscode.commands.registerCommand(OlyClientCommands.getSyntaxTree, getSyntaxTreeCommandHandler));
		context.subscriptions.push(vscode.commands.registerCommand(OlyClientCommands.navigateToSyntaxNode, navigateToSyntaxNodeCommandHandler));
	}
}