import * as vscode from 'vscode';
import { CancellationToken, CancellationTokenSource } from 'vscode-languageclient';
import { IOlySyntaxNodeViewModel, OlyTextPosition, OlyTextRange } from './IOlySyntaxTreeViewModel';
import { OlySyntaxTreeDataProvider, findOlySyntaxNodeTokenViewModelByPosition } from './OlySyntaxTreeDataProvider';
import { getActiveDocument, getActiveDocumentAndCursorPosition, sleep } from './Helpers';
import { OlyClientCommands } from './OlyClientCommands';


export class OlySyntaxTreeView {
	private cts: CancellationTokenSource = new CancellationTokenSource();
	private isRefreshing: boolean = false;
	private view: vscode.TreeView<IOlySyntaxNodeViewModel>;
	private dataProvider: OlySyntaxTreeDataProvider;

	private constructor(view: vscode.TreeView<IOlySyntaxNodeViewModel>, dataProvider: OlySyntaxTreeDataProvider) {
		this.view = view;
		this.dataProvider = dataProvider;
	}

	public static createFromVscodeWindow(): OlySyntaxTreeView {
		let dataProvider = new OlySyntaxTreeDataProvider();
		let view = vscode.window.createTreeView('oly.syntaxTree', {
			treeDataProvider: dataProvider
		});
		return new OlySyntaxTreeView(view, dataProvider);
	}

	public async refresh(document: vscode.TextDocument, pos) {
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

		let token = this.cts.token;
		let reveal = viewModel => {
			if (viewModel.nodes.length > 0 && !token.isCancellationRequested) {
				if (pos) {
					let node = findOlySyntaxNodeTokenViewModelByPosition(pos, viewModel.nodes[0], token);
					if (node != null) {
						this.view.reveal(node, { focus: false, select: true });
					}
				}
			}
		};

		this.isRefreshing = true;
		await sleep(300);
		if (!token.isCancellationRequested) {
			this.view.description = "Loading...";
			await this.dataProvider.refresh(document, token, () => { this.view.message = null; }, (viewModel) => {
				this.view.description = null;
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
		let getSyntaxTreeCommandHandler = async () => {
			if (syntaxTreeView != null) {
				let active = getActiveDocumentAndCursorPosition();
				if (active?.document?.languageId === 'oly') {
					let ct = CancellationToken.None;
					let pos = OlyTextPosition.fromVscodePosition(active.cursorPosition);

					let find = viewModel => {
						if (viewModel.nodes.length > 0) {
							let node = findOlySyntaxNodeTokenViewModelByPosition(pos, viewModel.nodes[0], ct);
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

		let navigateToSyntaxNodeCommandHandler = () => {
			let textEditor = vscode.window.activeTextEditor;
			let document = textEditor.document;
	
			if (document.languageId === 'oly') {
				let items = syntaxTreeView.view.selection;
				if (items?.length > 0) {
					let item = items[0];
					let range = OlyTextRange.toVscodeRange(item.range);
					vscode.window.activeTextEditor.revealRange(range);
					vscode.window.activeTextEditor.selection = new vscode.Selection(range.start, range.end);
				}
			}
		};

		context.subscriptions.push(vscode.commands.registerCommand(OlyClientCommands.getSyntaxTreeCommand, getSyntaxTreeCommandHandler));
		context.subscriptions.push(vscode.commands.registerCommand(OlyClientCommands.navigateToSyntaxNodeCommand, navigateToSyntaxNodeCommandHandler));
	}
}