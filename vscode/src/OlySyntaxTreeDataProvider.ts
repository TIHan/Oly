import * as vscode from 'vscode';
import { getActiveDocument, isClientReady, client } from './extension';
import { IOlySyntaxNodeViewModel, IOlySyntaxTreeViewModel, OlyTextPosition, OlyTextRange } from './IOlySyntaxTreeViewModel';
import { CancellationToken } from 'vscode-languageclient';
import { OlyClientCommands } from './OlyClientCommands';

export class OlySyntaxTreeDataProvider implements vscode.TreeDataProvider<IOlySyntaxNodeViewModel> {

	private viewModel: IOlySyntaxTreeViewModel = { nodes: [] };

	private _onDidChangeTreeData: vscode.EventEmitter<IOlySyntaxNodeViewModel | undefined | null | void> = new vscode.EventEmitter<IOlySyntaxNodeViewModel | undefined | null | void>();
	readonly onDidChangeTreeData: vscode.Event<IOlySyntaxNodeViewModel | undefined | null | void> = this._onDidChangeTreeData.event;

	getViewModel() {
		return this.viewModel;
	}

	getTreeItem(element: IOlySyntaxNodeViewModel): vscode.TreeItem | Thenable<vscode.TreeItem> {
		let document = getActiveDocument();
		element.tooltip = new vscode.MarkdownString("```oly\n" + document.getText(OlyTextRange.toVscodeRange(element.range)) + "\n```");
		if (element.icon != null) {
			element.iconPath = new vscode.ThemeIcon(element.icon, new vscode.ThemeColor(element.color));
		}
		element.command = { title: "Navigate to Syntax Node", command: OlyClientCommands.navigateToSyntaxNodeCommand };
		return element;
	}

	getChildren(element?: IOlySyntaxNodeViewModel): vscode.ProviderResult<IOlySyntaxNodeViewModel[]> {
		if (element == null) {
			// root
			return this.viewModel.nodes;
		}

		else {
			for (var i = 0; i < element.children.length; i++) {
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

		if (isClientReady && document?.languageId === 'oly') {
			return client.getSyntaxTree(document, token).then(viewModel => {
				if (!token.isCancellationRequested) {
					onSucceed();
					var callback = _viewModel => { };
					let sub = this.onDidChangeTreeData(() => callback(viewModel));
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

export function findOlySyntaxNodeTokenViewModelByPosition(pos: OlyTextPosition, viewModel: IOlySyntaxNodeViewModel, ct: CancellationToken): IOlySyntaxNodeViewModel {
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
			var result = findOlySyntaxNodeTokenViewModelByPosition(pos, child, ct);
			if (result != null)
			{
				return result;
			}
		}
	}

	return null;
}

