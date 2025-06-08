import * as vscode from 'vscode';
import { CancellationTokenSource } from 'vscode-languageclient';
import { sleep } from './Helpers';
import { client, isClientReady } from './extension';
import path = require('path');

interface IOlySolutionTreeNodeViewModel extends vscode.TreeItem {
    id: string;
    parent: IOlySolutionTreeNodeViewModel;
    children: IOlySolutionTreeNodeViewModel[];
    icon: string
    color: string
    resourcePath: string
}

export interface IOlySolutionExplorerViewModel {
	children: IOlySolutionTreeNodeViewModel[];
}

class OlySolutionExplorerDataProvider implements vscode.TreeDataProvider<IOlySolutionTreeNodeViewModel> {
    private viewModel: IOlySolutionExplorerViewModel = { children: [] };
    private context: vscode.ExtensionContext;

    private _onDidChangeTreeData: vscode.EventEmitter<IOlySolutionTreeNodeViewModel | undefined | null | void> = new vscode.EventEmitter<IOlySolutionTreeNodeViewModel | undefined | null | void>();
    readonly onDidChangeTreeData: vscode.Event<IOlySolutionTreeNodeViewModel | undefined | null | void> = this._onDidChangeTreeData.event;

    constructor(context: vscode.ExtensionContext) {
        this.context = context;
    }
        
    getTreeItem(element: IOlySolutionTreeNodeViewModel): vscode.TreeItem | Thenable<vscode.TreeItem> {
        if (element.resourcePath != null) {
            element.resourceUri = vscode.Uri.file(element.resourcePath);
            element.command = { title: "", command: "vscode.open", arguments: [element.resourceUri] };
        }
        return element;
    }

    getChildren(element?: IOlySolutionTreeNodeViewModel): vscode.ProviderResult<IOlySolutionTreeNodeViewModel[]> {
		if (element == null) {
			// root
			return this.viewModel.children;
		}
		return element.children;
    }

    getParent?(element: IOlySolutionTreeNodeViewModel): vscode.ProviderResult<IOlySolutionTreeNodeViewModel> {
        return element.parent;
    }

    // resolveTreeItem?(_item: vscode.TreeItem, element: IOlySolutionTreeNodeViewModel, token: vscode.CancellationToken): vscode.ProviderResult<vscode.TreeItem> {
    //     return element;
    // }

    // TODO: This has a dependency on 'extension.ts', we should try to fix this.
    async refresh( token: vscode.CancellationToken, onSucceed: any, onUpdate: any) {
        if (isClientReady) {
            let context = this.context;
            return client.getSolutionExplorer(token).then(viewModel => {
                if (!token.isCancellationRequested) {
                    function setupViewModel(element: IOlySolutionTreeNodeViewModel) {
                        if (element.icon != null) {
                            if (element.icon == "symbol-file") {
                                element.iconPath = {
                                    light: path.join(context.extensionPath, 'icons', 'oly-light.png'),
                                    dark: path.join(context.extensionPath, 'icons', 'oly-dark.png')
                                };
                            } else {
                                element.iconPath = new vscode.ThemeIcon(element.icon, new vscode.ThemeColor(element.color));
                            }
                        }
                        for (var i = 0; i < element.children.length; i++) {
                            let child = element.children[i];
                            child.parent = element;
                            setupViewModel(child);
                        }
                    }
                    viewModel.children.forEach(setupViewModel);
                    
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
		this.viewModel = { children: [] };
		this._onDidChangeTreeData.fire();
	}

    getViewModel() {
		return this.viewModel;
	}
}

export class OlySolutionExplorerView {
    private cts: CancellationTokenSource = new CancellationTokenSource();
    private isRefreshing: boolean = false;
    private view: vscode.TreeView<IOlySolutionTreeNodeViewModel>;
    private dataProvider: OlySolutionExplorerDataProvider;

    private constructor(view: vscode.TreeView<IOlySolutionTreeNodeViewModel>, dataProvider: OlySolutionExplorerDataProvider) {
        this.view = view;
        this.dataProvider = dataProvider;
    }

    public static createFromVscodeWindow(context: vscode.ExtensionContext): OlySolutionExplorerView {
        let dataProvider = new OlySolutionExplorerDataProvider(context);
        let view = vscode.window.createTreeView('oly.solutionExplorer', {
            treeDataProvider: dataProvider
        });
        return new OlySolutionExplorerView(view, dataProvider);
    }

    public async refresh() {
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
        this.isRefreshing = true;
        await sleep(300);
        if (!token.isCancellationRequested) {
            this.view.description = "Loading...";
            await this.dataProvider.refresh(token, () => { this.view.message = null; }, (viewModel) => {
                this.view.description = null;
            });
        }
    }

    public async goTo(resourceUri: vscode.Uri) {
        let vm = this.dataProvider.getViewModel();
        let view = this.view;
        async function goToChild (children: IOlySolutionTreeNodeViewModel[]) {
            for (var i = 0; i < children.length; i++)
            {
                let x = children[i];
                if (x.resourcePath != null && 
                    vscode.Uri.file(x.resourcePath).path == resourceUri.path && 
                    view.selection.findIndex(y => vscode.Uri.file(y.resourcePath).path == resourceUri.path) == -1) {
                    await view.reveal(x, { focus: true, select: true });
                    return true;
                }
                if (await goToChild(x.children))
                    return true;
            }
            return false;
        }
        await goToChild(vm.children);
    }

    public static register(context: vscode.ExtensionContext, view: OlySolutionExplorerView) {
        view.view.onDidChangeVisibility(async e => {
            if (e?.visible && view.dataProvider.getViewModel().children.length == 0)
            {
                await view.refresh();
            }
        });
    }
}