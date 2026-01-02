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

function initializeViewModel(context: vscode.ExtensionContext, element: IOlySolutionTreeNodeViewModel) {
    if (element.resourcePath != null) {
        element.resourceUri = vscode.Uri.file(element.resourcePath);
        element.command = { title: "", command: "vscode.open", arguments: [element.resourceUri] };
    }
    if (element.icon != null) {
        if (element.icon == "symbol-file") {
            element.iconPath = {
                light: path.join(context.extensionPath, 'icons', 'oly-file-light.png'),
                dark: path.join(context.extensionPath, 'icons', 'oly-file-dark.png')
            };
        } else if (element.icon == "project") {
            element.iconPath = {
                light: path.join(context.extensionPath, 'icons', 'oly-project-light.png'),
                dark: path.join(context.extensionPath, 'icons', 'oly-project-dark.png')
            };
        } else {
            element.iconPath = new vscode.ThemeIcon(element.icon, new vscode.ThemeColor(element.color));
        }
    }
    for (var i = 0; i < element.children.length; i++) {
        let child = element.children[i];
        child.parent = element;
        initializeViewModel(context, child);
    }
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
    async refresh(token: vscode.CancellationToken, onSucceed: any, onUpdate: any) {
        if (isClientReady) {
            let context = this.context;
            return client.getSolutionExplorer(token).then(viewModel => {
                if (!token.isCancellationRequested) {
                    viewModel.children.forEach(x => initializeViewModel(context, x));
                    
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
        this.view.message = "Loading...";
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

        let token = this.cts.token;
        this.isRefreshing = true;
        await sleep(300);
        if (!token.isCancellationRequested) {
            this.view.message = "Loading...";
            await this.dataProvider.refresh(token, () => { this.view.message = null; }, (_viewModel) => { });
        }
    }

    public get onDidChangeVisibility() {
        return this.view.onDidChangeVisibility;
    }

    static areResourcesEqual(resourceUri1: vscode.Uri, resourceUri2: vscode.Uri): boolean {
        return resourceUri1 !== undefined && resourceUri1 !== null && 
                resourceUri2 !== undefined && resourceUri2 !== null &&
                resourceUri1.path == resourceUri2.path;
    }

    isResourceSelected(resourceUri: vscode.Uri) {
        return this.view.selection.findIndex(x => OlySolutionExplorerView.areResourcesEqual(x.resourceUri, resourceUri)) != -1;
    }

    async goToChild (resourceUri: vscode.Uri, children: IOlySolutionTreeNodeViewModel[]) {
        for (var i = 0; i < children.length; i++)
        {
            let x = children[i];
            if (OlySolutionExplorerView.areResourcesEqual(x.resourceUri, resourceUri) && !this.isResourceSelected(resourceUri)) {
                await this.view.reveal(x, { focus: true, select: true });
                return true;
            }
            if (await this.goToChild(resourceUri, x.children))
                return true;
        }
        return false;
    }

    public async goTo(resourceUri: vscode.Uri) {
        if (!this.view.visible)
            return;

        let vm = this.dataProvider.getViewModel();
        await this.goToChild(resourceUri, vm.children);
    }

    public getSelectedFile(): vscode.Uri {
         if (!this.view.visible || this.view.selection.length == 0)
            return undefined;

        var view = this.view.selection[0];

        if (view.icon != "project" && view.icon != "symbol-file")
            return undefined;

        if (view.resourceUri === undefined || view.resourceUri === null)
            return undefined;

        return view.resourceUri;       
    }

    public getSelectedProject(): vscode.Uri {
        if (!this.view.visible || this.view.selection.length == 0)
            return undefined;

        var view = this.view.selection[0];
        while (view.icon != "project" && view.parent !== undefined && view.parent !== null)
        {
            view = view.parent;
        }

        if (view.resourceUri === undefined || view.resourceUri === null)
            return undefined;

        return view.resourceUri;
    }
}