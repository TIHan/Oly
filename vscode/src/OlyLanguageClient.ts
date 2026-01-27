import { CancellationToken, InitializeParams } from 'vscode-languageclient';
import { LanguageClient } from 'vscode-languageclient/node';
import * as vscode from 'vscode';
import { IOlySyntaxTreeViewModel } from './IOlySyntaxTreeViewModel';
import { IOlySolutionExplorerViewModel as IOlySolutionExplorerViewModel } from './OlySolutionExplorerView';

export interface OlyBuildResult {
	resultPath: string,
	error: string
}

export interface OlyProjectInfo {
	uri: vscode.Uri,
	configurationName: string,
	configurationList: string[],
	isDebuggable: boolean
}

export class OlyLanguageClient extends LanguageClient {
	protected fillInitializeParams(params: InitializeParams): void {
		super.fillInitializeParams(params);
		params.workDoneToken = "oly/analysis";
	}

	public async getProjectList(): Promise<string[]> {
		return await this.sendRequest("oly/getProjectList");
	}

	public async doesProjectExist(projectName: string): Promise<boolean> {
		return await this.sendRequest("oly/doesProjectExist", { ProjectName: projectName });
	}

	public async tryGetActiveProjectInfo(): Promise<OlyProjectInfo> {
		return await this.sendRequest("oly/tryGetActiveProjectInfo");
	}

	public async doesActiveProjectConfigurationExist(configName: string): Promise<boolean> {
		let proj = await this.tryGetActiveProjectInfo();
		return proj.configurationList.indexOf(configName) != -1;
	}

	public async buildActiveProject(): Promise<OlyBuildResult> {
		return await this.sendRequest("oly/buildActiveProject");
	}

	public async getSyntaxTree(document: vscode.TextDocument, ct: CancellationToken): Promise<IOlySyntaxTreeViewModel> {
		return await this.sendRequest("oly/getSyntaxTree", { documentPath: document.uri.path }, ct);
	}

	public async getSolutionExplorer(ct: CancellationToken): Promise<IOlySolutionExplorerViewModel> {
		return await this.sendRequest("oly/getSolutionExplorer", ct);
	}

	public async getSolutionExplorerProject(projectUri: vscode.Uri, ct: CancellationToken): Promise<IOlySolutionExplorerViewModel> {
		return await this.sendRequest("oly/getSolutionExplorerProject", { projectPath: projectUri.path }, ct);
	}

	public async cleanWorkspace(): Promise<void> {
		await this.sendRequest("oly/cleanWorkspace");
	}

	public async didChangeWorkspaceConfiguration(config: vscode.WorkspaceConfiguration): Promise<void> {
		await this.sendRequest("workspace/didChangeConfiguration", { settings: config });
	}
}
