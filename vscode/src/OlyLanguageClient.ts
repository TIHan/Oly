import { CancellationToken, InitializeParams } from 'vscode-languageclient';
import { LanguageClient } from 'vscode-languageclient/node';
import * as vscode from 'vscode';
import { IOlyToken, OlyTextRange, IOlySyntaxTreeViewModel } from './IOlySyntaxTreeViewModel';
import { IOlySolutionExplorerViewModel as IOlySolutionExplorerViewModel } from './OlySolutionExplorerView';

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

export interface OlyCompilationResult {
	resultPath: string,
	error: string
}

export class OlyLanguageClient extends LanguageClient {
	static get legend() { return legend; }

	protected fillInitializeParams(params: InitializeParams): void {
		super.fillInitializeParams(params);
		params.workDoneToken = "oly/analysis";
	}

	public async getProjectList(): Promise<string[]> {
		return await this.sendRequest("oly/getProjectList");
	}

	public async getActiveProjectConfigurationList(): Promise<string[]> {
		return await this.sendRequest("oly/getActiveProjectConfigurationList");
	}

	public async doesProjectExist(projectName: string): Promise<boolean> {
		return await this.sendRequest("oly/doesProjectExist", { ProjectName: projectName });
	}

	public async tryGetActiveProjectConfiguration(): Promise<string> {
		return await this.sendRequest("oly/tryGetActiveProjectConfiguration");
	}

	public async doesActiveProjectConfigurationExist(configName: string): Promise<boolean> {
		return await this.sendRequest("oly/doesActiveProjectConfigurationExist", { ConfigurationName: configName });
	}

	public async compileActiveProject(): Promise<OlyCompilationResult> {
		return await this.sendRequest("oly/compileActiveProject");
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

	public async getSemanticClassification(document: vscode.TextDocument, range: vscode.Range, ct: CancellationToken): Promise<vscode.SemanticTokens> {
		return await 
			this.sendRequest("oly/getSemanticClassification", { Range: OlyTextRange.fromVscodeRange(range), documentPath: document.uri.path, version: document.version }, ct)
			.then((tokens: IOlyToken[]) => {
				const builder = new vscode.SemanticTokensBuilder();
				tokens.forEach((token) => {
					builder.push(token.line, token.startCharacter, token.length, this._encodeTokenType(token.tokenType), this._encodeTokenModifiers(token.tokenModifiers));
				});
				return builder.build()
			});
	}

	public async cleanWorkspace(): Promise<void> {
		await this.sendRequest("oly/cleanWorkspace");
	}

	public async didChangeWorkspaceConfiguration(config: vscode.WorkspaceConfiguration): Promise<void> {
		await this.sendRequest("workspace/didChangeConfiguration", { settings: config });
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
