import * as vscode from 'vscode';
import { CancellationToken } from 'vscode-languageclient';
import { getActiveDocumentAndCursorPosition, getActiveDocument } from './extension';
import { OlyTextRange, IOlySyntaxNodeViewModel, OlyTextPosition } from './IOlySyntaxTreeViewModel';
import { findOlySyntaxNodeTokenViewModelByPosition, OlySyntaxTreeDataProvider } from './OlySyntaxTreeDataProvider';
import { OlyLanguageClient } from './OlyLanguageClient';


export namespace OlyClientCommands {
	export const navigateToSyntaxNodeCommand = `oly.navigateToSyntaxNode`;
	export const getSyntaxTreeCommand = `oly.getSyntaxTree`;

	export const compile = async (client: OlyLanguageClient, olyProjectStatusBarItem: vscode.StatusBarItem, ch: vscode.OutputChannel) => {
		let document = getActiveDocument();
		if (document != null && document.languageId == 'oly') {
			await document.save();
		}

		ch.appendLine("Compiling");
		let timeStart = new Date().getTime();
		let result = await client.compileActiveProject();
		let assemblyPath = result.resultPath;
		if (assemblyPath != null) {
			let timeEnd = new Date().getTime();
			let time = timeEnd - timeStart;
			ch.appendLine("Compiled successfully - " + time + "ms: " + assemblyPath);
			olyProjectStatusBarItem.color = undefined;
			olyProjectStatusBarItem.backgroundColor = undefined;
			return result;
		}

		else {
			let timeEnd = new Date().getTime();
			let time = timeEnd - timeStart;
			ch.appendLine("Compilation failed - " + time + "ms");
			olyProjectStatusBarItem.color = new vscode.ThemeColor("statusBarItem.errorForeground");
			olyProjectStatusBarItem.backgroundColor = new vscode.ThemeColor("statusBarItem.errorBackground");
			return result;
		}
	};

	export const compileOutputChannel = vscode.window.createOutputChannel("Oly Compile");
}
