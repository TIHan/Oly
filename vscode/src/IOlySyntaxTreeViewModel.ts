import * as vscode from 'vscode';

export class OlyTextPosition {
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

export class OlyTextRange {
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

export interface IOlySyntaxNodeViewModel extends vscode.TreeItem {
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

export interface IOlySyntaxTreeViewModel {
	nodes: IOlySyntaxNodeViewModel[];
}
