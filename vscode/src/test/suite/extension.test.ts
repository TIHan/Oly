import * as assert from 'assert';
import * as vscode from 'vscode';

suite('Oly Extension Test Suite', () => {
	vscode.window.showInformationMessage('Start all tests.');

    test('Check if extension is registered', () => {
        const extension = vscode.extensions.getExtension('oly-dev.oly');
        
        assert.ok(extension);
    });
});