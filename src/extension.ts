import * as vscode from 'vscode';
import { UrWebDocumentSymbolProvider } from './symbolProvider';

export function activate(context: vscode.ExtensionContext): void {
    // Register the document symbol provider for .ur and .urs files
    const symbolProvider = new UrWebDocumentSymbolProvider();

    context.subscriptions.push(
        vscode.languages.registerDocumentSymbolProvider(
            { language: 'ur' },
            symbolProvider
        )
    );

    context.subscriptions.push(
        vscode.languages.registerDocumentSymbolProvider(
            { language: 'urs' },
            symbolProvider
        )
    );
}

export function deactivate(): void {
    // Nothing to clean up
}
