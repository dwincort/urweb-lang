import * as vscode from 'vscode';
import { UrWebDocumentSymbolProvider } from './symbolProvider';
import { UrWebDefinitionProvider } from './definitionProvider';

export async function activate(context: vscode.ExtensionContext): Promise<void> {
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

    // Register the definition provider for .ur and .urs files
    const definitionProvider = new UrWebDefinitionProvider();
    await definitionProvider.initialize();

    context.subscriptions.push(
        vscode.languages.registerDefinitionProvider(
            { language: 'ur' },
            definitionProvider
        )
    );

    context.subscriptions.push(
        vscode.languages.registerDefinitionProvider(
            { language: 'urs' },
            definitionProvider
        )
    );
}

export function deactivate(): void {
    // Nothing to clean up
}
