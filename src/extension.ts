import * as vscode from 'vscode';
import { UrWebDocumentSymbolProvider } from './symbolProvider';
import { UrWebDefinitionProvider } from './definitionProvider';
import { startLspClient, stopLspClient } from './lspClient';

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

    // Start LSP client (optional - graceful degradation if urweb not installed)
    try {
        const client = await startLspClient(context);
        if (client) {
            console.log('Ur/Web LSP client started successfully');
        }
    } catch (e) {
        // LSP failed to start - extension still works for syntax highlighting, outline, go-to-def
        console.warn('Ur/Web LSP not available:', e instanceof Error ? e.message : e);
    }
}

export async function deactivate(): Promise<void> {
    await stopLspClient();
}
