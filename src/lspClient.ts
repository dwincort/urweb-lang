import * as vscode from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

export async function startLspClient(context: vscode.ExtensionContext): Promise<LanguageClient | undefined> {
    const config = vscode.workspace.getConfiguration('urweb');

    if (!config.get<boolean>('enableLsp', true)) {
        return undefined;
    }

    const urwebPath = config.get<string>('path', 'urweb');
    const urpFile = config.get<string>('urpFile', '');

    const serverOptions: ServerOptions = {
        command: urwebPath,
        args: ['-startLspServer']
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [
            { scheme: 'file', language: 'ur' },
            { scheme: 'file', language: 'urs' }
        ],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.{ur,urs,urp}')
        },
        initializationOptions: urpFile ? {
            urweb: {
                project: {
                    urpfile: urpFile
                }
            }
        } : {}
    };

    client = new LanguageClient(
        'urweb',
        'Ur/Web Language Server',
        serverOptions,
        clientOptions
    );

    try {
        await client.start();
        context.subscriptions.push(client);
        return client;
    } catch (e) {
        client = undefined;
        throw e;
    }
}

export function getClient(): LanguageClient | undefined {
    return client;
}

export async function stopLspClient(): Promise<void> {
    if (client) {
        await client.stop();
        client = undefined;
    }
}
