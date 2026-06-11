import * as fs from 'fs';
import * as path from 'path';
import * as vscode from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind,
} from 'vscode-languageclient/node';

// One server per project root (the directory containing the .urp file), mirroring
// the nvim `root_dir` behaviour. Keyed by the absolute root path.
const clients = new Map<string, LanguageClient>();

const URWEB_LANGUAGES = ['ur', 'urs'];

/**
 * Walk up from a file to the nearest ancestor directory that contains a `.urp`
 * project file. The Ur/Web LSP server scans its root for exactly one `.urp`,
 * so this is the directory we use as the workspace root.
 */
function findUrpRoot(filePath: string): string | undefined {
    let dir = path.dirname(filePath);
    // eslint-disable-next-line no-constant-condition
    while (true) {
        try {
            if (fs.readdirSync(dir).some((name) => name.endsWith('.urp'))) {
                return dir;
            }
        } catch {
            // Unreadable directory — stop searching this branch.
            return undefined;
        }
        const parent = path.dirname(dir);
        if (parent === dir) {
            return undefined;
        }
        dir = parent;
    }
}

function startClientForRoot(root: string): void {
    if (clients.has(root)) {
        return;
    }

    const config = vscode.workspace.getConfiguration('urweb');
    const command = config.get<string>('server.path', 'urweb');
    const args = config.get<string[]>('server.args', ['-startLspServer']);

    const serverOptions: ServerOptions = {
        run: { command, args, transport: TransportKind.stdio },
        debug: { command, args, transport: TransportKind.stdio },
    };

    const rootUri = vscode.Uri.file(root);
    const clientOptions: LanguageClientOptions = {
        // Scope this client to documents under its own project root so that, in a
        // multi-project workspace, each server only handles its own files.
        documentSelector: URWEB_LANGUAGES.map((language) => ({
            scheme: 'file',
            language,
            pattern: path.join(root, '**', '*'),
        })),
        // Make the .urp directory the LSP root, exactly like the nvim config.
        workspaceFolder: {
            uri: rootUri,
            name: path.basename(root),
            index: 0,
        },
    };

    const client = new LanguageClient(
        'urweb',
        'Ur/Web Language Server',
        serverOptions,
        clientOptions
    );
    clients.set(root, client);
    client.start();
}

function maybeStartForDocument(doc: vscode.TextDocument): void {
    if (!URWEB_LANGUAGES.includes(doc.languageId)) {
        return;
    }
    if (doc.uri.scheme !== 'file') {
        return;
    }
    const root = findUrpRoot(doc.uri.fsPath);
    // Mirror nvim: only start the server once a project root is found.
    if (root) {
        startClientForRoot(root);
    }
}

export function activate(context: vscode.ExtensionContext): void {
    // Start a server for any Ur/Web files already open, and for any opened later.
    vscode.workspace.textDocuments.forEach(maybeStartForDocument);
    context.subscriptions.push(
        vscode.workspace.onDidOpenTextDocument(maybeStartForDocument)
    );
}

export async function deactivate(): Promise<void> {
    await Promise.all([...clients.values()].map((client) => client.stop()));
    clients.clear();
}
