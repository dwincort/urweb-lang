import * as vscode from 'vscode';

/**
 * Represents a container (structure, signature, functor) that can hold child symbols.
 */
interface SymbolContainer {
    symbol: vscode.DocumentSymbol;
    kind: 'struct' | 'sig';
}

/**
 * Token types for the lexer.
 */
interface Token {
    type: 'keyword' | 'ident' | 'struct' | 'sig' | 'end' | 'let' | 'in' | 'lparen' | 'rparen' | 'colon' | 'equals' | 'other';
    value: string;
    offset: number;
    line: number;
}

/**
 * Provides document symbols for Ur/Web source files.
 * Supports Go to Symbol (Ctrl+Shift+O) and the Outline view.
 * Builds hierarchical symbol trees for nested structures/signatures/functors.
 */
export class UrWebDocumentSymbolProvider implements vscode.DocumentSymbolProvider {

    // Declaration keywords and their symbol kinds
    private static readonly DECLARATION_KINDS: Map<string, vscode.SymbolKind> = new Map([
        ['fun', vscode.SymbolKind.Function],
        ['val', vscode.SymbolKind.Variable],
        ['datatype', vscode.SymbolKind.Enum],
        ['type', vscode.SymbolKind.TypeParameter],
        ['con', vscode.SymbolKind.TypeParameter],
        ['structure', vscode.SymbolKind.Module],
        ['signature', vscode.SymbolKind.Interface],
        ['functor', vscode.SymbolKind.Function],
        ['table', vscode.SymbolKind.Struct],
        ['view', vscode.SymbolKind.Struct],
        ['sequence', vscode.SymbolKind.Variable],
        ['cookie', vscode.SymbolKind.Variable],
        ['style', vscode.SymbolKind.Variable],
        ['task', vscode.SymbolKind.Event],
        ['class', vscode.SymbolKind.Class],
        ['policy', vscode.SymbolKind.Object],
    ]);

    // Container keywords that open a block
    private static readonly CONTAINER_KEYWORDS = new Set(['structure', 'signature', 'functor']);

    public provideDocumentSymbols(
        document: vscode.TextDocument,
        _token: vscode.CancellationToken
    ): vscode.ProviderResult<vscode.DocumentSymbol[]> {
        try {
            const text = document.getText();
            const tokens = this.tokenize(text);
            return this.parseSymbols(document, tokens);
        } catch (e) {
            console.error('UrWeb symbol provider error:', e);
            return [];
        }
    }

    /**
     * Simple tokenizer that extracts keywords, identifiers, and structural tokens.
     * Skips comments, string literals, and XML blocks.
     */
    private tokenize(text: string): Token[] {
        const tokens: Token[] = [];
        let i = 0;
        let line = 0;

        while (i < text.length) {
            // Track line numbers
            if (text[i] === '\n') {
                line++;
                i++;
                continue;
            }

            // Skip whitespace
            if (/\s/.test(text[i])) {
                i++;
                continue;
            }

            // Skip comments (* ... *)
            if (text[i] === '(' && text[i + 1] === '*') {
                i += 2;
                let depth = 1;
                while (i < text.length && depth > 0) {
                    if (text[i] === '\n') line++;
                    if (text[i] === '(' && text[i + 1] === '*') {
                        depth++;
                        i += 2;
                    } else if (text[i] === '*' && text[i + 1] === ')') {
                        depth--;
                        i += 2;
                    } else {
                        i++;
                    }
                }
                continue;
            }

            // Skip string literals "..."
            if (text[i] === '"') {
                i++; // skip opening quote
                while (i < text.length) {
                    if (text[i] === '\n') line++;
                    if (text[i] === '\\' && i + 1 < text.length) {
                        // Skip escaped character
                        if (text[i + 1] === '\n') line++;
                        i += 2;
                    } else if (text[i] === '"') {
                        i++; // skip closing quote
                        break;
                    } else {
                        i++;
                    }
                }
                continue;
            }

            // Skip XML blocks <xml>...</xml> (nested)
            if (text.substring(i, i + 5) === '<xml>') {
                i += 5; // skip '<xml>'
                let depth = 1;
                while (i < text.length && depth > 0) {
                    if (text[i] === '\n') line++;
                    if (text.substring(i, i + 5) === '<xml>') {
                        depth++;
                        i += 5;
                    } else if (text.substring(i, i + 6) === '</xml>') {
                        depth--;
                        i += 6;
                    } else {
                        i++;
                    }
                }
                continue;
            }

            // Identifiers and keywords
            if (/[a-zA-Z_]/.test(text[i])) {
                const start = i;
                while (i < text.length && /[a-zA-Z0-9_']/.test(text[i])) {
                    i++;
                }
                const value = text.substring(start, i);
                let type: Token['type'] = 'ident';

                if (value === 'struct') type = 'struct';
                else if (value === 'sig') type = 'sig';
                else if (value === 'end') type = 'end';
                else if (value === 'let') type = 'let';
                else if (value === 'in') type = 'in';
                else if (UrWebDocumentSymbolProvider.DECLARATION_KINDS.has(value)) type = 'keyword';

                tokens.push({ type, value, offset: start, line });
                continue;
            }

            // Structural characters
            if (text[i] === '(') {
                tokens.push({ type: 'lparen', value: '(', offset: i, line });
                i++;
                continue;
            }
            if (text[i] === ')') {
                tokens.push({ type: 'rparen', value: ')', offset: i, line });
                i++;
                continue;
            }
            if (text[i] === ':') {
                tokens.push({ type: 'colon', value: ':', offset: i, line });
                i++;
                continue;
            }
            if (text[i] === '=') {
                tokens.push({ type: 'equals', value: '=', offset: i, line });
                i++;
                continue;
            }

            // Skip other characters
            i++;
        }

        return tokens;
    }

    /**
     * Parse tokens into a hierarchical symbol tree.
     */
    private parseSymbols(document: vscode.TextDocument, tokens: Token[]): vscode.DocumentSymbol[] {
        const rootSymbols: vscode.DocumentSymbol[] = [];
        const containerStack: SymbolContainer[] = [];
        let i = 0;
        // Track let...in...end depth to avoid confusing let's end with struct/sig's end
        let letDepth = 0;

        const addSymbol = (symbol: vscode.DocumentSymbol): void => {
            if (containerStack.length > 0) {
                containerStack[containerStack.length - 1].symbol.children.push(symbol);
            } else {
                rootSymbols.push(symbol);
            }
        };

        const pushContainer = (symbol: vscode.DocumentSymbol, kind: 'struct' | 'sig'): void => {
            addSymbol(symbol);
            containerStack.push({ symbol, kind });
        };

        while (i < tokens.length) {
            const token = tokens[i];

            // Track let...in...end blocks - 'let' opens, 'end' closes
            if (token.type === 'let') {
                letDepth++;
                i++;
                continue;
            }

            // 'in' is just part of let...in...end, doesn't change depth
            if (token.type === 'in') {
                i++;
                continue;
            }

            // Handle 'end' - only close container if not inside a let block
            if (token.type === 'end') {
                if (letDepth > 0) {
                    // This 'end' closes a let...in block, not a struct/sig
                    letDepth--;
                    i++;
                    continue;
                }
                if (containerStack.length > 0) {
                    const container = containerStack.pop()!;
                    // Update the container's range to include this 'end'
                    const endPos = document.positionAt(token.offset + 3);
                    container.symbol.range = new vscode.Range(
                        container.symbol.range.start,
                        endPos
                    );
                }
                i++;
                continue;
            }

            // Handle declaration keywords
            if (token.type === 'keyword') {
                const keyword = token.value;
                const kind = UrWebDocumentSymbolProvider.DECLARATION_KINDS.get(keyword)!;
                const startPos = document.positionAt(token.offset);

                // Look for the name (next identifier)
                let nameToken: Token | null = null;
                let j = i + 1;

                // Skip 'rec' after 'val'
                if (keyword === 'val' && j < tokens.length && tokens[j].type === 'ident' && tokens[j].value === 'rec') {
                    j++;
                }

                if (j < tokens.length && tokens[j].type === 'ident') {
                    nameToken = tokens[j];
                }

                if (!nameToken) {
                    i++;
                    continue;
                }

                const name = nameToken.value;
                const selectionEnd = document.positionAt(nameToken.offset + nameToken.value.length);

                // For container keywords (structure, signature, functor), check if they have a body
                if (UrWebDocumentSymbolProvider.CONTAINER_KEYWORDS.has(keyword)) {
                    const result = this.parseContainerDeclaration(document, tokens, i, kind, name, startPos, selectionEnd);
                    if (result) {
                        if (result.isContainer) {
                            pushContainer(result.symbol, result.containerKind!);
                            // Add any parameter symbols as children
                            for (const paramSymbol of result.parameterSymbols) {
                                result.symbol.children.push(paramSymbol);
                            }
                        } else {
                            addSymbol(result.symbol);
                        }
                        i = result.nextIndex;
                        continue;
                    }
                }

                // Regular declaration - find a reasonable end position
                const endPos = this.findSimpleDeclarationEnd(document, tokens, j + 1);
                const symbol = new vscode.DocumentSymbol(
                    name,
                    this.getSymbolDetail(keyword),
                    kind,
                    new vscode.Range(startPos, endPos),
                    new vscode.Range(startPos, selectionEnd)
                );
                addSymbol(symbol);
                i = j + 1;
                continue;
            }

            // Handle standalone 'struct' or 'sig' (anonymous, within expressions)
            // These are typically in functor applications like F(struct ... end)
            // We skip these as they don't define named symbols
            if (token.type === 'struct' || token.type === 'sig') {
                // Skip to matching 'end'
                i = this.skipToMatchingEnd(tokens, i + 1);
                continue;
            }

            i++;
        }

        return rootSymbols;
    }

    /**
     * Parse a container declaration (structure, signature, functor).
     * Returns the symbol and whether it opens a container.
     */
    private parseContainerDeclaration(
        document: vscode.TextDocument,
        tokens: Token[],
        startIndex: number,
        kind: vscode.SymbolKind,
        name: string,
        startPos: vscode.Position,
        selectionEnd: vscode.Position
    ): {
        symbol: vscode.DocumentSymbol;
        isContainer: boolean;
        containerKind?: 'struct' | 'sig';
        parameterSymbols: vscode.DocumentSymbol[];
        nextIndex: number;
    } | null {
        const keyword = tokens[startIndex].value;
        let j = startIndex + 1;

        // Skip the name
        if (j < tokens.length && tokens[j].type === 'ident') {
            j++;
        } else {
            return null;
        }

        const parameterSymbols: vscode.DocumentSymbol[] = [];
        let containerKind: 'struct' | 'sig' | undefined;

        // For functor, parse parameter list: (M : sig ... end)
        if (keyword === 'functor') {
            while (j < tokens.length && tokens[j].type === 'lparen') {
                j++; // skip '('

                // Look for parameter name
                if (j < tokens.length && tokens[j].type === 'ident') {
                    const paramName = tokens[j].value;
                    const paramStart = document.positionAt(tokens[j].offset);
                    j++;

                    // Check for ': sig'
                    if (j < tokens.length && tokens[j].type === 'colon') {
                        j++; // skip ':'

                        if (j < tokens.length && tokens[j].type === 'sig') {
                            // Parse the signature as a nested container
                            // Save the parameter name token position before incrementing j
                            const paramNameOffset = tokens[j - 2].offset; // j-2 is the param name after colon skip and before sig skip
                            j++; // skip 'sig'

                            // Parse declarations inside the sig until 'end'
                            const sigResult = this.parseNestedBlock(document, tokens, j, 'sig');
                            j = sigResult.nextIndex;

                            // Calculate the full range (must contain selection range)
                            const paramSelectionEnd = document.positionAt(paramNameOffset + paramName.length);
                            let paramEndPos = paramSelectionEnd; // Default: selection range = full range
                            if (j > 0 && j <= tokens.length && tokens[j - 1]) {
                                const endOffset = tokens[j - 1].offset;
                                paramEndPos = document.positionAt(endOffset + 3);
                            }

                            const paramSymbol = new vscode.DocumentSymbol(
                                paramName,
                                'parameter',
                                vscode.SymbolKind.Interface,
                                new vscode.Range(paramStart, paramEndPos),
                                new vscode.Range(paramStart, paramSelectionEnd)
                            );

                            for (const child of sigResult.symbols) {
                                paramSymbol.children.push(child);
                            }

                            parameterSymbols.push(paramSymbol);
                        }
                    }
                }

                // Skip to closing ')'
                let parenDepth = 1;
                while (j < tokens.length && parenDepth > 0) {
                    if (tokens[j].type === 'lparen') parenDepth++;
                    else if (tokens[j].type === 'rparen') parenDepth--;
                    j++;
                }
            }
        }

        // Look for ':' (type annotation) or '=' followed by 'struct', 'sig', or module reference
        // We need to scan until we find struct/sig or hit another declaration/end
        while (j < tokens.length) {
            const t = tokens[j];

            if (t.type === 'struct') {
                containerKind = 'struct';
                j++; // skip 'struct'
                break;
            } else if (t.type === 'sig') {
                containerKind = 'sig';
                j++; // skip 'sig'
                break;
            } else if (t.type === 'keyword' || t.type === 'end') {
                // Hit another declaration or end - this container doesn't have an inline body
                break;
            } else {
                j++;
            }
        }

        // Create the symbol
        const endPos = containerKind
            ? document.positionAt(tokens[Math.min(j, tokens.length - 1)].offset)
            : this.findSimpleDeclarationEnd(document, tokens, j);

        const symbol = new vscode.DocumentSymbol(
            name,
            this.getSymbolDetail(keyword),
            kind,
            new vscode.Range(startPos, endPos),
            new vscode.Range(startPos, selectionEnd)
        );

        return {
            symbol,
            isContainer: containerKind !== undefined,
            containerKind,
            parameterSymbols,
            nextIndex: j
        };
    }

    /**
     * Parse a nested block (sig...end or struct...end) and return its symbols.
     */
    private parseNestedBlock(
        document: vscode.TextDocument,
        tokens: Token[],
        startIndex: number,
        blockKind: 'struct' | 'sig'
    ): { symbols: vscode.DocumentSymbol[]; nextIndex: number } {
        const symbols: vscode.DocumentSymbol[] = [];
        let i = startIndex;
        let depth = 1;
        let letDepth = 0;

        // Safety check
        if (startIndex >= tokens.length) {
            return { symbols: [], nextIndex: startIndex };
        }

        while (i < tokens.length && depth > 0) {
            const token = tokens[i];

            // Track let...in...end blocks
            if (token.type === 'let') {
                letDepth++;
                i++;
                continue;
            }

            if (token.type === 'in') {
                i++;
                continue;
            }

            if (token.type === 'struct' || token.type === 'sig') {
                depth++;
                i++;
                continue;
            }

            if (token.type === 'end') {
                if (letDepth > 0) {
                    // This 'end' closes a let block, not a struct/sig
                    letDepth--;
                    i++;
                    continue;
                }
                depth--;
                if (depth === 0) {
                    i++; // consume the 'end'
                    break;
                }
                i++;
                continue;
            }

            // Parse declarations at this level
            if (token.type === 'keyword' && depth === 1) {
                const keyword = token.value;
                const kind = UrWebDocumentSymbolProvider.DECLARATION_KINDS.get(keyword)!;
                const startPos = document.positionAt(token.offset);

                // Look for the name
                let j = i + 1;
                if (keyword === 'val' && j < tokens.length && tokens[j].type === 'ident' && tokens[j].value === 'rec') {
                    j++;
                }

                if (j < tokens.length && tokens[j].type === 'ident') {
                    const nameToken = tokens[j];
                    const selectionEnd = document.positionAt(nameToken.offset + nameToken.value.length);

                    // Check if this is a nested container
                    if (UrWebDocumentSymbolProvider.CONTAINER_KEYWORDS.has(keyword)) {
                        // Look ahead for struct/sig
                        let k = j + 1;
                        let foundBlock: 'struct' | 'sig' | null = null;
                        while (k < tokens.length && k < j + 15) {
                            if (tokens[k].type === 'struct') {
                                foundBlock = 'struct';
                                break;
                            } else if (tokens[k].type === 'sig') {
                                foundBlock = 'sig';
                                break;
                            } else if (tokens[k].type === 'keyword' || tokens[k].type === 'end') {
                                break;
                            }
                            k++;
                        }

                        if (foundBlock) {
                            // Recursively parse the nested block first to get the end position
                            const nested = this.parseNestedBlock(document, tokens, k + 1, foundBlock);

                            // Calculate the full range (must contain selection range)
                            let symbolEndPos = selectionEnd; // Default: selection range = full range
                            if (nested.nextIndex > 0 && nested.nextIndex <= tokens.length) {
                                const endToken = tokens[nested.nextIndex - 1];
                                symbolEndPos = document.positionAt(endToken.offset + 3);
                            }

                            const symbol = new vscode.DocumentSymbol(
                                nameToken.value,
                                this.getSymbolDetail(keyword),
                                kind,
                                new vscode.Range(startPos, symbolEndPos),
                                new vscode.Range(startPos, selectionEnd)
                            );

                            for (const child of nested.symbols) {
                                symbol.children.push(child);
                            }

                            symbols.push(symbol);
                            i = nested.nextIndex;
                            continue;
                        }
                    }

                    // Simple declaration
                    const endPos = this.findSimpleDeclarationEnd(document, tokens, j + 1);
                    const symbol = new vscode.DocumentSymbol(
                        nameToken.value,
                        this.getSymbolDetail(keyword),
                        kind,
                        new vscode.Range(startPos, endPos),
                        new vscode.Range(startPos, selectionEnd)
                    );
                    symbols.push(symbol);
                    i = j + 1;
                    continue;
                }
            }

            i++;
        }

        return { symbols, nextIndex: i };
    }

    /**
     * Skip to the matching 'end' for an anonymous struct/sig block.
     */
    private skipToMatchingEnd(tokens: Token[], startIndex: number): number {
        let depth = 1;
        let letDepth = 0;
        let i = startIndex;

        while (i < tokens.length && depth > 0) {
            if (tokens[i].type === 'let') {
                letDepth++;
            } else if (tokens[i].type === 'struct' || tokens[i].type === 'sig') {
                depth++;
            } else if (tokens[i].type === 'end') {
                if (letDepth > 0) {
                    letDepth--;
                } else {
                    depth--;
                }
            }
            i++;
        }

        return i;
    }

    /**
     * Find a reasonable end position for a simple (non-container) declaration.
     */
    private findSimpleDeclarationEnd(
        document: vscode.TextDocument,
        tokens: Token[],
        startIndex: number
    ): vscode.Position {
        // Look for the next declaration keyword or end
        for (let i = startIndex; i < tokens.length; i++) {
            const t = tokens[i];
            if (t.type === 'keyword' || t.type === 'end') {
                // End just before this token
                if (i > 0) {
                    return document.positionAt(tokens[i].offset);
                }
                break;
            }
        }

        // Default to end of document
        return document.positionAt(document.getText().length);
    }

    /**
     * Get a human-readable detail string for a keyword.
     */
    private getSymbolDetail(keyword: string): string {
        switch (keyword) {
            case 'fun': return 'function';
            case 'val': return 'value';
            case 'datatype': return 'datatype';
            case 'type': return 'type';
            case 'con': return 'constructor';
            case 'structure': return 'structure';
            case 'signature': return 'signature';
            case 'functor': return 'functor';
            case 'table': return 'table';
            case 'view': return 'view';
            case 'sequence': return 'sequence';
            case 'cookie': return 'cookie';
            case 'style': return 'style';
            case 'task': return 'task';
            case 'class': return 'class';
            case 'policy': return 'policy';
            default: return '';
        }
    }
}
