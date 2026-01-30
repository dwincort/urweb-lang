import * as vscode from 'vscode';

/**
 * Token types for the lexer (extended from symbolProvider with 'dot' and 'open').
 */
interface Token {
    type: 'keyword' | 'ident' | 'struct' | 'sig' | 'end' | 'let' | 'in' | 'lparen' | 'rparen' | 'colon' | 'equals' | 'dot' | 'open' | 'and' | 'other';
    value: string;
    offset: number;
    line: number;
}

/**
 * Kinds of definitions we track.
 */
type DefinitionKind = 'val' | 'fun' | 'type' | 'datatype' | 'con' | 'structure' | 'signature' | 'functor' | 'table' | 'view' | 'sequence' | 'cookie' | 'style' | 'task' | 'class' | 'policy' | 'functor-param';

/**
 * A definition (binding) in the scope tree.
 */
interface Definition {
    name: string;
    kind: DefinitionKind;
    range: vscode.Range;
    selectionRange: vscode.Range;
    offset: number;
    moduleScope?: Scope;  // For structures/functors: their inner scope
}

/**
 * A module that was opened via `open M`.
 */
interface OpenedModule {
    moduleRef: Definition;  // The module that was opened
    offset: number;         // Where `open` appeared (for ordering)
}

/**
 * A lexical scope in the scope tree.
 */
interface Scope {
    parent: Scope | null;
    definitions: Map<string, Definition[]>;  // Name to definitions (multiple for shadowing)
    opens: OpenedModule[];  // Modules opened via `open M`
    children: Scope[];
    startOffset: number;
    endOffset: number;
    kind: 'file' | 'structure' | 'signature' | 'functor' | 'let' | 'functor-param';
    name?: string;
}

/**
 * Declaration keywords and their definition kinds.
 */
const DECLARATION_KINDS: Map<string, DefinitionKind> = new Map([
    ['fun', 'fun'],
    ['val', 'val'],
    ['datatype', 'datatype'],
    ['type', 'type'],
    ['con', 'con'],
    ['structure', 'structure'],
    ['signature', 'signature'],
    ['functor', 'functor'],
    ['table', 'table'],
    ['view', 'view'],
    ['sequence', 'sequence'],
    ['cookie', 'cookie'],
    ['style', 'style'],
    ['task', 'task'],
    ['class', 'class'],
    ['policy', 'policy'],
]);

/**
 * Keywords that create container scopes (structure, signature, functor).
 */
const CONTAINER_KEYWORDS = new Set(['structure', 'signature', 'functor']);

/**
 * Tokenizer for Ur/Web code.
 * Skips comments, string literals, and XML blocks.
 * Extended to recognize 'dot', 'open', and 'and' tokens.
 */
function tokenize(text: string): Token[] {
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
            else if (value === 'open') type = 'open';
            else if (value === 'and') type = 'and';
            else if (DECLARATION_KINDS.has(value)) type = 'keyword';

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
        if (text[i] === '.') {
            tokens.push({ type: 'dot', value: '.', offset: i, line });
            i++;
            continue;
        }

        // Skip other characters
        i++;
    }

    return tokens;
}

/**
 * Build the scope tree from tokens.
 */
class ScopeBuilder {
    private document: vscode.TextDocument;
    private tokens: Token[];
    private index: number = 0;
    private rootScope: Scope;
    private currentScope: Scope;

    constructor(document: vscode.TextDocument, tokens: Token[]) {
        this.document = document;
        this.tokens = tokens;
        this.rootScope = this.createScope(null, 0, document.getText().length, 'file');
        this.currentScope = this.rootScope;
    }

    private createScope(parent: Scope | null, startOffset: number, endOffset: number, kind: Scope['kind'], name?: string): Scope {
        const scope: Scope = {
            parent,
            definitions: new Map(),
            opens: [],
            children: [],
            startOffset,
            endOffset,
            kind,
            name
        };
        if (parent) {
            parent.children.push(scope);
        }
        return scope;
    }

    private addDefinition(scope: Scope, def: Definition): void {
        const existing = scope.definitions.get(def.name);
        if (existing) {
            existing.push(def);
        } else {
            scope.definitions.set(def.name, [def]);
        }
    }

    private peek(offset: number = 0): Token | undefined {
        return this.tokens[this.index + offset];
    }

    private advance(): Token | undefined {
        return this.tokens[this.index++];
    }

    private match(type: Token['type']): boolean {
        if (this.peek()?.type === type) {
            this.advance();
            return true;
        }
        return false;
    }

    public build(): Scope {
        this.parseScope(this.rootScope, 0);
        return this.rootScope;
    }

    private parseScope(scope: Scope, letDepth: number): void {
        while (this.index < this.tokens.length) {
            const token = this.peek();
            if (!token) break;

            // Handle 'end' - close current scope if not in let
            if (token.type === 'end') {
                if (letDepth > 0) {
                    // This 'end' closes a let block
                    this.advance();
                    return;
                }
                // End closes this scope (struct/sig/functor-param)
                scope.endOffset = token.offset + 3;
                this.advance();
                return;
            }

            // Handle 'let' - push into a let scope
            if (token.type === 'let') {
                const letStart = token.offset;
                this.advance();
                const letScope = this.createScope(scope, letStart, scope.endOffset, 'let');
                this.parseScope(letScope, letDepth + 1);
                letScope.endOffset = this.peek()?.offset ?? scope.endOffset;
                continue;
            }

            // Handle 'in' - we're in a let...in...end block, continue parsing
            if (token.type === 'in') {
                this.advance();
                continue;
            }

            // Handle 'open M'
            if (token.type === 'open') {
                const openOffset = token.offset;
                this.advance();
                const moduleNameToken = this.peek();
                if (moduleNameToken?.type === 'ident') {
                    const moduleName = moduleNameToken.value;
                    this.advance();
                    // We'll resolve the module reference later during resolution
                    // For now, store a placeholder
                    const placeholder: Definition = {
                        name: moduleName,
                        kind: 'structure',
                        range: new vscode.Range(
                            this.document.positionAt(moduleNameToken.offset),
                            this.document.positionAt(moduleNameToken.offset + moduleName.length)
                        ),
                        selectionRange: new vscode.Range(
                            this.document.positionAt(moduleNameToken.offset),
                            this.document.positionAt(moduleNameToken.offset + moduleName.length)
                        ),
                        offset: moduleNameToken.offset,
                    };
                    scope.opens.push({
                        moduleRef: placeholder,
                        offset: openOffset
                    });
                }
                continue;
            }

            // Handle declaration keywords
            if (token.type === 'keyword') {
                this.parseDeclaration(scope);
                continue;
            }

            // Handle standalone 'struct' or 'sig' (anonymous blocks)
            if (token.type === 'struct' || token.type === 'sig') {
                this.advance();
                this.skipToMatchingEnd(0);
                continue;
            }

            // Skip other tokens
            this.advance();
        }
    }

    private parseDeclaration(scope: Scope): void {
        const keywordToken = this.advance();
        if (!keywordToken || keywordToken.type !== 'keyword') return;

        const keyword = keywordToken.value;
        const kind = DECLARATION_KINDS.get(keyword);
        if (!kind) return;

        // Handle 'val rec'
        if (keyword === 'val' && this.peek()?.type === 'ident' && this.peek()?.value === 'rec') {
            this.advance(); // skip 'rec'
        }

        // Get the name
        const nameToken = this.peek();
        if (!nameToken || nameToken.type !== 'ident') return;
        this.advance();

        const startPos = this.document.positionAt(keywordToken.offset);
        const nameEndPos = this.document.positionAt(nameToken.offset + nameToken.value.length);

        // Create the definition
        const def: Definition = {
            name: nameToken.value,
            kind,
            range: new vscode.Range(startPos, nameEndPos),
            selectionRange: new vscode.Range(startPos, nameEndPos),
            offset: nameToken.offset,
        };

        // For container keywords, check if they have a body
        if (CONTAINER_KEYWORDS.has(keyword)) {
            this.parseContainerBody(scope, def, keyword);
        } else {
            this.addDefinition(scope, def);
            // Handle 'and' for mutual recursion (fun f ... and g ...)
            while (this.peek()?.type === 'and') {
                this.advance(); // skip 'and'
                const andNameToken = this.peek();
                if (andNameToken?.type === 'ident') {
                    this.advance();
                    const andDef: Definition = {
                        name: andNameToken.value,
                        kind,
                        range: new vscode.Range(
                            this.document.positionAt(andNameToken.offset),
                            this.document.positionAt(andNameToken.offset + andNameToken.value.length)
                        ),
                        selectionRange: new vscode.Range(
                            this.document.positionAt(andNameToken.offset),
                            this.document.positionAt(andNameToken.offset + andNameToken.value.length)
                        ),
                        offset: andNameToken.offset,
                    };
                    this.addDefinition(scope, andDef);
                }
            }
        }
    }

    private parseContainerBody(scope: Scope, def: Definition, keyword: string): void {
        // For functor, parse parameter list: (M : sig ... end)
        if (keyword === 'functor') {
            this.parseFunctorParams(scope, def);
        }

        // Look for ':' or '=' followed by 'struct', 'sig', or identifier
        while (this.index < this.tokens.length) {
            const t = this.peek();
            if (!t) break;

            if (t.type === 'struct') {
                this.advance();
                // Create the structure's inner scope
                const innerScope = this.createScope(scope, t.offset, scope.endOffset, 'structure', def.name);
                def.moduleScope = innerScope;
                this.addDefinition(scope, def);
                this.parseScope(innerScope, 0);
                return;
            } else if (t.type === 'sig') {
                this.advance();
                // Create the signature's inner scope
                const innerScope = this.createScope(scope, t.offset, scope.endOffset, 'signature', def.name);
                def.moduleScope = innerScope;
                this.addDefinition(scope, def);
                this.parseScope(innerScope, 0);
                return;
            } else if (t.type === 'keyword' || t.type === 'end') {
                // Hit another declaration or end - no inline body
                this.addDefinition(scope, def);
                return;
            } else {
                this.advance();
            }
        }

        // No body found
        this.addDefinition(scope, def);
    }

    private parseFunctorParams(scope: Scope, functorDef: Definition): void {
        while (this.peek()?.type === 'lparen') {
            this.advance(); // skip '('

            const paramNameToken = this.peek();
            if (paramNameToken?.type === 'ident') {
                this.advance();

                // Check for ': sig'
                if (this.peek()?.type === 'colon') {
                    this.advance(); // skip ':'

                    if (this.peek()?.type === 'sig') {
                        const sigToken = this.peek()!;
                        this.advance(); // skip 'sig'

                        // Create scope for functor parameter
                        const paramScope = this.createScope(scope, sigToken.offset, scope.endOffset, 'functor-param', paramNameToken.value);

                        // Create definition for the functor parameter
                        const paramDef: Definition = {
                            name: paramNameToken.value,
                            kind: 'functor-param',
                            range: new vscode.Range(
                                this.document.positionAt(paramNameToken.offset),
                                this.document.positionAt(paramNameToken.offset + paramNameToken.value.length)
                            ),
                            selectionRange: new vscode.Range(
                                this.document.positionAt(paramNameToken.offset),
                                this.document.positionAt(paramNameToken.offset + paramNameToken.value.length)
                            ),
                            offset: paramNameToken.offset,
                            moduleScope: paramScope
                        };

                        // Parse the signature content into the param scope
                        this.parseScope(paramScope, 0);

                        // Add the functor parameter as a definition in the current scope
                        // (it will be visible in the functor body)
                        this.addDefinition(scope, paramDef);
                    }
                }
            }

            // Skip to closing ')'
            let parenDepth = 1;
            while (this.index < this.tokens.length && parenDepth > 0) {
                const t = this.peek();
                if (t?.type === 'lparen') parenDepth++;
                else if (t?.type === 'rparen') parenDepth--;
                this.advance();
            }
        }
    }

    private skipToMatchingEnd(initialLetDepth: number): void {
        let depth = 1;
        let letDepth = initialLetDepth;

        while (this.index < this.tokens.length && depth > 0) {
            const t = this.peek();
            if (t?.type === 'let') {
                letDepth++;
            } else if (t?.type === 'struct' || t?.type === 'sig') {
                depth++;
            } else if (t?.type === 'end') {
                if (letDepth > 0) {
                    letDepth--;
                } else {
                    depth--;
                }
            }
            this.advance();
        }
    }
}

/**
 * Resolver for finding definitions given a position.
 */
class DefinitionResolver {
    private rootScope: Scope;
    private document: vscode.TextDocument;

    constructor(rootScope: Scope, document: vscode.TextDocument) {
        this.rootScope = rootScope;
        this.document = document;
    }

    /**
     * Find the innermost scope containing the given offset.
     */
    public findScopeAt(offset: number): Scope {
        return this.findScopeAtRecursive(this.rootScope, offset);
    }

    private findScopeAtRecursive(scope: Scope, offset: number): Scope {
        for (const child of scope.children) {
            if (offset >= child.startOffset && offset <= child.endOffset) {
                return this.findScopeAtRecursive(child, offset);
            }
        }
        return scope;
    }

    /**
     * Resolve a simple name in the given scope at the given usage offset.
     * Walks up the scope chain, checking direct definitions and opened modules.
     */
    public resolveSimpleName(name: string, scope: Scope, usageOffset: number): Definition | null {
        let currentScope: Scope | null = scope;

        while (currentScope) {
            // Check direct definitions (prefer later definitions for shadowing)
            const defs = currentScope.definitions.get(name);
            if (defs) {
                // Find the latest definition before usageOffset
                for (let i = defs.length - 1; i >= 0; i--) {
                    if (defs[i].offset < usageOffset) {
                        return defs[i];
                    }
                }
            }

            // Check opened modules (later opens shadow earlier ones)
            for (let i = currentScope.opens.length - 1; i >= 0; i--) {
                const opened = currentScope.opens[i];
                if (opened.offset < usageOffset) {
                    // Resolve the module reference first
                    const moduleRef = this.resolveSimpleName(opened.moduleRef.name, currentScope, opened.offset);
                    if (moduleRef?.moduleScope) {
                        // Search in the module's scope (no offset restriction inside module)
                        const result = this.searchInModuleScope(name, moduleRef.moduleScope);
                        if (result) return result;
                    }
                }
            }

            currentScope = currentScope.parent;
        }

        return null;
    }

    /**
     * Search for a name within a module's scope (no offset restriction).
     */
    private searchInModuleScope(name: string, moduleScope: Scope): Definition | null {
        const defs = moduleScope.definitions.get(name);
        if (defs && defs.length > 0) {
            return defs[defs.length - 1]; // Latest definition
        }

        // Check opened modules within this module scope
        for (let i = moduleScope.opens.length - 1; i >= 0; i--) {
            const opened = moduleScope.opens[i];
            // Resolve within module scope
            const moduleRef = this.searchInModuleScope(opened.moduleRef.name, moduleScope);
            if (moduleRef?.moduleScope) {
                const result = this.searchInModuleScope(name, moduleRef.moduleScope);
                if (result) return result;
            }
        }

        return null;
    }

    /**
     * Resolve a qualified name like A.B.c.
     */
    public resolveQualifiedName(parts: string[], scope: Scope, usageOffset: number): Definition | null {
        if (parts.length === 0) return null;

        // Resolve the first part in the current scope
        let currentDef = this.resolveSimpleName(parts[0], scope, usageOffset);
        if (!currentDef) return null;

        // Resolve remaining parts within module scopes
        for (let i = 1; i < parts.length; i++) {
            if (!currentDef.moduleScope) return null;
            const nextDef = this.searchInModuleScope(parts[i], currentDef.moduleScope);
            if (!nextDef) return null;
            currentDef = nextDef;
        }

        return currentDef;
    }
}

/**
 * Get the word (identifier) at the given position, including qualified names.
 * Returns the parts of the qualified name and the range.
 */
function getQualifiedNameAtPosition(document: vscode.TextDocument, position: vscode.Position): { parts: string[], range: vscode.Range } | null {
    const line = document.lineAt(position.line).text;
    const offset = position.character;

    // Find the start of the identifier/qualified name
    let start = offset;
    while (start > 0 && /[a-zA-Z0-9_'.]/.test(line[start - 1])) {
        start--;
    }

    // Find the end of the identifier/qualified name
    let end = offset;
    while (end < line.length && /[a-zA-Z0-9_']/.test(line[end])) {
        end++;
    }

    if (start === end) return null;

    const text = line.substring(start, end);

    // Handle case where cursor is on a dot
    if (text.endsWith('.')) return null;

    // Split by dots and validate each part is a valid identifier
    const parts = text.split('.');
    for (const part of parts) {
        if (!/^[a-zA-Z_][a-zA-Z0-9_']*$/.test(part)) return null;
    }

    const range = new vscode.Range(
        new vscode.Position(position.line, start),
        new vscode.Position(position.line, end)
    );

    return { parts, range };
}

/**
 * VS Code Definition Provider for Ur/Web.
 */
export class UrWebDefinitionProvider implements vscode.DefinitionProvider {
    public provideDefinition(
        document: vscode.TextDocument,
        position: vscode.Position,
        _token: vscode.CancellationToken
    ): vscode.ProviderResult<vscode.Definition> {
        try {
            // Get the qualified name at the cursor position
            const qualifiedName = getQualifiedNameAtPosition(document, position);
            if (!qualifiedName) return null;

            // Tokenize and build scope tree
            const text = document.getText();
            const tokens = tokenize(text);
            const builder = new ScopeBuilder(document, tokens);
            const rootScope = builder.build();

            // Find the scope at the cursor position
            const cursorOffset = document.offsetAt(position);
            const resolver = new DefinitionResolver(rootScope, document);
            const scope = resolver.findScopeAt(cursorOffset);

            // Resolve the name
            let definition: Definition | null;
            if (qualifiedName.parts.length === 1) {
                definition = resolver.resolveSimpleName(qualifiedName.parts[0], scope, cursorOffset);
            } else {
                definition = resolver.resolveQualifiedName(qualifiedName.parts, scope, cursorOffset);
            }

            if (!definition) return null;

            // Return the location
            return new vscode.Location(document.uri, definition.selectionRange);
        } catch (e) {
            console.error('UrWeb definition provider error:', e);
            return null;
        }
    }
}
