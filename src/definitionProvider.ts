import * as vscode from 'vscode';

/**
 * Token types for the lexer (extended from symbolProvider with 'dot' and 'open').
 */
interface Token {
    type: 'keyword' | 'ident' | 'struct' | 'sig' | 'end' | 'let' | 'in' | 'lparen' | 'rparen' | 'lbracket' | 'rbracket' | 'lbrace' | 'rbrace' | 'colon' | 'equals' | 'dot' | 'open' | 'and' | 'fn' | 'arrow' | 'larrow' | 'semicolon' | 'comma' | 'pipe' | 'case' | 'of' | 'other';
    value: string;
    offset: number;
    line: number;
}

/**
 * Kinds of definitions we track.
 */
type DefinitionKind = 'val' | 'fun' | 'type' | 'datatype' | 'con' | 'structure' | 'signature' | 'functor' | 'table' | 'view' | 'sequence' | 'cookie' | 'style' | 'task' | 'class' | 'policy' | 'functor-param' | 'param';

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
    kind: 'file' | 'structure' | 'signature' | 'functor' | 'let' | 'functor-param' | 'function-body';
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
 * Tokenizer state passed between tokenization functions.
 */
interface TokenizerState {
    text: string;
    index: number;
    line: number;
    tokens: Token[];
}

/**
 * Skip a comment block (* ... *) with nesting support.
 */
function skipComment(state: TokenizerState): void {
    state.index += 2; // skip '(*'
    let depth = 1;
    while (state.index < state.text.length && depth > 0) {
        if (state.text[state.index] === '\n') state.line++;
        if (state.text[state.index] === '(' && state.text[state.index + 1] === '*') {
            depth++;
            state.index += 2;
        } else if (state.text[state.index] === '*' && state.text[state.index + 1] === ')') {
            depth--;
            state.index += 2;
        } else {
            state.index++;
        }
    }
}

/**
 * Skip a string literal "...".
 */
function skipString(state: TokenizerState): void {
    state.index++; // skip opening quote
    while (state.index < state.text.length) {
        if (state.text[state.index] === '\n') state.line++;
        if (state.text[state.index] === '\\' && state.index + 1 < state.text.length) {
            if (state.text[state.index + 1] === '\n') state.line++;
            state.index += 2;
        } else if (state.text[state.index] === '"') {
            state.index++; // skip closing quote
            break;
        } else {
            state.index++;
        }
    }
}

/**
 * Tokenize an identifier or keyword.
 */
function tokenizeIdentifier(state: TokenizerState): void {
    const start = state.index;
    while (state.index < state.text.length && /[a-zA-Z0-9_']/.test(state.text[state.index])) {
        state.index++;
    }
    const value = state.text.substring(start, state.index);
    let type: Token['type'] = 'ident';

    if (value === 'struct') type = 'struct';
    else if (value === 'sig') type = 'sig';
    else if (value === 'end') type = 'end';
    else if (value === 'let') type = 'let';
    else if (value === 'in') type = 'in';
    else if (value === 'open') type = 'open';
    else if (value === 'and') type = 'and';
    else if (value === 'fn') type = 'fn';
    else if (value === 'case') type = 'case';
    else if (value === 'of') type = 'of';
    else if (DECLARATION_KINDS.has(value)) type = 'keyword';

    state.tokens.push({ type, value, offset: start, line: state.line });
}

/**
 * Process XML content, tokenizing embedded Ur/Web code in {...}.
 * Called after '<xml>' has been consumed.
 */
function processXmlContent(state: TokenizerState): void {
    let xmlDepth = 1;

    while (state.index < state.text.length && xmlDepth > 0) {
        // Track line numbers
        if (state.text[state.index] === '\n') {
            state.line++;
            state.index++;
            continue;
        }

        // Check for nested <xml>
        if (state.text.substring(state.index, state.index + 5) === '<xml>') {
            xmlDepth++;
            state.index += 5;
            continue;
        }

        // Check for </xml>
        if (state.text.substring(state.index, state.index + 6) === '</xml>') {
            xmlDepth--;
            state.index += 6;
            continue;
        }

        // Check for embedded Ur/Web code in {...}
        if (state.text[state.index] === '{') {
            state.index++; // skip '{'
            tokenizeUrWebInBraces(state);
            continue;
        }

        // Skip other XML content
        state.index++;
    }
}

/**
 * Tokenize Ur/Web code inside braces {...} within XML.
 * Handles nested braces and nested XML blocks.
 * Called after '{' has been consumed.
 */
function tokenizeUrWebInBraces(state: TokenizerState): void {
    let braceDepth = 1;

    while (state.index < state.text.length && braceDepth > 0) {
        const text = state.text;
        const i = state.index;

        // Track line numbers
        if (text[i] === '\n') {
            state.line++;
            state.index++;
            continue;
        }

        // Skip whitespace
        if (/\s/.test(text[i])) {
            state.index++;
            continue;
        }

        // Handle closing brace
        if (text[i] === '}') {
            braceDepth--;
            if (braceDepth === 0) {
                state.index++;
                return;
            }
            // Emit token for nested record pattern
            state.tokens.push({ type: 'rbrace', value: '}', offset: i, line: state.line });
            state.index++;
            continue;
        }

        // Handle opening brace (for record literals, etc.)
        if (text[i] === '{') {
            state.tokens.push({ type: 'lbrace', value: '{', offset: i, line: state.line });
            braceDepth++;
            state.index++;
            continue;
        }

        // Skip comments
        if (text[i] === '(' && text[i + 1] === '*') {
            skipComment(state);
            continue;
        }

        // Skip strings
        if (text[i] === '"') {
            skipString(state);
            continue;
        }

        // Handle nested XML
        if (text.substring(i, i + 5) === '<xml>') {
            state.index += 5;
            processXmlContent(state);
            continue;
        }

        // Identifiers and keywords
        if (/[a-zA-Z_]/.test(text[i])) {
            tokenizeIdentifier(state);
            continue;
        }

        // Structural characters
        if (text[i] === '(') {
            state.tokens.push({ type: 'lparen', value: '(', offset: i, line: state.line });
            state.index++;
            continue;
        }
        if (text[i] === ')') {
            state.tokens.push({ type: 'rparen', value: ')', offset: i, line: state.line });
            state.index++;
            continue;
        }
        if (text[i] === '[') {
            state.tokens.push({ type: 'lbracket', value: '[', offset: i, line: state.line });
            state.index++;
            continue;
        }
        if (text[i] === ']') {
            state.tokens.push({ type: 'rbracket', value: ']', offset: i, line: state.line });
            state.index++;
            continue;
        }
        if (text[i] === ':') {
            state.tokens.push({ type: 'colon', value: ':', offset: i, line: state.line });
            state.index++;
            continue;
        }
        if (text[i] === '=' && text[i + 1] === '>') {
            state.tokens.push({ type: 'arrow', value: '=>', offset: i, line: state.line });
            state.index += 2;
            continue;
        }
        if (text[i] === '=') {
            state.tokens.push({ type: 'equals', value: '=', offset: i, line: state.line });
            state.index++;
            continue;
        }
        if (text[i] === '.') {
            state.tokens.push({ type: 'dot', value: '.', offset: i, line: state.line });
            state.index++;
            continue;
        }
        if (text[i] === '<' && text[i + 1] === '-') {
            state.tokens.push({ type: 'larrow', value: '<-', offset: i, line: state.line });
            state.index += 2;
            continue;
        }
        if (text[i] === ';') {
            state.tokens.push({ type: 'semicolon', value: ';', offset: i, line: state.line });
            state.index++;
            continue;
        }
        if (text[i] === ',') {
            state.tokens.push({ type: 'comma', value: ',', offset: i, line: state.line });
            state.index++;
            continue;
        }
        if (text[i] === '|') {
            state.tokens.push({ type: 'pipe', value: '|', offset: i, line: state.line });
            state.index++;
            continue;
        }

        // Skip other characters
        state.index++;
    }
}

/**
 * Tokenizer for Ur/Web code.
 * Handles comments, string literals, and XML blocks with embedded Ur/Web.
 */
function tokenize(text: string): Token[] {
    const state: TokenizerState = {
        text,
        index: 0,
        line: 0,
        tokens: []
    };

    while (state.index < text.length) {
        const i = state.index;

        // Track line numbers
        if (text[i] === '\n') {
            state.line++;
            state.index++;
            continue;
        }

        // Skip whitespace
        if (/\s/.test(text[i])) {
            state.index++;
            continue;
        }

        // Skip comments (* ... *)
        if (text[i] === '(' && text[i + 1] === '*') {
            skipComment(state);
            continue;
        }

        // Skip string literals "..."
        if (text[i] === '"') {
            skipString(state);
            continue;
        }

        // Handle XML blocks <xml>...</xml> with embedded Ur/Web in {...}
        if (text.substring(i, i + 5) === '<xml>') {
            state.index += 5; // skip '<xml>'
            processXmlContent(state);
            continue;
        }

        // Identifiers and keywords
        if (/[a-zA-Z_]/.test(text[i])) {
            tokenizeIdentifier(state);
            continue;
        }

        // Structural characters
        if (text[i] === '(') {
            state.tokens.push({ type: 'lparen', value: '(', offset: i, line: state.line });
            state.index++;
            continue;
        }
        if (text[i] === ')') {
            state.tokens.push({ type: 'rparen', value: ')', offset: i, line: state.line });
            state.index++;
            continue;
        }
        if (text[i] === '[') {
            state.tokens.push({ type: 'lbracket', value: '[', offset: i, line: state.line });
            state.index++;
            continue;
        }
        if (text[i] === ']') {
            state.tokens.push({ type: 'rbracket', value: ']', offset: i, line: state.line });
            state.index++;
            continue;
        }
        if (text[i] === ':') {
            state.tokens.push({ type: 'colon', value: ':', offset: i, line: state.line });
            state.index++;
            continue;
        }
        if (text[i] === '=' && text[i + 1] === '>') {
            state.tokens.push({ type: 'arrow', value: '=>', offset: i, line: state.line });
            state.index += 2;
            continue;
        }
        if (text[i] === '=') {
            state.tokens.push({ type: 'equals', value: '=', offset: i, line: state.line });
            state.index++;
            continue;
        }
        if (text[i] === '.') {
            state.tokens.push({ type: 'dot', value: '.', offset: i, line: state.line });
            state.index++;
            continue;
        }
        if (text[i] === '<' && text[i + 1] === '-') {
            state.tokens.push({ type: 'larrow', value: '<-', offset: i, line: state.line });
            state.index += 2;
            continue;
        }
        if (text[i] === ';') {
            state.tokens.push({ type: 'semicolon', value: ';', offset: i, line: state.line });
            state.index++;
            continue;
        }
        if (text[i] === '{') {
            state.tokens.push({ type: 'lbrace', value: '{', offset: i, line: state.line });
            state.index++;
            continue;
        }
        if (text[i] === '}') {
            state.tokens.push({ type: 'rbrace', value: '}', offset: i, line: state.line });
            state.index++;
            continue;
        }
        if (text[i] === ',') {
            state.tokens.push({ type: 'comma', value: ',', offset: i, line: state.line });
            state.index++;
            continue;
        }
        if (text[i] === '|') {
            state.tokens.push({ type: 'pipe', value: '|', offset: i, line: state.line });
            state.index++;
            continue;
        }

        // Skip other characters
        state.index++;
    }

    return state.tokens;
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

        // Handle 'val rec' - recursive val, binding IS visible in RHS
        let isRecursiveVal = false;
        if (keyword === 'val' && this.peek()?.type === 'ident' && this.peek()?.value === 'rec') {
            this.advance(); // skip 'rec'
            isRecursiveVal = true;
        }

        // For val declarations, handle patterns
        if (keyword === 'val') {
            // Parse the pattern to get all bindings
            const patternBindings = this.parsePattern();

            // Skip type annotation if present
            // In .urs signature files, there's no '=' - just 'val name : type'
            // So we also break on 'keyword' (next declaration) or 'end'
            if (this.peek()?.type === 'colon') {
                this.advance();
                while (this.index < this.tokens.length) {
                    const next = this.peek();
                    if (next?.type === 'equals' || next?.type === 'keyword' || next?.type === 'end') break;
                    this.advance();
                }
            }

            // Check if there's an '=' (implementation) or not (signature)
            const hasEquals = this.peek()?.type === 'equals';
            if (hasEquals) {
                this.advance(); // skip '='
            }

            if (!hasEquals) {
                // Signature file: no RHS to parse, just add the bindings
                for (const binding of patternBindings) {
                    const def: Definition = {
                        name: binding.name,
                        kind: 'val',
                        range: new vscode.Range(
                            this.document.positionAt(binding.offset),
                            this.document.positionAt(binding.offset + binding.name.length)
                        ),
                        selectionRange: new vscode.Range(
                            this.document.positionAt(binding.offset),
                            this.document.positionAt(binding.offset + binding.name.length)
                        ),
                        offset: binding.offset,
                    };
                    this.addDefinition(scope, def);
                }
            } else if (isRecursiveVal) {
                // val rec - bindings ARE visible in RHS (for recursion)
                for (const binding of patternBindings) {
                    const def: Definition = {
                        name: binding.name,
                        kind: 'val',
                        range: new vscode.Range(
                            this.document.positionAt(binding.offset),
                            this.document.positionAt(binding.offset + binding.name.length)
                        ),
                        selectionRange: new vscode.Range(
                            this.document.positionAt(binding.offset),
                            this.document.positionAt(binding.offset + binding.name.length)
                        ),
                        offset: binding.offset,
                    };
                    this.addDefinition(scope, def);
                }
                this.parseValDeclaration(scope);
            } else {
                // Plain val - bindings NOT visible in RHS
                const exprEndOffset = this.parseValDeclaration(scope);
                for (const binding of patternBindings) {
                    const def: Definition = {
                        name: binding.name,
                        kind: 'val',
                        range: new vscode.Range(
                            this.document.positionAt(binding.offset),
                            this.document.positionAt(binding.offset + binding.name.length)
                        ),
                        selectionRange: new vscode.Range(
                            this.document.positionAt(binding.offset),
                            this.document.positionAt(binding.offset + binding.name.length)
                        ),
                        offset: exprEndOffset,
                    };
                    this.addDefinition(scope, def);
                }
            }
            return;
        }

        // Get the name for non-val declarations
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
        } else if (keyword === 'fun') {
            // Handle function declaration with parameters
            // fun is always recursive - binding visible in body
            this.addDefinition(scope, def);
            this.parseFunDeclaration(scope, kind);
        } else {
            this.addDefinition(scope, def);
            // Handle 'and' for mutual recursion (datatype, type, etc.)
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

    /**
     * Parse a fun declaration's parameters and body.
     */
    private parseFunDeclaration(scope: Scope, kind: DefinitionKind): void {
        const result = this.parseFunctionParameters();
        if (!result) return;

        const { params, bodyStartOffset } = result;

        if (params.length > 0) {
            // Create function-body scope
            const bodyScope = this.createScope(scope, bodyStartOffset, scope.endOffset, 'function-body');

            // Add parameters to function body scope
            for (const param of params) {
                const paramDef: Definition = {
                    name: param.name,
                    kind: 'param',
                    range: new vscode.Range(
                        this.document.positionAt(param.offset),
                        this.document.positionAt(param.offset + param.name.length)
                    ),
                    selectionRange: new vscode.Range(
                        this.document.positionAt(param.offset),
                        this.document.positionAt(param.offset + param.name.length)
                    ),
                    offset: param.offset,
                };
                this.addDefinition(bodyScope, paramDef);
            }

            // Parse the function body
            this.parseFunctionBody(bodyScope);
        }

        // Handle 'and' for mutually recursive functions
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

                // Parse parameters for this 'and' function too
                this.parseFunDeclaration(scope, kind);
            }
        }
    }

    /**
     * Parse a val declaration, looking for lambda expressions.
     * Returns the offset where the expression ends (for non-recursive val scoping).
     */
    private parseValDeclaration(scope: Scope): number {
        let parenDepth = 0;
        let bracketDepth = 0;
        let braceDepth = 0;

        while (this.index < this.tokens.length) {
            const t = this.peek();
            if (!t) break;

            // Track parentheses
            if (t.type === 'lparen') {
                parenDepth++;
                this.advance();
                continue;
            }
            if (t.type === 'rparen') {
                if (parenDepth === 0) {
                    // This closes something from before - end of expression
                    return t.offset;
                }
                parenDepth--;
                this.advance();
                continue;
            }

            // Track brackets
            if (t.type === 'lbracket') {
                bracketDepth++;
                this.advance();
                continue;
            }
            if (t.type === 'rbracket') {
                if (bracketDepth === 0) {
                    return t.offset;
                }
                bracketDepth--;
                this.advance();
                continue;
            }

            // Track braces (for record literals)
            if (t.type === 'lbrace') {
                braceDepth++;
                this.advance();
                continue;
            }
            if (t.type === 'rbrace') {
                if (braceDepth === 0) {
                    return t.offset;
                }
                braceDepth--;
                this.advance();
                continue;
            }

            // Found a lambda - parse it and continue
            if (t.type === 'fn') {
                this.parseLambda(scope);
                continue;
            }

            // Handle let blocks
            if (t.type === 'let') {
                const letStart = t.offset;
                this.advance();
                const letScope = this.createScope(scope, letStart, scope.endOffset, 'let');
                this.parseLetBody(letScope);
                letScope.endOffset = this.peek()?.offset ?? scope.endOffset;
                continue;
            }

            // Handle case statements
            if (t.type === 'case') {
                this.parseCaseStatement(scope);
                continue;
            }

            // Hit a declaration boundary - stop
            if (t.type === 'keyword' || t.type === 'and' || t.type === 'end' || t.type === 'in') {
                return t.offset;
            }

            this.advance();
        }

        // End of file
        return this.tokens.length > 0
            ? this.tokens[this.tokens.length - 1].offset + 1
            : 0;
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

    /**
     * Skip tokens until matching ')' is found.
     */
    private skipToMatchingParen(): void {
        let depth = 1;
        while (this.index < this.tokens.length && depth > 0) {
            const t = this.peek();
            if (t?.type === 'lparen') depth++;
            else if (t?.type === 'rparen') depth--;
            this.advance();
        }
    }

    /**
     * Skip tokens until matching ']' is found.
     */
    private skipToMatchingBracket(): void {
        let depth = 1;
        while (this.index < this.tokens.length && depth > 0) {
            const t = this.peek();
            if (t?.type === 'lbracket') depth++;
            else if (t?.type === 'rbracket') depth--;
            this.advance();
        }
    }

    /**
     * Check if an identifier is a constructor (starts with uppercase).
     */
    private isConstructor(name: string): boolean {
        return name.length > 0 && name[0] >= 'A' && name[0] <= 'Z';
    }

    /**
     * Parse a pattern and return the bindings it introduces.
     * Patterns can be:
     * - Simple identifier (binding if lowercase, constructor if uppercase)
     * - Wildcard: _
     * - Tuple: (p1, p2, ...)
     * - Record: {field1 = p1, field2 = p2, ...}
     * - Constructor with argument: Con p
     */
    private parsePattern(): Array<{ name: string, offset: number }> {
        const bindings: Array<{ name: string, offset: number }> = [];
        const t = this.peek();
        if (!t) return bindings;

        // Tuple pattern: (p1, p2, ...)
        if (t.type === 'lparen') {
            this.advance(); // skip '('
            while (this.index < this.tokens.length) {
                const next = this.peek();
                if (!next) break;
                if (next.type === 'rparen') {
                    this.advance(); // skip ')'
                    break;
                }
                // Skip comma if present
                if (next.type === 'comma') {
                    this.advance();
                    continue;
                }
                // Only parse pattern if we have something that starts a pattern
                if (next.type === 'ident' || next.type === 'lparen' || next.type === 'lbrace') {
                    const beforeIndex = this.index;
                    bindings.push(...this.parsePattern());
                    // Safety: if parsePattern didn't advance, skip this token to avoid infinite loop
                    if (this.index === beforeIndex) {
                        this.advance();
                    }
                } else {
                    // Unknown token in tuple - skip it to avoid infinite loop
                    this.advance();
                }
            }
            return bindings;
        }

        // Record pattern: {field1 = p1, field2 = p2, ...}
        if (t.type === 'lbrace') {
            this.advance(); // skip '{'
            while (this.index < this.tokens.length) {
                const next = this.peek();
                if (!next) break;
                if (next.type === 'rbrace') {
                    this.advance(); // skip '}'
                    break;
                }
                // Skip comma if present
                if (next.type === 'comma') {
                    this.advance();
                    continue;
                }
                // Handle ... for record wildcard
                if (next.type === 'dot') {
                    // Skip ... (three dots)
                    while (this.peek()?.type === 'dot') {
                        this.advance();
                    }
                    continue;
                }
                // Field pattern: field = pattern
                if (next.type === 'ident') {
                    this.advance(); // skip field name
                    if (this.peek()?.type === 'equals') {
                        this.advance(); // skip '='
                        const beforeIndex = this.index;
                        bindings.push(...this.parsePattern());
                        // Safety: if parsePattern didn't advance, skip to avoid infinite loop
                        if (this.index === beforeIndex) {
                            this.advance();
                        }
                    }
                    continue;
                }
                // Unknown token in record pattern - skip it to avoid infinite loop
                this.advance();
            }
            return bindings;
        }

        // Identifier: could be variable binding or constructor
        if (t.type === 'ident') {
            const name = t.value;
            const offset = t.offset;
            this.advance();

            if (name === '_') {
                // Wildcard - no binding
                return bindings;
            }

            if (this.isConstructor(name)) {
                // Constructor - parse argument pattern if present
                const next = this.peek();
                if (next && (next.type === 'ident' || next.type === 'lparen' || next.type === 'lbrace')) {
                    bindings.push(...this.parsePattern());
                }
            } else {
                // Variable binding
                bindings.push({ name, offset });
            }
            return bindings;
        }

        return bindings;
    }

    /**
     * Parse function parameters between function name and '='.
     * Returns the list of parameters and advances past the '='.
     * Handles patterns including tuples, records, and constructors.
     */
    private parseFunctionParameters(): { params: Array<{ name: string, offset: number }>, bodyStartOffset: number } | null {
        const params: Array<{ name: string, offset: number }> = [];

        while (this.index < this.tokens.length) {
            const t = this.peek();
            if (!t) break;

            // Found the '=' - function body starts after this
            if (t.type === 'equals') {
                const bodyStart = t.offset + 1;
                this.advance(); // skip '='
                return { params, bodyStartOffset: bodyStart };
            }

            // Simple identifier parameter (could be pattern or constructor)
            if (t.type === 'ident') {
                if (t.value === '_') {
                    // Wildcard - no binding
                    this.advance();
                    continue;
                }
                if (this.isConstructor(t.value)) {
                    // Constructor pattern - parse with parsePattern
                    params.push(...this.parsePattern());
                } else {
                    // Simple variable
                    params.push({ name: t.value, offset: t.offset });
                    this.advance();
                }
                continue;
            }

            // Type-level parameter or constraint: [a ::: Type] or [[key] ~ b]
            // Skip these - they're not value parameters
            if (t.type === 'lbracket') {
                this.advance(); // skip '['
                this.skipToMatchingBracket();
                continue;
            }

            // Parenthesized: could be typed parameter (x : type) or tuple pattern (a, b)
            if (t.type === 'lparen') {
                // Look ahead to determine if it's a typed parameter or a pattern
                // Typed parameter has form: (ident : ...) or (pattern : ...)
                const savedIndex = this.index;
                this.advance(); // skip '('

                // Try to detect if this is (pattern : type) vs (pattern, pattern, ...)
                // by looking for a colon that's not inside nested parens
                let parenDepth = 0;
                let hasColonAtTopLevel = false;
                let scanIndex = this.index;
                while (scanIndex < this.tokens.length) {
                    const scanToken = this.tokens[scanIndex];
                    if (scanToken.type === 'lparen') parenDepth++;
                    else if (scanToken.type === 'rparen') {
                        if (parenDepth === 0) break;
                        parenDepth--;
                    }
                    else if (scanToken.type === 'colon' && parenDepth === 0) {
                        hasColonAtTopLevel = true;
                        break;
                    }
                    else if (scanToken.type === 'comma' && parenDepth === 0) {
                        // It's a tuple pattern
                        break;
                    }
                    scanIndex++;
                }

                // Restore position and parse appropriately
                this.index = savedIndex;

                if (hasColonAtTopLevel) {
                    // Typed parameter: (pattern : type)
                    this.advance(); // skip '('
                    params.push(...this.parsePattern());
                    // Skip to closing ')' (past the type annotation)
                    this.skipToMatchingParen();
                } else {
                    // Tuple pattern or just parenthesized pattern
                    params.push(...this.parsePattern());
                }
                continue;
            }

            // Record pattern: {field = pattern, ...}
            if (t.type === 'lbrace') {
                params.push(...this.parsePattern());
                continue;
            }

            // Colon indicates return type annotation - skip until '='
            if (t.type === 'colon') {
                this.advance();
                // Skip type annotation until we hit '='
                while (this.index < this.tokens.length) {
                    const next = this.peek();
                    if (next?.type === 'equals') break;
                    this.advance();
                }
                continue;
            }

            // Unexpected token - stop parsing parameters
            break;
        }

        return null;
    }

    /**
     * Parse a function body, looking for nested constructs and stopping at declaration boundaries.
     */
    private parseFunctionBody(bodyScope: Scope): void {
        let structSigDepth = 0; // Track nested struct/sig blocks
        let parenDepth = 0;     // Track parentheses - stop if we'd go negative
        let bracketDepth = 0;   // Track brackets - stop if we'd go negative

        while (this.index < this.tokens.length) {
            const t = this.peek();
            if (!t) break;

            // Track parentheses - if we see ')' at depth 0, it closes something
            // from before this function body started, so we should stop
            if (t.type === 'lparen') {
                parenDepth++;
                this.advance();
                continue;
            }
            if (t.type === 'rparen') {
                if (parenDepth === 0) {
                    // This ')' closes something from before the function body
                    bodyScope.endOffset = t.offset;
                    return;
                }
                parenDepth--;
                this.advance();
                continue;
            }

            // Same for brackets
            if (t.type === 'lbracket') {
                bracketDepth++;
                this.advance();
                continue;
            }
            if (t.type === 'rbracket') {
                if (bracketDepth === 0) {
                    // This ']' closes something from before the function body
                    bodyScope.endOffset = t.offset;
                    return;
                }
                bracketDepth--;
                this.advance();
                continue;
            }

            // Handle let blocks - create a proper scope and parse contents
            if (t.type === 'let') {
                const letStart = t.offset;
                this.advance(); // skip 'let'
                const letScope = this.createScope(bodyScope, letStart, bodyScope.endOffset, 'let');
                this.parseLetBody(letScope);
                letScope.endOffset = this.peek()?.offset ?? bodyScope.endOffset;
                continue;
            }

            // Track nesting for struct/sig blocks
            if (t.type === 'struct' || t.type === 'sig') {
                structSigDepth++;
                this.advance();
                continue;
            }

            if (t.type === 'end') {
                if (structSigDepth > 0) {
                    structSigDepth--;
                    this.advance();
                    continue;
                }
                // End at depth 0 means we're done
                bodyScope.endOffset = t.offset;
                return;
            }

            // Handle nested lambda: fn x => body
            if (t.type === 'fn') {
                this.parseLambda(bodyScope);
                continue;
            }

            // At depth 0, these signal the end of function body
            // 'in' means we're inside a let block and hit the expression part
            if (structSigDepth === 0) {
                if (t.type === 'keyword' || t.type === 'and' || t.type === 'in') {
                    bodyScope.endOffset = t.offset;
                    return;
                }
            }

            // Handle monadic binding: pattern <- expr ;
            // The binding is only visible after the semicolon
            if (this.isMonadicBinding()) {
                const bindings = this.parsePattern();
                if (this.peek()?.type === 'larrow') {
                    this.advance(); // skip <-
                }

                // Parse the expression until ';', handling nested constructs
                const semicolonOffset = this.skipToSemicolonWithBindings(bodyScope);
                if (semicolonOffset !== null) {
                    // Add bindings with offset after the semicolon
                    for (const binding of bindings) {
                        const bindingDef: Definition = {
                            name: binding.name,
                            kind: 'val',
                            range: new vscode.Range(
                                this.document.positionAt(binding.offset),
                                this.document.positionAt(binding.offset + binding.name.length)
                            ),
                            selectionRange: new vscode.Range(
                                this.document.positionAt(binding.offset),
                                this.document.positionAt(binding.offset + binding.name.length)
                            ),
                            offset: semicolonOffset + 1,
                        };
                        this.addDefinition(bodyScope, bindingDef);
                    }
                }
                continue;
            }

            // Handle case statements: case expr of pattern => expr | ...
            if (t.type === 'case') {
                this.parseCaseStatement(bodyScope);
                continue;
            }

            this.advance();
        }

        // End of file
        bodyScope.endOffset = this.tokens[this.tokens.length - 1]?.offset ?? bodyScope.startOffset;
    }

    /**
     * Check if current position starts a monadic binding (pattern <- expr).
     * We need to look ahead to find <- after the pattern.
     */
    private isMonadicBinding(): boolean {
        const t = this.peek();
        if (!t) return false;

        // Must start with something that can begin a pattern
        if (t.type !== 'ident' && t.type !== 'lparen' && t.type !== 'lbrace') {
            return false;
        }

        // Scan forward to find <- without going past declaration boundaries
        let scanIndex = this.index;
        let parenDepth = 0;
        let braceDepth = 0;

        while (scanIndex < this.tokens.length) {
            const scanToken = this.tokens[scanIndex];

            if (scanToken.type === 'larrow' && parenDepth === 0 && braceDepth === 0) {
                return true;
            }

            // Track nesting
            if (scanToken.type === 'lparen') parenDepth++;
            else if (scanToken.type === 'rparen') parenDepth--;
            else if (scanToken.type === 'lbrace') braceDepth++;
            else if (scanToken.type === 'rbrace') braceDepth--;

            // Stop at boundaries
            if (scanToken.type === 'semicolon' || scanToken.type === 'keyword' ||
                scanToken.type === 'and' || scanToken.type === 'in' ||
                scanToken.type === 'end' || scanToken.type === 'equals' ||
                scanToken.type === 'arrow') {
                return false;
            }

            // Gone too deep
            if (parenDepth < 0 || braceDepth < 0) {
                return false;
            }

            scanIndex++;
        }

        return false;
    }

    /**
     * Parse a case statement: case expr of pattern => expr | pattern => expr ...
     */
    private parseCaseStatement(parentScope: Scope): void {
        this.advance(); // skip 'case'

        // Parse the scrutinee expression until 'of', handling nested constructs
        this.parseExpressionUntil(parentScope, 'of');
        if (this.peek()?.type === 'of') {
            this.advance(); // skip 'of'
        }

        // Parse the branches
        this.parseCaseBranches(parentScope);
    }

    /**
     * Parse an expression until a specific token type, handling nested constructs.
     * Used for case scrutinees, monadic binding RHS, etc.
     */
    private parseExpressionUntil(parentScope: Scope, stopToken: Token['type']): void {
        let parenDepth = 0;
        let bracketDepth = 0;
        let braceDepth = 0;

        while (this.index < this.tokens.length) {
            const t = this.peek();
            if (!t) break;

            // Check for stop token at depth 0
            if (t.type === stopToken && parenDepth === 0 && bracketDepth === 0 && braceDepth === 0) {
                return;
            }

            // Track nesting
            if (t.type === 'lparen') {
                parenDepth++;
                this.advance();
                continue;
            }
            if (t.type === 'rparen') {
                if (parenDepth === 0) return;
                parenDepth--;
                this.advance();
                continue;
            }
            if (t.type === 'lbracket') {
                bracketDepth++;
                this.advance();
                continue;
            }
            if (t.type === 'rbracket') {
                if (bracketDepth === 0) return;
                bracketDepth--;
                this.advance();
                continue;
            }
            if (t.type === 'lbrace') {
                braceDepth++;
                this.advance();
                continue;
            }
            if (t.type === 'rbrace') {
                if (braceDepth === 0) return;
                braceDepth--;
                this.advance();
                continue;
            }

            // Handle nested constructs that introduce bindings
            if (t.type === 'fn') {
                this.parseLambda(parentScope);
                continue;
            }
            if (t.type === 'case') {
                this.parseCaseStatement(parentScope);
                continue;
            }
            if (t.type === 'let') {
                const letStart = t.offset;
                this.advance();
                const letScope = this.createScope(parentScope, letStart, parentScope.endOffset, 'let');
                this.parseLetBody(letScope);
                letScope.endOffset = this.peek()?.offset ?? parentScope.endOffset;
                continue;
            }

            // Stop at declaration boundaries
            if (t.type === 'keyword' || t.type === 'and' || t.type === 'in' || t.type === 'end') {
                return;
            }

            this.advance();
        }
    }

    /**
     * Parse case branches: pattern => expr | pattern => expr ...
     */
    private parseCaseBranches(parentScope: Scope): void {
        while (this.index < this.tokens.length) {
            const t = this.peek();
            if (!t) break;

            // Check for end of case (these would end the case expression)
            if (t.type === 'keyword' || t.type === 'and' || t.type === 'in' ||
                t.type === 'end' || t.type === 'rparen' || t.type === 'rbracket' ||
                t.type === 'rbrace' || t.type === 'semicolon' || t.type === 'comma') {
                return;
            }

            // Skip leading | if present
            if (t.type === 'pipe') {
                this.advance();
                continue;
            }

            // Parse the pattern
            const beforePattern = this.index;
            const patternBindings = this.parsePattern();

            // Expect =>
            if (this.peek()?.type === 'arrow') {
                this.advance(); // skip '=>'
            } else if (this.index === beforePattern) {
                // Safety: parsePattern didn't advance and no =>, skip token to avoid infinite loop
                this.advance();
                continue;
            }

            // Create a scope for this branch's pattern bindings
            const branchStart = this.peek()?.offset ?? parentScope.endOffset;
            const branchScope = this.createScope(parentScope, branchStart, parentScope.endOffset, 'function-body');

            // Add pattern bindings to the branch scope
            for (const binding of patternBindings) {
                const bindingDef: Definition = {
                    name: binding.name,
                    kind: 'param',
                    range: new vscode.Range(
                        this.document.positionAt(binding.offset),
                        this.document.positionAt(binding.offset + binding.name.length)
                    ),
                    selectionRange: new vscode.Range(
                        this.document.positionAt(binding.offset),
                        this.document.positionAt(binding.offset + binding.name.length)
                    ),
                    offset: binding.offset,
                };
                this.addDefinition(branchScope, bindingDef);
            }

            // Parse the branch expression
            this.parseBranchExpression(branchScope);

            // Update branch scope end offset
            branchScope.endOffset = this.peek()?.offset ?? parentScope.endOffset;
        }
    }

    /**
     * Parse a case branch expression until we hit | or end of case.
     */
    private parseBranchExpression(branchScope: Scope): void {
        let parenDepth = 0;
        let bracketDepth = 0;
        let braceDepth = 0;

        while (this.index < this.tokens.length) {
            const t = this.peek();
            if (!t) break;

            // Track nesting
            if (t.type === 'lparen') {
                parenDepth++;
                this.advance();
                continue;
            }
            if (t.type === 'rparen') {
                if (parenDepth === 0) return; // End of case in parens
                parenDepth--;
                this.advance();
                continue;
            }
            if (t.type === 'lbracket') {
                bracketDepth++;
                this.advance();
                continue;
            }
            if (t.type === 'rbracket') {
                if (bracketDepth === 0) return;
                bracketDepth--;
                this.advance();
                continue;
            }
            if (t.type === 'lbrace') {
                braceDepth++;
                this.advance();
                continue;
            }
            if (t.type === 'rbrace') {
                if (braceDepth === 0) return;
                braceDepth--;
                this.advance();
                continue;
            }

            // At depth 0, these tokens end the branch
            if (parenDepth === 0 && bracketDepth === 0 && braceDepth === 0) {
                // | starts the next branch
                if (t.type === 'pipe') {
                    return;
                }
                // These end the case expression entirely
                if (t.type === 'keyword' || t.type === 'and' || t.type === 'in' ||
                    t.type === 'end' || t.type === 'semicolon' || t.type === 'comma') {
                    return;
                }
            }

            // Handle nested constructs
            if (t.type === 'let') {
                const letStart = t.offset;
                this.advance();
                const letScope = this.createScope(branchScope, letStart, branchScope.endOffset, 'let');
                this.parseLetBody(letScope);
                letScope.endOffset = this.peek()?.offset ?? branchScope.endOffset;
                continue;
            }

            if (t.type === 'fn') {
                this.parseLambda(branchScope);
                continue;
            }

            if (t.type === 'case') {
                this.parseCaseStatement(branchScope);
                continue;
            }

            this.advance();
        }
    }

    /**
     * Skip tokens until we find a semicolon at the right nesting level.
     * Parses nested constructs (fn, case, let) to capture their bindings.
     * Returns the offset of the semicolon, or null if not found.
     */
    private skipToSemicolonWithBindings(parentScope: Scope): number | null {
        let parenDepth = 0;
        let bracketDepth = 0;
        let braceDepth = 0;
        let structSigDepth = 0;

        while (this.index < this.tokens.length) {
            const t = this.peek();
            if (!t) break;

            // Track nesting
            if (t.type === 'lparen') {
                parenDepth++;
                this.advance();
                continue;
            }
            if (t.type === 'rparen') {
                if (parenDepth === 0) return null;
                parenDepth--;
                this.advance();
                continue;
            }
            if (t.type === 'lbracket') {
                bracketDepth++;
                this.advance();
                continue;
            }
            if (t.type === 'rbracket') {
                if (bracketDepth === 0) return null;
                bracketDepth--;
                this.advance();
                continue;
            }
            if (t.type === 'lbrace') {
                braceDepth++;
                this.advance();
                continue;
            }
            if (t.type === 'rbrace') {
                if (braceDepth === 0) return null;
                braceDepth--;
                this.advance();
                continue;
            }
            if (t.type === 'struct' || t.type === 'sig') {
                structSigDepth++;
                this.advance();
                continue;
            }
            if (t.type === 'end') {
                if (structSigDepth > 0) {
                    structSigDepth--;
                    this.advance();
                    continue;
                }
                return null;
            }

            // Handle nested constructs that introduce bindings
            if (t.type === 'let') {
                const letStart = t.offset;
                this.advance();
                const letScope = this.createScope(parentScope, letStart, parentScope.endOffset, 'let');
                this.parseLetBody(letScope);
                letScope.endOffset = this.peek()?.offset ?? parentScope.endOffset;
                continue;
            }
            if (t.type === 'fn') {
                this.parseLambda(parentScope);
                continue;
            }
            if (t.type === 'case') {
                this.parseCaseStatement(parentScope);
                continue;
            }

            // Found semicolon at the right nesting level
            if (t.type === 'semicolon' && parenDepth === 0 && bracketDepth === 0 && braceDepth === 0 && structSigDepth === 0) {
                const offset = t.offset;
                this.advance(); // skip the semicolon
                return offset;
            }

            // Stop conditions
            if (t.type === 'keyword' || t.type === 'and' || t.type === 'in') {
                return null;
            }

            this.advance();
        }

        return null;
    }

    /**
     * Parse the body of a let block, handling declarations and the in...end portion.
     */
    private parseLetBody(letScope: Scope): void {
        // Parse declarations until 'in'
        while (this.index < this.tokens.length) {
            const t = this.peek();
            if (!t) break;

            if (t.type === 'in') {
                this.advance(); // skip 'in'
                break;
            }

            if (t.type === 'keyword') {
                this.parseDeclaration(letScope);
                continue;
            }

            // Handle nested let
            if (t.type === 'let') {
                const letStart = t.offset;
                this.advance();
                const innerLetScope = this.createScope(letScope, letStart, letScope.endOffset, 'let');
                this.parseLetBody(innerLetScope);
                innerLetScope.endOffset = this.peek()?.offset ?? letScope.endOffset;
                continue;
            }

            this.advance();
        }

        // Parse the expression part (in ... end) - look for lambdas and nested lets
        let depth = 0;
        while (this.index < this.tokens.length) {
            const t = this.peek();
            if (!t) break;

            if (t.type === 'let') {
                const letStart = t.offset;
                this.advance();
                const innerLetScope = this.createScope(letScope, letStart, letScope.endOffset, 'let');
                this.parseLetBody(innerLetScope);
                innerLetScope.endOffset = this.peek()?.offset ?? letScope.endOffset;
                continue;
            }

            if (t.type === 'struct' || t.type === 'sig') {
                depth++;
                this.advance();
                continue;
            }

            if (t.type === 'end') {
                if (depth > 0) {
                    depth--;
                    this.advance();
                    continue;
                }
                // This 'end' closes our let block
                this.advance();
                return;
            }

            if (t.type === 'fn') {
                this.parseLambda(letScope);
                continue;
            }

            // Handle case statements
            if (t.type === 'case') {
                this.parseCaseStatement(letScope);
                continue;
            }

            // Handle monadic binding: pattern <- expr ;
            if (this.isMonadicBinding()) {
                const bindings = this.parsePattern();
                if (this.peek()?.type === 'larrow') {
                    this.advance(); // skip <-
                }

                const semicolonOffset = this.skipToSemicolonWithBindings(letScope);
                if (semicolonOffset !== null) {
                    for (const binding of bindings) {
                        const bindingDef: Definition = {
                            name: binding.name,
                            kind: 'val',
                            range: new vscode.Range(
                                this.document.positionAt(binding.offset),
                                this.document.positionAt(binding.offset + binding.name.length)
                            ),
                            selectionRange: new vscode.Range(
                                this.document.positionAt(binding.offset),
                                this.document.positionAt(binding.offset + binding.name.length)
                            ),
                            offset: semicolonOffset + 1,
                        };
                        this.addDefinition(letScope, bindingDef);
                    }
                }
                continue;
            }

            this.advance();
        }
    }

    /**
     * Parse a lambda expression: fn x => body
     */
    private parseLambda(parentScope: Scope): void {
        const fnToken = this.peek();
        if (fnToken?.type !== 'fn') return;
        this.advance(); // skip 'fn'

        const params: Array<{ name: string, offset: number }> = [];

        // Parse parameters until '=>'
        while (this.index < this.tokens.length) {
            const t = this.peek();
            if (!t) break;

            if (t.type === 'arrow') {
                this.advance(); // skip '=>'
                break;
            }

            // Simple identifier (variable or constructor pattern)
            if (t.type === 'ident') {
                if (t.value === '_') {
                    this.advance();
                    continue;
                }
                if (this.isConstructor(t.value)) {
                    params.push(...this.parsePattern());
                } else {
                    params.push({ name: t.value, offset: t.offset });
                    this.advance();
                }
                continue;
            }

            // Type-level parameter: [a] or [a ::: Type]
            if (t.type === 'lbracket') {
                this.advance(); // skip '['
                this.skipToMatchingBracket();
                continue;
            }

            // Parenthesized pattern or typed parameter
            if (t.type === 'lparen') {
                // Check if it's (pattern : type) or just a pattern
                const savedIndex = this.index;
                this.advance();

                let parenDepth = 0;
                let hasColonAtTopLevel = false;
                let scanIndex = this.index;
                while (scanIndex < this.tokens.length) {
                    const scanToken = this.tokens[scanIndex];
                    if (scanToken.type === 'lparen') parenDepth++;
                    else if (scanToken.type === 'rparen') {
                        if (parenDepth === 0) break;
                        parenDepth--;
                    }
                    else if (scanToken.type === 'colon' && parenDepth === 0) {
                        hasColonAtTopLevel = true;
                        break;
                    }
                    else if (scanToken.type === 'comma' && parenDepth === 0) {
                        break;
                    }
                    scanIndex++;
                }

                this.index = savedIndex;

                if (hasColonAtTopLevel) {
                    this.advance(); // skip '('
                    params.push(...this.parsePattern());
                    this.skipToMatchingParen();
                } else {
                    params.push(...this.parsePattern());
                }
                continue;
            }

            // Record pattern
            if (t.type === 'lbrace') {
                params.push(...this.parsePattern());
                continue;
            }

            this.advance();
        }

        if (params.length === 0) return;

        // Create function-body scope for the lambda
        const bodyStartOffset = this.peek()?.offset ?? parentScope.endOffset;
        const lambdaScope = this.createScope(parentScope, bodyStartOffset, parentScope.endOffset, 'function-body');

        // Add parameters to the lambda scope
        for (const param of params) {
            const paramDef: Definition = {
                name: param.name,
                kind: 'param',
                range: new vscode.Range(
                    this.document.positionAt(param.offset),
                    this.document.positionAt(param.offset + param.name.length)
                ),
                selectionRange: new vscode.Range(
                    this.document.positionAt(param.offset),
                    this.document.positionAt(param.offset + param.name.length)
                ),
                offset: param.offset,
            };
            this.addDefinition(lambdaScope, paramDef);
        }

        // Parse the lambda body
        this.parseFunctionBody(lambdaScope);
    }
}

/**
 * Manages discovery and caching of external .urs files.
 */
class FileManager {
    private fileCache = new Map<string, { scope: Scope, uri: vscode.Uri } | null>();
    private basisScope: Scope | null = null;
    private topScope: Scope | null = null;
    private basisUri: vscode.Uri | null = null;
    private topUri: vscode.Uri | null = null;
    private stdlibDir: vscode.Uri | null = null;  // Directory containing basis.urs

    async initialize(): Promise<void> {
        await this.findAndParseBasisFiles();
    }

    private async findAndParseBasisFiles(): Promise<void> {
        const basisFiles = await vscode.workspace.findFiles('**/basis.urs', null, 1);
        const topFiles = await vscode.workspace.findFiles('**/top.urs', null, 1);

        if (basisFiles.length > 0) {
            this.basisUri = basisFiles[0];
            // Store the stdlib directory for looking up other modules
            this.stdlibDir = vscode.Uri.joinPath(this.basisUri, '..');
            const doc = await vscode.workspace.openTextDocument(this.basisUri);
            this.basisScope = this.parseDocument(doc, 'Basis');
        }
        if (topFiles.length > 0) {
            this.topUri = topFiles[0];
            const doc = await vscode.workspace.openTextDocument(this.topUri);
            this.topScope = this.parseDocument(doc, 'Top');
        }
    }

    private parseDocument(doc: vscode.TextDocument, name?: string): Scope {
        const tokens = tokenize(doc.getText());
        const builder = new ScopeBuilder(doc, tokens);
        const scope = builder.build();
        if (name) scope.name = name;
        return scope;
    }

    /**
     * Convert module name to filename: Foo -> foo.urs
     */
    private moduleToFilename(moduleName: string): string {
        return moduleName.charAt(0).toLowerCase() + moduleName.slice(1) + '.urs';
    }

    /**
     * Find and parse a module file by name (e.g., "Foo" -> find "foo.urs")
     */
    async getModuleScope(moduleName: string): Promise<{ scope: Scope, uri: vscode.Uri } | null> {
        // Check cache first
        if (this.fileCache.has(moduleName)) {
            return this.fileCache.get(moduleName)!;
        }

        const filename = this.moduleToFilename(moduleName);
        let uri: vscode.Uri | null = null;

        // First, search in the workspace
        const files = await vscode.workspace.findFiles(`**/${filename}`, null, 1);
        if (files.length > 0) {
            uri = files[0];
        }

        // If not found in workspace, try the standard library directory
        if (!uri && this.stdlibDir) {
            const stdlibPath = vscode.Uri.joinPath(this.stdlibDir, filename);
            try {
                await vscode.workspace.fs.stat(stdlibPath);
                uri = stdlibPath;
            } catch {
                // File doesn't exist in stdlib
            }
        }

        if (!uri) {
            this.fileCache.set(moduleName, null);
            return null;
        }

        const doc = await vscode.workspace.openTextDocument(uri);
        const scope = this.parseDocument(doc, moduleName);

        const result = { scope, uri };
        this.fileCache.set(moduleName, result);
        return result;
    }

    getBasisScope(): Scope | null { return this.basisScope; }
    getTopScope(): Scope | null { return this.topScope; }
    getBasisUri(): vscode.Uri | null { return this.basisUri; }
    getTopUri(): vscode.Uri | null { return this.topUri; }
}

/**
 * Result of resolving a definition, including cross-file info.
 */
interface ResolvedDefinition {
    definition: Definition;
    uri: vscode.Uri;  // The file containing the definition
}

/**
 * Resolver for finding definitions given a position.
 */
class DefinitionResolver {
    private rootScope: Scope;
    private document: vscode.TextDocument;
    private fileManager: FileManager;

    constructor(rootScope: Scope, document: vscode.TextDocument, fileManager: FileManager) {
        this.rootScope = rootScope;
        this.document = document;
        this.fileManager = fileManager;
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
     * Walks up the scope chain, checking direct definitions, opened modules,
     * and standard library scopes (Top, Basis).
     */
    public async resolveSimpleName(name: string, scope: Scope, usageOffset: number): Promise<ResolvedDefinition | null> {
        // 1. Check local scopes
        const localResult = this.resolveInLocalScopes(name, scope, usageOffset);
        if (localResult) return { definition: localResult, uri: this.document.uri };

        // 2. Check opened modules (including external files)
        const openedResult = await this.resolveInOpenedModules(name, scope, usageOffset);
        if (openedResult) return openedResult;

        // 3. Check Top scope (standard library - more specific)
        const topScope = this.fileManager.getTopScope();
        const topUri = this.fileManager.getTopUri();
        if (topScope && topUri) {
            const result = this.searchInModuleScope(name, topScope);
            if (result) return { definition: result, uri: topUri };
        }

        // 4. Check Basis scope (standard library - base types)
        const basisScope = this.fileManager.getBasisScope();
        const basisUri = this.fileManager.getBasisUri();
        if (basisScope && basisUri) {
            const result = this.searchInModuleScope(name, basisScope);
            if (result) return { definition: result, uri: basisUri };
        }

        return null;
    }

    /**
     * Resolve a name in local scopes only (no external files).
     */
    private resolveInLocalScopes(name: string, scope: Scope, usageOffset: number): Definition | null {
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

            // Check opened modules within this scope (local modules only here)
            for (let i = currentScope.opens.length - 1; i >= 0; i--) {
                const opened = currentScope.opens[i];
                if (opened.offset < usageOffset) {
                    // Try to resolve the module reference locally
                    const moduleRef = this.resolveInLocalScopes(opened.moduleRef.name, currentScope, opened.offset);
                    if (moduleRef?.moduleScope) {
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
     * Resolve a name in opened modules, including external .urs files.
     */
    private async resolveInOpenedModules(name: string, scope: Scope, usageOffset: number): Promise<ResolvedDefinition | null> {
        let currentScope: Scope | null = scope;

        while (currentScope) {
            // Check opened modules (later opens shadow earlier ones)
            for (let i = currentScope.opens.length - 1; i >= 0; i--) {
                const opened = currentScope.opens[i];
                if (opened.offset < usageOffset) {
                    const moduleName = opened.moduleRef.name;

                    // Try to resolve the module locally first
                    let moduleScope = this.resolveModuleScope(moduleName, currentScope, opened.offset);
                    let moduleUri: vscode.Uri | null = null;

                    // If not found locally, check external files
                    if (!moduleScope) {
                        const external = await this.fileManager.getModuleScope(moduleName);
                        if (external) {
                            moduleScope = external.scope;
                            moduleUri = external.uri;
                        }
                    }

                    if (moduleScope) {
                        const result = this.searchInModuleScope(name, moduleScope);
                        if (result) {
                            return {
                                definition: result,
                                uri: moduleUri ?? this.document.uri
                            };
                        }
                    }
                }
            }
            currentScope = currentScope.parent;
        }
        return null;
    }

    /**
     * Resolve a module name to its scope (local modules only).
     */
    private resolveModuleScope(name: string, scope: Scope, offset: number): Scope | null {
        const def = this.resolveInLocalScopes(name, scope, offset);
        return def?.moduleScope ?? null;
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
     * Check if the given offset is at a definition site for the given name.
     * Returns the definition if found, null otherwise.
     */
    public findDefinitionAt(name: string, offset: number): Definition | null {
        return this.findDefinitionAtRecursive(this.rootScope, name, offset);
    }

    private findDefinitionAtRecursive(scope: Scope, name: string, offset: number): Definition | null {
        const defs = scope.definitions.get(name);
        if (defs) {
            for (const def of defs) {
                // Check if offset is within the definition's name
                // For definitions, the stored offset is the visibility offset, but selectionRange has the actual position
                const selStart = this.document.offsetAt(def.selectionRange.start);
                const selEnd = this.document.offsetAt(def.selectionRange.end);
                if (offset >= selStart && offset < selEnd) {
                    return def;
                }
            }
        }
        for (const child of scope.children) {
            const found = this.findDefinitionAtRecursive(child, name, offset);
            if (found) return found;
        }
        return null;
    }

    /**
     * Resolve a qualified name like A.B.c.
     */
    public async resolveQualifiedName(parts: string[], scope: Scope, usageOffset: number): Promise<ResolvedDefinition | null> {
        if (parts.length === 0) return null;

        const moduleName = parts[0];

        // 1. Try to resolve first part as local module
        const localModuleDef = this.resolveInLocalScopes(moduleName, scope, usageOffset);

        // 2. Handle special cases: Basis and Top qualified access
        if (!localModuleDef) {
            if (moduleName === 'Basis') {
                const basisScope = this.fileManager.getBasisScope();
                const basisUri = this.fileManager.getBasisUri();
                if (basisScope && basisUri && parts.length > 1) {
                    const result = this.resolveInScope(parts.slice(1), basisScope);
                    if (result) return { definition: result, uri: basisUri };
                }
                return null;
            }
            if (moduleName === 'Top') {
                const topScope = this.fileManager.getTopScope();
                const topUri = this.fileManager.getTopUri();
                if (topScope && topUri && parts.length > 1) {
                    const result = this.resolveInScope(parts.slice(1), topScope);
                    if (result) return { definition: result, uri: topUri };
                }
                return null;
            }
        }

        // 3. If not found locally, look for external .urs file
        if (!localModuleDef || !localModuleDef.moduleScope) {
            const external = await this.fileManager.getModuleScope(moduleName);
            if (external && parts.length > 1) {
                const result = this.resolveInScope(parts.slice(1), external.scope);
                if (result) return { definition: result, uri: external.uri };
            }
            // If parts.length === 1, we're looking for the module itself
            // (e.g., just "Foo" not "Foo.bar")
            if (external && parts.length === 1) {
                // Return a synthetic definition for the module itself
                // This case is rare - usually we're looking for members
                return null;
            }
            return null;
        }

        // 4. Found local module - resolve remaining parts
        if (parts.length === 1) {
            return { definition: localModuleDef, uri: this.document.uri };
        }
        const result = this.resolveInScope(parts.slice(1), localModuleDef.moduleScope);
        if (result) return { definition: result, uri: this.document.uri };
        return null;
    }

    /**
     * Resolve remaining parts within a scope.
     */
    private resolveInScope(parts: string[], scope: Scope): Definition | null {
        if (parts.length === 0) return null;

        const def = this.searchInModuleScope(parts[0], scope);
        if (!def) return null;

        if (parts.length === 1) return def;

        // Nested module access
        if (def.moduleScope) {
            return this.resolveInScope(parts.slice(1), def.moduleScope);
        }
        return null;
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
    private fileManager: FileManager;

    constructor() {
        this.fileManager = new FileManager();
    }

    async initialize(): Promise<void> {
        await this.fileManager.initialize();
    }

    public async provideDefinition(
        document: vscode.TextDocument,
        position: vscode.Position,
        _token: vscode.CancellationToken
    ): Promise<vscode.Definition | null> {
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
            const resolver = new DefinitionResolver(rootScope, document, this.fileManager);
            const scope = resolver.findScopeAt(cursorOffset);

            // Check if we're clicking on a definition site itself
            if (qualifiedName.parts.length === 1) {
                const defAtCursor = resolver.findDefinitionAt(qualifiedName.parts[0], cursorOffset);
                if (defAtCursor) {
                    // We're on a definition site - return null (already at definition)
                    return null;
                }
            }

            // Resolve the name (now async for external files)
            let resolved: ResolvedDefinition | null;
            if (qualifiedName.parts.length === 1) {
                resolved = await resolver.resolveSimpleName(qualifiedName.parts[0], scope, cursorOffset);
            } else {
                resolved = await resolver.resolveQualifiedName(qualifiedName.parts, scope, cursorOffset);
            }

            if (!resolved) return null;

            // Return the location with the correct URI
            return new vscode.Location(resolved.uri, resolved.definition.selectionRange);
        } catch (e) {
            console.error('UrWeb definition provider error:', e);
            return null;
        }
    }
}
