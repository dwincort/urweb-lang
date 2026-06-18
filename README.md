# Ur/Web for VS Code

Language support for the [Ur/Web language](http://impredicative.com/ur/): syntax
highlighting for `.ur`, `.urs`, and `.urp` files, plus full IDE features
(diagnostics, go-to-definition, hover, completion, references, rename, document
symbols, and semantic highlighting) powered by the Ur/Web compiler's built-in
language server.

## Features

- **Syntax highlighting** for source (`.ur`), signature (`.urs`), and project
  (`.urp`) files — works immediately, no server required.
- **Language server features** when the Ur/Web compiler is available:
  - Diagnostics (errors and warnings) as you edit
  - Go to definition / references
  - Hover information
  - Code completion
  - Rename
  - Document symbols / outline
  - Semantic highlighting layered on top of the base syntax highlighting

The syntax highlighting works on its own. The richer features require an Ur/Web
compiler that supports the language server (see below).

## Requirements

You need an `urweb` executable that supports the `-startLspServer` flag. The
extension launches it as `urweb -startLspServer` and communicates over stdio.

Verify your compiler supports it:

```bash
urweb -startLspServer
```

(It will wait for LSP input — press `Ctrl-C` to exit.)

## Setup

### 1. Install the extension

Install from the VS Code Marketplace, or from a local `.vsix`:

```bash
code --install-extension urweb-lang-*.vsix
```

### 2. Point the extension at your compiler

If `urweb` is on your `PATH`, nothing else is needed. Otherwise, set the path to
the executable in your **workspace** settings (`.vscode/settings.json` in your
Ur/Web project):

```json
{
  "urweb.server.path": "/absolute/path/to/urweb"
}
```

### 3. Open a project

Open a `.ur` or `.urs` file — the server starts automatically. The extension
finds the project root by walking up from the file you open to the nearest
ancestor directory that contains one of the root markers configured via
`urweb.rootMarkers` (by default, a `.git` folder). If no marker is found, the
workspace folder containing the file is used instead.

## Extension settings

| Setting | Default | Description |
| --- | --- | --- |
| `urweb.server.path` | `"urweb"` | Path to the `urweb` executable used to start the language server. |
| `urweb.server.args` | `["-startLspServer"]` | Arguments passed to the executable to start the language server. |
| `urweb.rootMarkers` | `[".git"]` | File or directory names that mark a project root. The server is started in the closest ancestor directory of an Ur/Web file containing one of these — useful if you use a VCS other than git (e.g. `[".hg"]`). If none is found, the workspace folder is used. |
| `urweb.trace.server` | `"off"` | Trace the communication between VS Code and the language server (`off`, `messages`, `verbose`). Useful for debugging. |

## Troubleshooting

- **`urweb -startLspServer` does nothing / errors** — your compiler build may not
  include language-server support. Use a build that does.

## License

MIT — see [LICENSE](./LICENSE).
