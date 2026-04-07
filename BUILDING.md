# Building and Installing

## Prerequisites

- [Node.js](https://nodejs.org/) (v16+)
- [VS Code](https://code.visualstudio.com/)

## Build

```bash
npm install
npm run compile
```

Use `npm run watch` for continuous compilation during development.

## Test Locally

Press **F5** in VS Code to launch an Extension Development Host with the extension loaded.

## Package and Install

Install the packaging tool:

```bash
npm install -g @vscode/vsce
```

Build a `.vsix` file:

```bash
vsce package
```

Install the `.vsix` into VS Code:

```bash
code --install-extension urweb-lang-0.1.0.vsix
```

## Uploading
```bash
vsce publish -p <Personal Access Token>
```
and
```bash
ovsx publish -p <Personal Access Token>
```
