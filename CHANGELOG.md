# Changelog

## 0.1.1 - 2026-03-02

- Add companion file navigation (clicking definitions switches between `.ur` and `.urs`)
- Add build instructions
- Simplify ScopeBuilder with extracted helpers
- Handle `[]` list patterns, `::` cons patterns, and `val...and` chains

## 0.1.0 - 2026-01-31

- Add Go to Symbol and Code Outline support
- Add Go to Definition support for Ur/Web
- Enhance Go to Definition with cross-module workspace support
- Add proper Go to Definition support around if-then-else
- Add support for datatype constructors in symbol provider
- Add MIT license

## 0.0.4 - 2026-01-29

- Add Go to Symbol and Code Outline support (DocumentSymbolProvider)

## 0.0.3 - 2024-03-27

- Integrate Emacs syntax definitions from Ur/Web source
- Separate `.urp` project files for better highlighting
- Add file icons
- Support nested block comments in grammar
- Support intermingling Ur/Web and XML in syntax highlighting
- Make `<xml/>` a keyword and improve auto-close behavior
- Add `money` as a base type
- Fix bad highlighting for Ur/Web nested in XML

## 0.0.2 - 2023-11-07

- Remove buggy infix operators

## 0.0.1 - 2023-11-07

- Initial release with TextMate grammar for `.ur` and `.urs` files
