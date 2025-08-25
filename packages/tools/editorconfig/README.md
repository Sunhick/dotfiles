# EditorConfig

EditorConfig helps maintain consistent coding styles for multiple developers working on the same project across various editors and IDEs.

## What it does

- Maintains consistent indentation (spaces vs tabs, indent size)
- Ensures consistent line endings (LF vs CRLF)
- Manages trailing whitespace
- Sets character encoding
- Controls final newlines

## Installation

This package installs a global `.editorconfig` file to your home directory.

## Supported Editors

Most modern editors support EditorConfig either natively or through plugins:
- VS Code (built-in)
- Vim/Neovim (plugin required)
- Emacs (plugin required)
- Sublime Text (plugin required)
- IntelliJ IDEA (built-in)
- Atom (plugin required)

## Configuration

The included `.editorconfig` provides sensible defaults for common file types:
- 4 spaces for Python, Java, R
- 2 spaces for JavaScript, TypeScript, JSON, YAML, HTML, CSS
- Tabs for Go and Makefiles
- Unix line endings (LF) for all files
- UTF-8 encoding
- Trim trailing whitespace (except Markdown)
- Insert final newline
