# inputrc

GNU Readline configuration for enhanced command line editing (XDG compliant).

## Features

- Case-insensitive tab completion
- Intelligent history search with arrow keys
- Enhanced word navigation with Alt/Ctrl + arrow keys
- Colored completion output
- UTF-8 support
- Disabled bell sounds
- Smart completion behavior

## Key Bindings

- `Ctrl+R` / `Ctrl+S`: Incremental history search (reverse/forward)
- `Up/Down arrows`: History search based on current input
- `Alt + Left/Right`: Word navigation
- `Ctrl + Left/Right`: Alternative word navigation
- `Alt + Delete`: Delete preceding word
- `Home/End`: Beginning/end of line

## Installation

This package follows XDG Base Directory specification and will be installed to `~/.config/readline/inputrc`.

The bash configuration automatically loads this file. The configuration will be used by:
- Bash (via explicit loading in bashrc)
- Python REPL (when INPUTRC environment variable is set)
- MySQL client
- PostgreSQL client
- Any other program using GNU Readline

## XDG Compliance

- Configuration file: `~/.config/readline/inputrc`
- Loaded automatically by bash configuration
- Falls back to `~/.inputrc` if XDG location doesn't exist
