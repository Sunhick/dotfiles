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

### History Navigation
- `Ctrl+R` / `Ctrl+S`: Incremental history search (reverse/forward)
- `Up/Down arrows`: History search based on current input
- `Alt+P` / `Alt+N`: Previous/next history

### Line Navigation (Emacs-style)
- `Ctrl+A`: Beginning of line
- `Ctrl+E`: End of line
- `Ctrl+F`: Forward character
- `Ctrl+B`: Backward character
- `Home/End`: Beginning/end of line

### Word Navigation
- `Alt+F` / `Alt+B`: Forward/backward word
- `Alt + Left/Right`: Word navigation (arrow keys)
- `Ctrl + Left/Right`: Alternative word navigation

### Text Editing
- `Ctrl+D`: Delete character forward
- `Ctrl+H`: Delete character backward (backspace)
- `Ctrl+K`: Kill (cut) to end of line
- `Ctrl+U`: Kill (cut) entire line
- `Ctrl+W`: Kill (cut) word backward
- `Ctrl+Y`: Yank (paste)
- `Ctrl+T`: Transpose characters

### Word Operations
- `Alt+D`: Kill word forward
- `Alt+Backspace`: Kill word backward
- `Alt+U`: Uppercase word
- `Alt+L`: Lowercase word
- `Alt+C`: Capitalize word

### Completion
- `Tab`: Complete
- `Alt+?`: Show possible completions
- `Alt+*`: Insert all completions
- `Alt+/`: Complete filename
- `Alt+~`: Complete username
- `Alt+$`: Complete variable
- `Alt+@`: Complete hostname

### Miscellaneous
- `Ctrl+L`: Clear screen
- `Ctrl+G`: Abort current operation
- `Ctrl+X Ctrl+U`: Undo
- `Ctrl+X Ctrl+R`: Re-read inputrc file

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
