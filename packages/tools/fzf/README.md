# FZF Configuration

Minimal XDG-compliant configuration for [fzf](https://github.com/junegunn/fzf) (fuzzy finder).

## Features

- **XDG compliant** - Configuration in `~/.config/fzf/`
- **Gruvbox theme** - Matches dotfiles color scheme
- **Smart tool integration** - Uses `fd` and `bat` when available
- **Cross-platform** - Works on macOS and Linux

## Installation

```bash
# Install fzf first
brew install fzf  # macOS
# or
sudo apt install fzf  # Ubuntu/Debian

# Install this configuration
cd packages/tools
make fzf
```

## Key Bindings

- `Ctrl+T` - Find files
- `Ctrl+R` - Search command history
- `Alt+C` - Find directories and cd

## Configuration

- **Colors**: Gruvbox Dark Hard theme
- **File finder**: Uses `fd` if available, falls back to `find`
- **Preview**: Uses `bat` for syntax highlighting if available
