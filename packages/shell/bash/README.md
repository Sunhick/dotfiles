# Bash Configuration

XDG-compliant bash configuration with modern enhancements.

## Features

- **XDG Base Directory Specification** - Clean organization in `~/.config/bash/`
- **Gruvbox color scheme** - Beautiful dark theme throughout
- **Smart tool detection** - Automatically uses modern CLI tools when available
- **Cross-platform support** - Works on macOS and Linux
- **Git integration** - Branch display in prompt, useful aliases
- **Enhanced history** - Better history management with timestamps
- **Modern shell options** - Improved navigation and completion

## Files

- `bashrc` - Main configuration file
- `prompt` - Custom prompt with git integration
- `aliases` - Smart aliases that adapt to available tools
- `colors` - Gruvbox color definitions
- `history` - Enhanced history configuration
- `shopt` - Shell options for better experience
- `exports` - Environment variables
- `darwin` - macOS-specific configuration
- `linux` - Linux-specific configuration
- `dir_colors` - Gruvbox theme for GNU ls

## Dependencies

### Required
- bash 4.0+

### Optional (Enhanced Experience)
- **GNU coreutils** - Better ls colors (`brew install coreutils`)
- **fzf** - Fuzzy finder for command history
- **bat** - Better cat with syntax highlighting
- **eza/exa** - Modern ls replacement
- **fd** - Modern find replacement
- **ripgrep** - Modern grep replacement

## Installation

```bash
cd packages/shell
make install-bash
```

Or directly with stow:
```bash
cd packages/shell/bash
stow --target=$HOME .
```
