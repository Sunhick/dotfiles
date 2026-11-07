# Bash Configuration

XDG-compliant bash configuration with modern enhancements.

## Features

- **XDG Base Directory Specification** - Clean organization in `~/.config/bash/`
- **Gruvbox color scheme** - Beautiful dark theme throughout
- **Smart tool detection** - Automatically uses modern CLI tools when available
- **Cross-platform support** - Works on macOS and Linux
- **Git integration** - Branch in the prompt (git's `__git_ps1`), useful aliases
- **Enhanced history** - Timestamps, shared live across terminals
- **Modern shell options** - Improved navigation and completion
- **Emacs as editor** - `EDITOR`/`VISUAL` are `emacsclient -t` (starts the daemon if needed)
- **zoxide / mise** - Set up automatically when installed (`z foo` to jump; per-project tool versions)

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
- bash 4.0+ (macOS: `brew install bash`; `/bin/bash` is 3.2)

### Optional (Enhanced Experience)
- **GNU coreutils** - Better ls colors (`brew install coreutils`)
- **fzf** - Fuzzy finder for command history
- **bat** - Better cat with syntax highlighting
- **eza** - Modern ls replacement (falls back to exa)
- **fd** - Modern find replacement
- **ripgrep** - Modern grep replacement
- **zoxide** - Frecency-based `cd` (`z`, `zi`)
- **mise** - Per-project tool versions (`.mise.toml`, `.tool-versions`)

The zoxide and mise init scripts are cached in `~/.cache/bash/` and
regenerated when the tool is upgraded.

## Installation

From the repo root:

```bash
make bash
```

Or directly with stow:
```bash
cd bash
stow --no-folding --target=$HOME .
```
