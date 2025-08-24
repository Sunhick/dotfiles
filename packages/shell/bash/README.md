# Bash Configuration

XDG-compliant bash configuration with macOS compatibility and intelligent loading.

## Features

- **Smart Loading**: Automatically detects available tools and adapts
- **Cross-Platform**: Works on macOS and Linux with platform-specific optimizations
- **Gruvbox Colors**: Beautiful gruvbox dark hard theme for ls output
- **Git Integration**: Shows git branch in prompt and includes git aliases
- **Modern Tools**: Integrates with bat, exa/eza, fd, ripgrep when available
- **Safe Defaults**: Includes safety aliases and better history management
- **XDG Compliant**: Follows XDG Base Directory specification

## Files

### Compatibility Layer
- `.bashrc` - Compatibility wrapper that sources XDG-compliant config
- `.bash_profile` - macOS compatibility (sources .bashrc)

### XDG-Compliant Configuration (`~/.config/bash/`)
- `bashrc` - Main XDG-compliant configuration
- `exports` - Environment variables and PATH management
- `prompt` - Custom prompt with git integration
- `aliases` - Command aliases and shortcuts
- `colors` - Color definitions for prompt and output
- `dir_colors` - Gruvbox theme for GNU ls colors
- `shopt` - Shell options and behavior settings
- `darwin` - macOS-specific configuration (gls preference, LSCOLORS)
- `linux` - Linux-specific configuration

### Logout Configuration
- `.bash_logout` - Cleanup tasks on shell exit (clear history, temp files)

## Installation

```bash
cd packages/shell
make install-bash
```

## Customization

### Local Overrides
Create `~/.config/bash/local` for machine-specific customizations that won't be tracked in git.

### macOS Compatibility
The setup provides both:
- `~/.bashrc` - Compatibility wrapper for systems expecting this location
- `~/.config/bash/bashrc` - XDG-compliant main configuration

This ensures compatibility with macOS while maintaining clean XDG organization.

### Color Themes
Edit `.colors` to change the color scheme. Three themes are available:
- `vibrant_colors` (default)
- `original_colors`
- `muted_colors`

### Adding Aliases
Add personal aliases to `~/.bashrc.local` or modify `.aliases` directly.

## Dependencies

### Required
- **bash** - The shell itself

### Optional (Enhanced Functionality)

#### Core Tools
- **git** - Git integration in prompt and aliases
- **fzf** - Fuzzy finder for command history and file search
- **GNU coreutils** - Better ls colors with dir_colors support
  ```bash
  # macOS (Homebrew)
  brew install coreutils
  # Provides: gls, gdircolors, etc.
  ```

#### Modern CLI Replacements
- **bat** - Syntax-highlighted cat replacement
- **exa/eza** - Modern ls replacement with git integration
- **fd** - Fast and user-friendly find replacement
- **ripgrep (rg)** - Fast text search tool

#### Terminal & Editor Integration
- **iTerm2** (macOS) - Enhanced terminal with better color support
- **GNU Readline** - Better line editing (usually system-provided)
- **Emacs** - Editor integration with emacsclient aliases

#### Installation Commands

**macOS (Homebrew):**
```bash
# Core enhancements
brew install coreutils fzf git

# Modern CLI tools
brew install bat eza fd ripgrep

# Terminal & Editor
brew install --cask iterm2
brew install emacs
```

**Linux (Ubuntu/Debian):**
```bash
# Core tools
sudo apt install git fzf

# Modern CLI tools
sudo apt install bat fd-find ripgrep

# Note: exa/eza may need manual installation or snap
```

### Color Support

The configuration includes two color systems:

1. **GNU dir_colors** (when coreutils installed)
   - Full gruvbox dark hard theme
   - Extensive file type recognition
   - Used by `gls --color=auto`

2. **BSD LSCOLORS** (macOS fallback)
   - Gruvbox-inspired colors for BSD ls
   - Used when GNU coreutils not available
