# Bash Configuration

Modern bash configuration with intelligent loading and cross-platform support.

## Features

- **Smart Loading**: Automatically detects available tools and adapts
- **Cross-Platform**: Works on macOS and Linux with platform-specific optimizations
- **Git Integration**: Shows git branch in prompt and includes git aliases
- **Modern Tools**: Integrates with bat, exa, fd, ripgrep when available
- **Safe Defaults**: Includes safety aliases and better history management

## Files

- `.bashrc` - Main configuration entry point
- `.bash_profile` - macOS compatibility (sources .bashrc)
- `.bash_export` - Environment variables and PATH management
- `.bash_history` - History configuration and settings
- `.bash_prompt` - Custom prompt with git integration
- `.aliases` - Command aliases and shortcuts
- `.colors` - Color definitions for prompt and output
- `.shopt` - Shell options and behavior settings
- `.darwin` - macOS-specific configuration
- `.linux` - Linux-specific configuration

## Installation

```bash
cd packages/shell
make install-bash
```

## Customization

### Local Overrides
Create `~/.bashrc.local` for machine-specific customizations that won't be tracked in git.

### Color Themes
Edit `.colors` to change the color scheme. Three themes are available:
- `vibrant_colors` (default)
- `original_colors`
- `muted_colors`

### Adding Aliases
Add personal aliases to `~/.bashrc.local` or modify `.aliases` directly.

## Dependencies

**Required:**
- bash

**Optional (enhanced functionality):**
- git - for git integration
- fzf - for fuzzy finding
- bat - better cat with syntax highlighting
- exa - modern ls replacement
- fd - fast file finder
- ripgrep - fast text search
