# Bash Configuration

XDG-compliant bash configuration with macOS compatibility and intelligent loading.

## Features

- **Smart Loading**: Automatically detects available tools and adapts
- **Cross-Platform**: Works on macOS and Linux with platform-specific optimizations
- **Git Integration**: Shows git branch in prompt and includes git aliases
- **Modern Tools**: Integrates with bat, exa, fd, ripgrep when available
- **Safe Defaults**: Includes safety aliases and better history management

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
- `shopt` - Shell options and behavior settings
- `darwin` - macOS-specific configuration
- `linux` - Linux-specific configuration

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

**Required:**
- bash

**Optional (enhanced functionality):**
- git - for git integration
- fzf - for fuzzy finding
- bat - better cat with syntax highlighting
- exa - modern ls replacement
- fd - fast file finder
- ripgrep - fast text search
