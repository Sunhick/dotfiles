# Ripgrep Configuration

Fast and intelligent text search configuration for ripgrep (rg).

## Features

- **Smart defaults** - Case-insensitive search, follow symlinks, search hidden files
- **Beautiful colors** - Gruvbox-inspired color scheme for output
- **Performance optimized** - Reasonable column limits and preview settings
- **Comprehensive ignores** - Excludes common build artifacts, dependencies, and temporary files
- **Custom file types** - Predefined types for web, config, and documentation files

## Files

- `.config/ripgrep/config` - Main ripgrep configuration
- `.rgignore` - Global ignore patterns (alternative to .gitignore for rg)

## Installation

```bash
cd packages/tools
make install-ripgrep

# or directly with stow
stow ripgrep
```

## Configuration Details

### Search Behavior
- `--smart-case` - Case insensitive unless uppercase letters are used
- `--follow` - Follow symbolic links
- `--hidden` - Search hidden files and directories
- `--context=2` - Show 2 lines of context around matches

### Colors (Gruvbox-inspired)
- **Line numbers** - Yellow and bold
- **File paths** - Green and bold
- **Matches** - Black text on yellow background

### Custom File Types
- `web` - HTML, CSS, JS, TS, Vue, Svelte files
- `config` - JSON, YAML, TOML, INI configuration files
- `docs` - Markdown, RST, text documentation files

### Performance
- `--max-columns=150` - Limit line length for performance
- `--max-columns-preview` - Show preview for long lines

## Usage Examples

```bash
# Basic search
rg "function"

# Search specific file types
rg "TODO" --type=web
rg "password" --type=config

# Search with context
rg "error" --context=5

# Case sensitive search
rg "Error" --case-sensitive

# Search in specific directory
rg "pattern" src/

# Show files that would be searched
rg --files

# Show files matching pattern
rg --files | rg "\.js$"
```

## Environment Variables

The configuration uses these environment variables:

- `RIPGREP_CONFIG_PATH` - Path to config file (automatically set to `~/.config/ripgrep/config`)

## Dependencies

- **ripgrep** - The search tool itself

### Installation

**macOS (Homebrew):**
```bash
brew install ripgrep
```

**Linux (Ubuntu/Debian):**
```bash
sudo apt install ripgrep
```

**Arch Linux:**
```bash
sudo pacman -S ripgrep
```

## Customization

### Adding Custom File Types
Edit `.config/ripgrep/config` and add:
```
--type-add=mytype:*.{ext1,ext2,ext3}
```

### Adding Global Ignores
Edit `.rgignore` to add patterns that should be ignored globally.

### Local Project Ignores
Create `.rgignore` in project root for project-specific ignore patterns.

## Integration

Ripgrep integrates well with:
- **fzf** - Fuzzy finder with rg backend
- **Vim/Neovim** - Fast grep replacement
- **Emacs** - Search backend for various packages
- **VS Code** - Can be configured as search provider
