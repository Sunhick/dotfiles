# XDG-Compliant Dotfiles

Personal dotfiles managed with GNU Stow, following the XDG Base Directory Specification for clean and organized configuration management.

## Quick Start

```bash
git clone https://github.com/sunhick/dotfiles.git ~/.dotfiles
cd ~/.dotfiles
make install
```

### Enhanced Experience (Optional Dependencies)

For the best experience, especially with the bash shell configuration, install these tools:

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
sudo apt install git fzf coreutils

# Modern CLI tools
sudo apt install bat fd-find ripgrep
```

## XDG Base Directory Specification

This dotfiles setup follows the XDG Base Directory Specification:

- **`~/.config/`** - Configuration files
- **`~/.local/share/`** - Data files
- **`~/.cache/`** - Cache files
- **`~/.local/state/`** - State files

### Benefits
- **Clean home directory** - No more dot-file clutter
- **Standardized locations** - Follows Linux/Unix standards
- **Better organization** - Logical separation of config, data, cache
- **Future-proof** - Modern applications expect XDG compliance

## Structure

```
dotfiles/
├── make/               # shared Makefile helpers (log.mk)
├── packages/
│   ├── shell/          # bash (.config/bash/), zsh (.config/zsh/), powershell
│   ├── editors/        # emacs (.config/emacs/), nano (.config/nano/), vscode (.config/Code/)
│   ├── tools/          # tmux (.config/tmux/), htop (.config/htop/), gnupg (.gnupg/)
│   ├── desktop/        # i3 (.config/i3/), iterm2 (.config/)
│   └── development/    # git (.config/git/)
├── management/stow/    # stow configuration
└── Makefile            # installation commands
```

## Featured Packages

### Shell (bash)
- **Gruvbox colors** - Beautiful dark theme for terminal output
- **Smart tool detection** - Automatically uses modern CLI tools when available
- **Cross-platform** - Works on macOS and Linux with platform-specific optimizations
- **XDG compliant** - Clean organization in `~/.config/bash/`
- **Git integration** - Branch display in prompt, useful aliases

**Key files:**
- `dir_colors` - Gruvbox theme for GNU ls
- `darwin` - macOS-specific config (prefers gls, LSCOLORS fallback)
- `aliases` - Smart aliases that adapt to available tools
- `bash_logout` - Cleanup on shell exit

## Installation

### Install Everything
```bash
make install
```

### Install by Category
```bash
make shell              # bash, zsh
make editors            # emacs, nano, vscode
make tools              # tmux, htop, gnupg, etc.
make desktop            # i3, iterm2
make development        # git
```

### Install Individual Packages
```bash
cd packages/tools
make tmux

# or directly with stow
cd packages/tools/tmux
stow --target=$HOME .
```

## Commands

| Command            | Description                                    |
| ------------------ | ---------------------------------------------- |
| `make install`     | Install all packages                           |
| `make install-safe`| Backup existing configs then install            |
| `make uninstall`   | Remove all packages                            |
| `make dryrun`      | Preview changes (dry run)                      |
| `make restow`      | Re-link all packages                           |
| `make list`        | List available packages                        |
| `make doctor`      | Verify tools, symlinks, and config health      |
| `make backup`      | Backup existing configuration files            |
| `make restore`     | Restore from latest backup                     |

## Adding Packages

1. Create directory in appropriate category:
   ```bash
   mkdir packages/tools/mynewpackage
   ```

2. Add your dotfiles following XDG structure:
   ```bash
   packages/tools/mynewpackage/
   └── .config/
       └── mynewpackage/
           └── config.yml
   ```

3. Install:
   ```bash
   cd packages/tools
   make mynewpackage
   ```

### XDG Guidelines for New Packages
- Configuration files → `.config/appname/`
- Data files → `.local/share/appname/`
- Cache files → `.cache/appname/`
- State files → `.local/state/appname/`

## Color Theming

The dotfiles use a **Gruvbox Dark Hard** color scheme throughout:

### Terminal Colors (bash)
- **GNU dir_colors** - Full gruvbox theme when coreutils installed
- **BSD LSCOLORS** - Gruvbox-inspired fallback for macOS
- **Smart detection** - Automatically chooses best color system

### Color Priority
1. **gls --color=auto** (GNU coreutils) - Full gruvbox dir_colors
2. **ls -G** (BSD) - Gruvbox LSCOLORS on macOS
3. **exa/eza** - Modern ls with built-in colors

Install GNU coreutils for the best color experience:
```bash
# macOS
brew install coreutils

# Test colors
gls -la --color=auto
```

## Requirements

### Essential
- [GNU Stow](https://www.gnu.org/software/stow/) - For symlink management
- Git - For version control and git integration

### Optional (Enhanced Experience)
- **GNU coreutils** - Better ls colors (gls, gdircolors)
- **fzf** - Fuzzy finder for command history
- **Modern CLI tools** - bat, eza, fd, ripgrep
- **iTerm2** (macOS) - Enhanced terminal with better color support
- **Emacs** - Editor integration

See individual package READMEs for specific dependencies.

## License

MIT
