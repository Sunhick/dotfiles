# Modern Dotfiles

A modernized, modular dotfiles configuration with intelligent loading, XDG Base Directory compliance, and cross-platform support.

## ✨ Features

- **🚀 Intelligent Module Loading**: Async loading with dependency resolution and feature detection
- **📦 Advanced Stow Management**: Comprehensive package management with conflict resolution
- **📁 XDG Base Directory Compliant**: Clean home directory organization following XDG standards
- **🛡️ Enhanced Error Handling**: Comprehensive logging and graceful degradation
- **🔧 Cross-Platform**: Works seamlessly on macOS and Linux
- **⚡ Performance Optimized**: Conditional loading based on available tools
- **🧩 Modular Architecture**: Easy to extend and customize
- **🔄 Automated Workflows**: Make targets for common operations
- **🛠️ Dependency Validation**: Ensures required tools are available
- **💾 Backup System**: Automatic backups before making changes

## 🏗️ Architecture

```
dotfiles/
├── bash/                           # Bash configuration package
│   ├── core/                      # Core infrastructure
│   │   ├── loader.bash           # Module loading system
│   │   └── error_handler.bash    # Error handling & validation
│   ├── features/                  # Feature modules
│   │   └── xdg.bash              # XDG Base Directory support
│   ├── .bashrc                   # Main bash configuration
│   ├── .bash_profile             # Bash profile
│   ├── .aliases                  # Command aliases
│   ├── .colors                   # Color definitions
│   ├── .darwin                   # macOS-specific config
│   ├── .linux                    # Linux-specific config
│   ├── .stow-dependencies        # Package dependencies
│   └── .stow-local-ignore        # Package-specific ignores
├── emacs/                          # Emacs configuration package
│   ├── .emacs.d/                 # Emacs configuration directory
│   ├── .stow-dependencies        # Emacs dependencies
│   └── .stow-local-ignore        # Emacs-specific ignores
├── git/                            # Git configuration package
│   ├── .config/git/              # XDG-compliant git config
│   └── .stow-local-ignore        # Git-specific ignores
├── tmux/                           # Tmux configuration package
│   ├── .tmux.conf                # Tmux configuration
│   └── .stow-dependencies        # Tmux dependencies
├── zsh/                            # Zsh configuration package
│   └── .zshrc                    # Zsh configuration
├── vscode/                         # VS Code configuration package
│   └── settings.json             # VS Code settings
├── stow/                           # Stow management configuration
│   ├── .stowrc                   # Advanced stow configuration
│   ├── .stow-global-ignore       # Global ignore patterns
│   ├── .stow-aliases             # Convenient stow aliases
│   └── .stow-hooks               # Pre/post stow hooks
├── htop/                           # htop configuration package
├── i3/                             # i3 window manager package
├── iterm2/                         # iTerm2 configuration package
├── nano/                           # nano editor package
├── gnupg/                          # GnuPG configuration package
├── Makefile                        # Advanced stow operations
├── installer.sh                    # Installation script
└── README.md                       # This file
```

## 📋 Prerequisites

### Required
- **Bash 5+** (for modern features and associative arrays)

### Recommended Tools
- [fzf](https://github.com/junegunn/fzf) - Fuzzy finder
- [ripgrep (rg)](https://github.com/BurntSushi/ripgrep) - Fast text search
- [fd](https://github.com/sharkdp/fd) - Fast file finder
- [bat](https://github.com/sharkdp/bat) - Better cat with syntax highlighting
- [delta](https://github.com/dandavison/delta) - Better git diff
- [exa](https://github.com/ogham/exa) - Modern ls replacement
- [lazygit](https://github.com/jesseduffield/lazygit) - Git TUI

### Legacy Support
- [oh-my-zsh](https://github.com/robbyrussell/oh-my-zsh) (if using zsh)
- [Emacs](https://emacsformacosx.com) (macOS) / [Emacs](https://www.gnu.org/software/emacs/) (Linux)
- [Tmux](https://github.com/tmux/tmux)
- [stow](https://www.gnu.org/software/stow/manual/stow.html)
- [i3](https://i3wm.org) (Linux only)

## 🚀 Installation

### Quick Start
```bash
git clone --recursive https://github.com/Sunhick/dotfiles.git ~/.dotfiles/dotfiles
cd ~/.dotfiles/dotfiles
./installer.sh
```

### Advanced Stow Installation
```bash
# Install all packages
make install

# Install specific packages
make install-bash install-git install-tmux

# Install core packages only
make install-core

# Platform-specific installation
make install-macos    # macOS packages
make install-linux    # Linux packages
```

### Manual Stow Operations
```bash
# Install single package
stow bash

# Remove package
stow --delete bash

# Update package (restow)
stow --restow bash

# Test installation (dry run)
stow --simulate bash
```

### Legacy Installation (Modern Loader)
```bash
# Source the core infrastructure
source bash/core/loader.bash

# Load specific modules
load_module "xdg"
load_module "git"
load_module "navigation"
```

## 🔧 Configuration

### Stow Management
```bash
# Check package status
make status

# Validate package structure
make validate

# Create backup before changes
make backup

# Clean broken symlinks
make clean

# Test all packages (dry run)
make test
```

### Package-Specific Operations
```bash
# Source stow aliases for convenience
source stow/.stow-aliases

# Quick package operations
stow-pkg install bash
stow-pkg remove tmux
stow-pkg update git

# Batch operations
stow-install-all
stow-remove-all
stow-update-all
```

### Environment Variables
```bash
# Enable debug mode
export DEBUG_MODE=true
export DEBUG_LOADER=true

# Customize XDG directories (optional)
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_STATE_HOME="$HOME/.local/state"
```

### Module Management (Legacy Loader)
```bash
# List all modules and their status
list_modules

# Check module status
module_status "git"

# Load deferred modules when needed
load_deferred_modules
```

## 📦 Advanced Stow Management

This dotfiles repository uses an advanced stow setup with intelligent package management:

### Package Structure
```
package/
├── .stow-local-ignore     # Package-specific ignore patterns
├── .stow-dependencies     # Required tools/packages
└── files...               # Actual dotfiles
```

### Stow Configuration Features
- **Comprehensive ignore patterns** - Automatically ignores system files, backups, and build artifacts
- **Package dependencies** - Validates required tools before installation
- **Conflict resolution** - Adopts existing files or creates backups
- **Hooks system** - Pre/post installation actions
- **Batch operations** - Install/update multiple packages efficiently

### Available Make Targets
```bash
make help              # Show all available targets
make install           # Install all packages
make install-core      # Install core packages (stow, bash, git)
make install-macos     # Install macOS-specific packages
make install-linux     # Install Linux-specific packages
make uninstall         # Remove all packages
make restow            # Update all packages
make status            # Show package status
make validate          # Validate package structure
make test              # Test operations (dry run)
make clean             # Remove broken symlinks
make backup            # Create backup of existing files
```

## 📁 XDG Base Directory Support

This configuration automatically migrates your existing dotfiles to XDG-compliant locations:

- `~/.bash_history` → `$XDG_STATE_HOME/bash/history`
- `~/.gitconfig` → `$XDG_CONFIG_HOME/git/config`
- `~/.vimrc` → `$XDG_CONFIG_HOME/vim/vimrc`
- And many more...

### XDG Status
```bash
# Check XDG directory status and migrations
show_xdg_status
```

## 🗑️ Uninstall

### Complete Uninstall
```bash
cd ~/.dotfiles/dotfiles

# Remove all packages
make uninstall

# Or manually with stow
stow -D bash emacs git htop i3 iterm2 nano tmux
stow -D -t ~ stow

# Clean up broken symlinks
make clean

# Clean up XDG directories (optional)
rm -rf ~/.config/shell ~/.local/share/shell ~/.cache/shell ~/.local/state/shell
```

### Selective Uninstall
```bash
# Remove specific packages
make uninstall-bash uninstall-tmux

# Or with stow directly
stow --delete bash tmux
```

## More
See [Wiki](https://github.com/Sunhick/dotfiles/wiki) for installation & other information.

## 🐛 Troubleshooting

### Stow Issues
```bash
# Test before installing
make test
stow --simulate package_name

# Check for conflicts
make status
make validate

# Force adoption of existing files
stow --adopt package_name

# Clean up broken symlinks
make clean

# Create backup before changes
make backup
```

### Debug Mode
Enable debug logging to troubleshoot issues:
```bash
export DEBUG_MODE=true
export VERBOSE_MODE=true
source bash/core/loader.bash
```

### Check Error Logs
```bash
# View recent errors
show_recent_errors

# Clear error log
clear_error_log
```

### Common Issues

**Stow Conflicts**
- Run `make backup` before installation
- Use `stow --adopt` to adopt existing files
- Check `.stow-local-ignore` files for proper exclusions
- Validate package structure with `make validate`

**Module Loading Failures**
- Ensure Bash 5+ is installed: `bash --version`
- Check file permissions: `ls -la bash/core/`
- Verify file integrity: `bash -n bash/core/loader.bash`

**XDG Migration Issues**
- Check migration log: `cat ~/.local/state/shell/migrations.log`
- Verify directory permissions: `ls -la ~/.local/`
- Manual migration: `migrate_to_xdg "old_path" "new_path" "migration_name"`

**Package Dependencies**
- Check `.stow-dependencies` files in each package
- Install missing tools before stowing packages
- Use `make install-core` for essential packages first

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch: `git checkout -b feature/amazing-feature`
3. Follow the modular architecture patterns
4. Add tests for new functionality
5. Update documentation
6. Submit a pull request

### Adding New Modules

1. Create your module in the appropriate directory (`bash/features/`, `bash/core/`, etc.)
2. Register it in the loader:
```bash
register_module "my_feature" "features/my_feature.bash" "async" "command -v my_tool" "error_handler"
```
3. Follow error handling patterns using the provided functions

## 📚 Documentation

- [Wiki](https://github.com/Sunhick/dotfiles/wiki) - Detailed installation and configuration guides
- [XDG Base Directory Specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
- [Bash Manual](https://www.gnu.org/software/bash/manual/bash.html)

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- [XDG Base Directory Specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
- [Bash Best Practices](https://mywiki.wooledge.org/BashGuide)
- Community dotfiles repositories for inspiration

## 🔧 Stow Aliases and Shortcuts

For convenience, source the stow aliases:
```bash
source stow/.stow-aliases
```

This provides shortcuts like:
```bash
# Quick package operations
stow-pkg install bash     # Install bash package
stow-pkg remove tmux      # Remove tmux package
stow-pkg update git       # Update git package

# Batch operations
stow-install-all          # Install all packages
stow-remove-all           # Remove all packages
stow-status              # Show status of all packages

# Safety operations
stow-backup              # Create backup
stow-clean               # Clean broken symlinks
stow-validate            # Validate packages
```

## 📋 Quick Reference

| Command              | Description                   |
| -------------------- | ----------------------------- |
| `make install`       | Install all packages          |
| `make install-bash`  | Install specific package      |
| `make uninstall`     | Remove all packages           |
| `make status`        | Show package status           |
| `make test`          | Test operations (dry run)     |
| `make backup`        | Create backup                 |
| `make clean`         | Remove broken symlinks        |
| `stow bash`          | Install bash package directly |
| `stow --delete bash` | Remove bash package directly  |
| `stow --restow bash` | Update bash package directly  |

---

**Note**: This dotfiles repository supports both modern stow management and legacy installation methods for maximum flexibility.
