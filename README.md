# Modern Dotfiles

A modernized, modular dotfiles configuration with intelligent loading, XDG Base Directory compliance, and cross-platform support.

## ✨ Features

- **🚀 Intelligent Module Loading**: Async loading with dependency resolution and feature detection
- **📁 XDG Base Directory Compliant**: Clean home directory organization following XDG standards
- **🛡️ Enhanced Error Handling**: Comprehensive logging and graceful degradation
- **🔧 Cross-Platform**: Works seamlessly on macOS and Linux
- **⚡ Performance Optimized**: Conditional loading based on available tools
- **🧩 Modular Architecture**: Easy to extend and customize

## 🏗️ Architecture

```
bash/
├── core/                    # Core infrastructure
│   ├── loader.bash         # Module loading system
│   ├── error_handler.bash  # Error handling & validation
│   ├── history.bash        # History management
│   └── navigation.bash     # Directory navigation
├── features/               # Feature modules
│   ├── xdg.bash           # XDG Base Directory support
│   ├── git.bash           # Git enhancements
│   └── performance.bash   # Performance optimizations
├── themes/                # Visual themes
└── platform/             # Platform-specific configs
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

### Manual Installation
```bash
# Source the core infrastructure
source bash/core/loader.bash

# Load specific modules
load_module "xdg"
load_module "git"
load_module "navigation"
```

## 🔧 Configuration

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

### Module Management
```bash
# List all modules and their status
list_modules

# Check module status
module_status "git"

# Load deferred modules when needed
load_deferred_modules
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
```bash
cd ~/.dotfiles/dotfiles
stow -D bash emacs git htop i3 iterm2 nano tmux
stow -D -t ~ stow

# Clean up XDG directories (optional)
rm -rf ~/.config/shell ~/.local/share/shell ~/.cache/shell ~/.local/state/shell
```

## More
See [Wiki](https://github.com/Sunhick/dotfiles/wiki) for installation & other information.

## 🐛 Troubleshooting

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

**Module Loading Failures**
- Ensure Bash 5+ is installed: `bash --version`
- Check file permissions: `ls -la bash/core/`
- Verify file integrity: `bash -n bash/core/loader.bash`

**XDG Migration Issues**
- Check migration log: `cat ~/.local/state/shell/migrations.log`
- Verify directory permissions: `ls -la ~/.local/`
- Manual migration: `migrate_to_xdg "old_path" "new_path" "migration_name"`

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

---

**Note**: This is a modernized version of the original dotfiles. The legacy installation method using stow is still supported for backward compatibility.
