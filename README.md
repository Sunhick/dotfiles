# Dotfiles

Personal dotfiles managed with GNU Stow, organized by category for easy installation and maintenance.

## Quick Start

```bash
git clone https://github.com/yourusername/dotfiles.git ~/.dotfiles
cd ~/.dotfiles
make install
```

## Structure

```
dotfiles/
├── packages/
│   ├── shell/          # bash, zsh
│   ├── editors/        # emacs, nano, vscode
│   ├── tools/          # tmux, htop, gnupg
│   ├── desktop/        # i3, iterm2
│   └── development/    # git
├── management/stow/    # stow configuration
└── Makefile           # installation commands
```

## Installation

### Install Everything
```bash
make install
```

### Install by Category
```bash
make install-shell      # bash, zsh
make install-editors    # emacs, nano, vscode
make install-tools      # tmux, htop, gnupg
make install-desktop    # i3, iterm2
make install-dev        # git
```

### Install Individual Packages
```bash
cd packages/shell
make install-bash

# or directly with stow
stow bash
```

## Commands

| Command          | Description               |
| ---------------- | ------------------------- |
| `make install`   | Install all packages      |
| `make uninstall` | Remove all packages       |
| `make status`    | Show installation status  |
| `make test`      | Preview changes (dry run) |
| `make clean`     | Remove broken symlinks    |
| `make list`      | List available packages   |

## Adding Packages

1. Create directory in appropriate category:
   ```bash
   mkdir packages/tools/mynewpackage
   ```

2. Add your dotfiles:
   ```bash
   packages/tools/mynewpackage/
   └── .config/
       └── mynewpackage/
           └── config.yml
   ```

3. Install:
   ```bash
   cd packages/tools
   make install-mynewpackage
   ```

## Requirements

- [GNU Stow](https://www.gnu.org/software/stow/)
- Git

## License

MIT
