# XDG-Compliant Dotfiles

Personal dotfiles managed with GNU Stow, following the XDG Base Directory Specification for clean and organized configuration management.

## Quick Start

```bash
git clone https://github.com/yourusername/dotfiles.git ~/.dotfiles
cd ~/.dotfiles
make install
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
├── packages/
│   ├── shell/          # bash (.config/bash/), zsh (.config/zsh/)
│   ├── editors/        # emacs (.config/emacs/), nano (.config/nano/), vscode (.config/Code/)
│   ├── tools/          # tmux (.config/tmux/), htop (.config/htop/), gnupg (.gnupg/)
│   ├── desktop/        # i3 (.config/i3/), iterm2 (.config/)
│   └── development/    # git (.config/git/)
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
   make install-mynewpackage
   ```

### XDG Guidelines for New Packages
- Configuration files → `.config/appname/`
- Data files → `.local/share/appname/`
- Cache files → `.cache/appname/`
- State files → `.local/state/appname/`

## Requirements

- [GNU Stow](https://www.gnu.org/software/stow/)
- Git

## License

MIT
