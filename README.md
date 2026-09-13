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
# Install everything from the bundled Brewfile
brew bundle --file=homebrew/Brewfile

# ...or install the essentials manually
brew install coreutils fzf git      # core enhancements
brew install bat eza fd ripgrep     # modern CLI tools
brew install --cask iterm2          # terminal
brew install emacs                  # editor
```

**Linux (Ubuntu/Debian):**
```bash
# Install from the bundled package list
xargs sudo apt install -y < apt/package-list.txt

# ...or install the essentials manually
sudo apt install git fzf coreutils  # core tools
sudo apt install bat fd-find ripgrep # modern CLI tools
```

**Windows (PowerShell):**
```powershell
# Bootstrap PowerShell profile + tools
./powershell/Setup.ps1
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

The repo has two kinds of top-level directories: **Stow packages** (symlinked
into your home directory by `make`) and **helper directories** (package lists,
keyboard layouts, and shared Makefile logic that are *not* stowed).

```
dotfiles/
│
├── Stow packages (linked by `make`) ──────────────
│   ├── bash/            # bash shell        → ~/.config/bash/ (+ ~/.bashrc, ~/.bash_profile)
│   ├── git/             # git               → ~/.config/git/
│   ├── emacs/           # emacs             → ~/.config/emacs/
│   ├── nano/            # nano              → ~/.config/nano/
│   ├── vscode/          # VS Code           → ~/.config/Code/ (+ ~/Library/… on macOS)
│   ├── tmux/            # tmux              → ~/.config/tmux/
│   ├── fzf/             # fzf               → ~/.config/fzf/
│   ├── ripgrep/         # ripgrep           → ~/.config/ripgrep/
│   ├── inputrc/         # readline          → ~/.config/readline/
│   ├── curlrc/          # curl              → ~/.config/curl/
│   ├── editorconfig/    # editorconfig      → ~/.editorconfig
│   ├── gnupg/           # GnuPG             → ~/.config/gnupg/
│   ├── htop/            # htop              → ~/.config/htop/
│   ├── i3/              # i3 wm (Linux)     → ~/.config/i3/
│   └── terminal/        # terminal / iterm2 config
│
├── Windows (not stowed; use Setup.ps1) ───────────
│   ├── powershell/      # PowerShell profile + Setup.ps1
│   └── windows-terminal/# Windows Terminal settings.json
│
└── Helper directories (not stowed) ───────────────
    ├── apt/             # Debian/Ubuntu package lists (package-list.txt, apt-packages)
    ├── homebrew/        # macOS Brewfile
    ├── keyboard/        # QMK/Vial homerow-miryoku keyboard layouts
    ├── make/            # shared Makefile helpers (log.mk)
    ├── management/stow/ # stow configuration
    └── Makefile         # one Makefile for everything
```

> **Note:** `powershell/` and `windows-terminal/` are managed on Windows via
> `powershell/Setup.ps1`, not through the `make` targets below (which target
> macOS/Linux and GNU Stow).

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
- `linux` - Linux-specific config
- `aliases` - Smart aliases that adapt to available tools
- `prompt` - Git-aware prompt

## Installation

### Install Everything
```bash
make install
```

### Install by Category
```bash
make shell              # bash
make editors            # emacs, nano, vscode
make tools              # tmux, fzf, ripgrep, inputrc, curlrc, editorconfig, gnupg, htop
make desktop            # i3, terminal
make development        # git
```

### Install Individual Packages
```bash
make tmux
make git
make bash

# or directly with stow
cd tmux
stow --target=$HOME .
```

## Commands

| Command              | Description                               |
| -------------------- | ----------------------------------------- |
| `make install`       | Install all packages                      |
| `make safe-install`  | Backup existing configs then install      |
| `make adopt`         | Install all, adopting existing files      |
| `make rm-<package>`  | Remove a package (e.g. `make rm-tmux`)    |
| `make plan`          | Preview changes (dry run)                 |
| `make sync`          | Re-link all packages                      |
| `make ls`            | List available packages                   |
| `make doctor`        | Verify tools, symlinks, and config health |
| `make prune`         | Remove broken symlinks (under ~/.config)  |
| `make backup`        | Backup existing configuration files       |
| `make restore`       | Restore from latest backup                |
| `make list-backups`  | List available backups                    |
| `make prune-backups` | Remove old backups (keeps last 5)         |

## Adding Packages

1. Create a directory at the repo root:
   ```bash
   mkdir mynewpackage
   ```

2. Add your dotfiles following XDG structure:
   ```bash
   mynewpackage/
   └── .config/
       └── mynewpackage/
           └── config.yml
   ```

3. Add it to `ALL_PKGS` in the Makefile and the appropriate group variable.

4. Install:
   ```bash
   make mynewpackage
   ```

### XDG Guidelines for New Packages
- Configuration files → `.config/appname/`
- Data files → `.local/share/appname/`
- Cache files → `.cache/appname/`
- State files → `.local/state/appname/`

## Package Management Bundles

Beyond the stowed configs, the repo bundles reproducible package lists so a
fresh machine can be provisioned quickly:

- **`homebrew/Brewfile`** — macOS packages/casks. Install with `brew bundle --file=homebrew/Brewfile`.
- **`apt/package-list.txt`** — Debian/Ubuntu packages. Install with `xargs sudo apt install -y < apt/package-list.txt`.
- **`keyboard/`** — QMK/Vial homerow Miryoku layouts (`.vil`), including a Colemak variant.

## Color Theming

The dotfiles use a **Gruvbox Dark Hard** color scheme throughout:

### Terminal Colors (bash)
- **GNU dir_colors** - Full gruvbox theme when coreutils installed
- **BSD LSCOLORS** - Gruvbox-inspired fallback for macOS
- **Smart detection** - Automatically chooses best color system

### Color Priority
1. **gls --color=auto** (GNU coreutils) - Full gruvbox dir_colors
2. **ls -G** (BSD) - Gruvbox LSCOLORS on macOS
3. **eza** - Modern ls with built-in colors

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
