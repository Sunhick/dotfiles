# PowerShell Configuration

This package provides a modular PowerShell profile that replicates bash-like functionality and integrates with the XDG Base Directory Specification on Windows.

## Features

- **Modular configuration** - Split into logical components for easy customization
- **XDG Base Directory support** - Windows-equivalent implementation
- **Bash-style aliases** - Navigation shortcuts (`.`, `..`, `...`, etc.)
- **Git integration** - Branch display in prompt and git shortcuts
- **Modern tool replacements** - Support for bat, fd, ripgrep, fzf
- **Enhanced history** - PSReadLine integration with Emacs-style bindings
- **Colorized prompt** - Status indicators and AWS profile display
- **Cross-platform consistency** - Matches bash configuration behavior

## Modular Structure

The configuration is split into focused modules:

```
.config/powershell/
├── environment.ps1   # Environment variables, XDG dirs, PATH
├── prompt.ps1        # Custom prompt with colors and git
├── aliases.ps1       # Command aliases and utility functions
├── history.ps1       # History configuration and PSReadLine
├── fzf.ps1          # FZF integration for fuzzy finding
└── README.md        # Module documentation
```

## Installation

### Using Setup Script (Recommended)

Run the PowerShell setup script for interactive installation:

```powershell
# Basic installation
.\setup-powershell-profile.ps1

# Install with recommended modules
.\setup-powershell-profile.ps1 -InstallModules

# Force overwrite existing profile
.\setup-powershell-profile.ps1 -Force
```

### Manual Installation

1. Copy `Microsoft.PowerShell_profile.ps1` to your PowerShell profile location:
   ```powershell
   $PROFILE  # Shows the expected location
   ```

2. Reload your profile:
   ```powershell
   . $PROFILE
   ```

## Recommended Modules

The profile works best with these PowerShell modules:

- **PSReadLine** - Enhanced command line editing
- **posh-git** - Git integration for PowerShell
- **Terminal-Icons** - File and folder icons in terminal

Install them with:
```powershell
Install-Module PSReadLine, posh-git, Terminal-Icons -Scope CurrentUser
```

## Recommended CLI Tools

For the best experience, install these modern CLI tools:

```powershell
# Using winget
winget install sharkdp.bat          # Better cat
winget install sharkdp.fd           # Better find
winget install BurntSushi.ripgrep.MSVC  # Better grep
winget install junegunn.fzf         # Fuzzy finder
```

## Aliases and Functions

### Navigation
- `..`, `...`, `....`, `.....` - Navigate up directories
- `~` - Go to home directory
- `-` - Go to previous directory

### Listing
- `l`, `la`, `ll`, `lsa` - Various ls equivalents

### Git Shortcuts
- `g` - git
- `gs` - git status
- `ga` - git add
- `gc` - git commit
- `gp` - git push
- `gl` - git pull
- `gd` - git diff
- `gb` - git branch
- `gco` - git checkout

### Utilities
- `h` - Get-History
- `j` - Get-Job
- `c` - Clear-Host
- `reload` - Reload profile

## XDG Directory Structure

The profile creates these directories for organized configuration:

```
%USERPROFILE%\
├── .config\          # XDG_CONFIG_HOME
├── .local\
│   ├── share\        # XDG_DATA_HOME
│   └── state\        # XDG_STATE_HOME
└── .cache\           # XDG_CACHE_HOME
```

## Customization

The modular structure makes customization easy:

- **Environment** - Edit `.config/powershell/environment.ps1` for variables and PATH
- **Colors & Prompt** - Modify `.config/powershell/prompt.ps1` for appearance
- **Aliases** - Add functions to `.config/powershell/aliases.ps1`
- **History** - Adjust settings in `.config/powershell/history.ps1`
- **FZF** - Configure fuzzy finding in `.config/powershell/fzf.ps1`
- **Disable features** - Remove or rename module files
- **Add modules** - Create new `.ps1` files and update the main profile

## Compatibility

- **PowerShell 5.0+** - Required minimum version
- **Windows Terminal** - Recommended for best experience
- **Nerd Fonts** - For proper icon display with Terminal-Icons
