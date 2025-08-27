# PowerShell Configuration

This package provides a PowerShell profile that replicates bash-like functionality and integrates with the XDG Base Directory Specification on Windows.

## Features

- **XDG Base Directory support** - Windows-equivalent implementation
- **Bash-style aliases** - Navigation shortcuts (`.`, `..`, `...`, etc.)
- **Git integration** - Branch display in prompt and git shortcuts
- **Modern tool replacements** - Support for bat, fd, ripgrep, fzf
- **Enhanced history** - PSReadLine integration with Emacs-style bindings
- **Colorized prompt** - Status indicators and AWS profile display
- **Cross-platform consistency** - Matches bash configuration behavior

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

Edit the profile file to customize:

- **Colors** - Modify the `$Colors` hashtable
- **Prompt** - Update the `prompt` function
- **Aliases** - Add your own functions and aliases
- **Editor** - Change `$env:EDITOR` to your preferred editor

## Compatibility

- **PowerShell 5.0+** - Required minimum version
- **Windows Terminal** - Recommended for best experience
- **Nerd Fonts** - For proper icon display with Terminal-Icons
