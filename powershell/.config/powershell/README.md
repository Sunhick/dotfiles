# PowerShell Modular Configuration

This directory contains modular PowerShell configuration files that are sourced by the main profile.

## File Structure

- **`environment.ps1`** - Environment variables, XDG directories, and PATH management
- **`prompt.ps1`** - Custom prompt configuration with colors and git integration
- **`aliases.ps1`** - Command aliases and utility functions
- **`history.ps1`** - History configuration and PSReadLine settings
- **`fzf.ps1`** - FZF integration for fuzzy finding

## Usage

The main profile (`Microsoft.PowerShell_profile.ps1`) automatically sources these files in order. Each file is self-contained and can be disabled by removing or renaming it.

## Customization

You can:
- Add new `.ps1` files to this directory and update the main profile to source them
- Modify individual files without affecting others
- Disable specific functionality by commenting out or removing files
- Override settings by creating additional files that load after the base configuration

## Dependencies

- **PSReadLine** - Enhanced command line editing (recommended)
- **fzf** - Fuzzy finder (optional, for enhanced search functionality)
- **git** - Version control (optional, for git branch display in prompt)
