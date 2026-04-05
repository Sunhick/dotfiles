# Zsh Configuration

XDG-compliant zsh configuration with oh-my-zsh integration.

## Files

- `.zshenv` - Environment variables and XDG setup
- `.config/zsh/.zshrc` - Main zsh configuration

## XDG Compliance

- `ZDOTDIR` set to `~/.config/zsh/`
- Oh-my-zsh moved to `~/.local/share/oh-my-zsh/`
- XDG environment variables exported

## Features

- Oh-my-zsh integration with robbyrussell theme
- Zsh autosuggestions plugin
- Java environment setup
- Emacs as default editor
- FZF integration (XDG-compliant path)

## Installation Notes

After installing, you may need to:
1. Install oh-my-zsh to the XDG location: `~/.local/share/oh-my-zsh/`
2. Install zsh-autosuggestions plugin
3. Configure FZF to use XDG paths
