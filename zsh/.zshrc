#!/usr/bin/env zsh
# macOS Compatibility .zshrc
# This file provides compatibility for systems that expect ~/.zshrc
# while maintaining XDG Base Directory Specification compliance

# Set up XDG directories if not already set
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
export XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}"

# Set ZDOTDIR to use XDG config directory
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

# Source the XDG-compliant zshrc
if [[ -f "$ZDOTDIR/.zshrc" ]]; then
    source "$ZDOTDIR/.zshrc"
else
    echo "Warning: XDG-compliant zshrc not found at $ZDOTDIR/.zshrc" >&2
fi
