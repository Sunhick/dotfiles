#!/usr/bin/env bash
# macOS Compatibility .bashrc
# This file provides compatibility for systems that expect ~/.bashrc
# while maintaining XDG Base Directory Specification compliance

# Set up XDG directories if not already set
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
export XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}"

# Source the XDG-compliant bashrc
if [[ -f "${XDG_CONFIG_HOME}/bash/bashrc" ]]; then
    source "${XDG_CONFIG_HOME}/bash/bashrc"
else
    echo "Warning: XDG-compliant bashrc not found at ${XDG_CONFIG_HOME}/bash/bashrc" >&2
fi
