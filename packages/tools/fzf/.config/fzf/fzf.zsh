#!/usr/bin/env zsh
# FZF configuration for zsh
# XDG Base Directory Specification compliant

# Set up XDG directories
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"

# Source the main fzf configuration
if [[ -f "${XDG_CONFIG_HOME}/fzf/config" ]]; then
    source "${XDG_CONFIG_HOME}/fzf/config"
fi

# Auto-completion and key bindings
if command -v fzf >/dev/null 2>&1; then
    # Try to source fzf completion from common locations
    for completion_file in \
        "/opt/homebrew/share/fzf/shell/completion.zsh" \
        "/usr/local/share/fzf/shell/completion.zsh" \
        "/usr/share/fzf/completion.zsh"; do
        if [[ -f "$completion_file" ]]; then
            source "$completion_file"
            break
        fi
    done

    # Try to source fzf key bindings from common locations
    for keybind_file in \
        "/opt/homebrew/share/fzf/shell/key-bindings.zsh" \
        "/usr/local/share/fzf/shell/key-bindings.zsh" \
        "/usr/share/fzf/key-bindings.zsh"; do
        if [[ -f "$keybind_file" ]]; then
            source "$keybind_file"
            break
        fi
    done
fi
