#!/usr/bin/env bash

# Define the root of your dotfiles repository
DOTFILES_ROOT="${HOME}/.dotfiles/"

# Safe source function
include() {
    if [[ -f "$1" ]]; then
        source "$1"
    elif [[ "${2:-}" != "optional" ]]; then
        echo "Warning: Could not source $1" >&2
    fi
}

# Detect Docker environment
if [[ -f "/.dockerenv" ]]; then
    # Inside Docker – use container-specific rc
    include "${DOTFILES_ROOT}.dockerrc" "optional"
    return
fi

# Load shell options first
include "${DOTFILES_ROOT}packages/shell/bash/.shopt"

# Load environment variables
include "${DOTFILES_ROOT}packages/shell/bash/.bash_export"

# Load history configuration
include "${DOTFILES_ROOT}packages/shell/bash/.bash_history"

# Load colors and prompt
include "${DOTFILES_ROOT}packages/shell/bash/.colors"
include "${DOTFILES_ROOT}packages/shell/bash/.bash_prompt"

# Load aliases
include "${DOTFILES_ROOT}packages/shell/bash/.aliases"

# Load OS-specific configuration
case "$(uname -s)" in
    Darwin)
        include "${DOTFILES_ROOT}packages/shell/bash/.darwin"
        ;;
    Linux)
        include "${DOTFILES_ROOT}packages/shell/bash/.linux"
        ;;
esac

# Load fuzzy finder if available
include "${HOME}/.fzf.bash" "optional"

# Load local machine-specific overrides
os_name="$(uname -s | tr '[:upper:]' '[:lower:]')"
include "${DOTFILES_ROOT}.${os_name}" "optional"
