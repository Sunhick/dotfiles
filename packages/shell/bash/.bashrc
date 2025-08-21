#!/usr/bin/env bash

# Get the directory where this .bashrc is located
BASH_CONFIG_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Safe source function with error handling
include() {
    if [[ -f "$1" ]]; then
        source "$1"
    elif [[ "${2:-}" != "optional" ]]; then
        echo "Warning: Could not source $1" >&2
    fi
}

# Load shell options first
include "${BASH_CONFIG_DIR}/.shopt"

# Load environment variables
include "${BASH_CONFIG_DIR}/.bash_export"

# Load history configuration
include "${BASH_CONFIG_DIR}/.bash_history"

# Load colors and prompt
include "${BASH_CONFIG_DIR}/.colors"
include "${BASH_CONFIG_DIR}/.bash_prompt"

# Load aliases
include "${BASH_CONFIG_DIR}/.aliases"

# Load OS-specific configuration
case "$(uname -s)" in
    Darwin)
        include "${BASH_CONFIG_DIR}/.darwin"
        ;;
    Linux)
        include "${BASH_CONFIG_DIR}/.linux"
        ;;
esac

# Load fuzzy finder if available
include "${HOME}/.fzf.bash" "optional"

# Load local overrides if they exist
include "${HOME}/.bashrc.local" "optional"
