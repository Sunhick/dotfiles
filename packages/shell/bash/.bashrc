#!/usr/bin/env bash

# Define the root of your dotfiles
DOTFILES_ROOT="${HOME}/.dotfiles/"

# Safe source function
include() {
    [[ -f "$1" ]] && source "$1"
}

# Detect Docker environment
if [[ -f "/.dockerenv" ]]; then
    # Inside Docker – use container-specific rc
    include "${DOTFILES_ROOT}.dockerrc"
    return
fi

# Include base configurations
include "${DOTFILES_ROOT}dotfiles/bash/.bash_export"
include "${DOTFILES_ROOT}dotfiles/bash/.bash_history"
include "${DOTFILES_ROOT}dotfiles/bash/.aliases"
include "${DOTFILES_ROOT}dotfiles/bash/.bash_prompt"

# Include fuzzy finder if installed
include "${HOME}/.fzf.bash"

# Load OS-specific configuration
os_name="$(uname | tr '[:upper:]' '[:lower:]')"
include "${DOTFILES_ROOT}dotfiles/bash/.${os_name}"

# Load local machine-specific overrides
include "${DOTFILES_ROOT}.${os_name}"
