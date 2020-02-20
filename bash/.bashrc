#!/usr/bin/env bash

# include other sources
function include () {
    [[ -f "$1" ]] && source "$1"
}

# Add java path
export EDITOR="emacs"

# include other sources
include ~/.fzf.bash
include ~/.dotfiles/dotfiles/bash/.aliases

# Invoke host specific common script
pfile=$(echo $(uname) | tr '[:upper:]' '[:lower:]')
include ~/.dotfiles/dotfiles/bash/.${pfile}

# Include any local overrides specific customizations per host
include ~/.dotfiles/.${pfile}
