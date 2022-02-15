#!/usr/bin/env bash

eval DOTFILES_ROOT="~/.dotfiles/"

# include other sources
function include () {
    [[ -f "$1" ]] && source "$1"
}

if [ -f /.dockerenv ]; then
    # Inside the docker. Don't use host .bashrc if the home folder is mounted.
    include ${DOTFILES_ROOT}.dockerrc
    return
fi

# bash exports
include ${DOTFILES_ROOT}dotfiles/bash/.bash_export

# bash history configurations
include ${DOTFILES_ROOT}dotfiles/bash/.bash_history

# include other sources
include ~/.fzf.bash
include ${DOTFILES_ROOT}dotfiles/bash/.aliases

# Invoke host specific common script
pfile=$(echo $(uname) | tr '[:upper:]' '[:lower:]')
include ${DOTFILES_ROOT}dotfiles/bash/.${pfile}

# Include any local overrides specific customizations per host
include ${DOTFILES_ROOT}.${pfile}

# provide custom bash prompt
include ${DOTFILES_ROOT}dotfiles/bash/.bash_prompt
