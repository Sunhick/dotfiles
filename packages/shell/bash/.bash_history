#!/usr/bin/env bash

# History configuration
HISTSIZE=10000
HISTFILESIZE=20000
HISTFILE="${HOME}/.bash_history"

# Don't save duplicate commands or commands starting with space
HISTCONTROL=ignoreboth:erasedups

# Append to history file, don't overwrite
shopt -s histappend

# Save multi-line commands as single history entry
shopt -s cmdhist

# Save history after each command
PROMPT_COMMAND="history -a; ${PROMPT_COMMAND}"
