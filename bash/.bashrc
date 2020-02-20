# include other sources
function include () {
    [[ -f "$1" ]] && source "$1"
}

# Add java path
export JAVA_HOME=$(/usr/libexec/java_home)
export EDITOR="emacs"

# include other sources
include ~/.fzf.bash

# source machine specific customizations
include ~/.dotfiles/.localrc
