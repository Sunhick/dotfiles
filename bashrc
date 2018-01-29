#######################################################
#
# Author: Sunil 
#
# Keep this file contents pretty generic so that it will
# work on all systems. This shouldn't override the contents
# already defined in $HOME/.bashrc. This will be invoked
# after $HOME/.bashrc. .bashrc will point to this file.
# 
# $HOME/.bashrc -> $(pwd)/bashrc
#
#######################################################
export CONF_VERSION=1.0

# gtag label is required for cscope to work magically.
export GTAGSLABEL=ctags

alias ..="cd .."
alias ...="cd ../.."
alias .3="cd ../../.."
alias .4="cd ../../../.."
alias .5="cd ../../../../.."

# load git auto completion file.
source ~/.git-complete

# weather forecast
weather() {
    location="$1"
    if [ -z "$location" ]; then
        location="seattle"
    fi

    curl http://wttr.in/$location?lang=en
}
