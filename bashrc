#######################################################
#
# Author: Sunil 
#
#######################################################
export CONF_VERSION=1.0

# gtag label is required for cscope to work magically.
export GTAGSLABEL=ctags

# Set the default editor terminal
export EDITOR=emacs

alias ..="cd .."
alias ...="cd ../.."
alias .3="cd ../../.."
alias .4="cd ../../../.."
alias .5="cd ../../../../.."

# No more emacs color invert(-r) mode. I'm using customize-themes
# to set the dark mode emacs. I'm setting the geometry here and
# not in .emacs because if i set in .emacs file it will first
# open emacs in default size and then resizes it.
alias emacs="Emacs -geometry 200x60"

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
