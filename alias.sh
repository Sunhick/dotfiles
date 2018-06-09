# gtag label is required for cscope to work magically.
export GTAGSLABEL=ctags

alias ..="cd .."
alias ...="cd ../.."
alias .3="cd ../../.."
alias .4="cd ../../../.."
alias .5="cd ../../../../.."

# weather forecast
weather() {
    location="$1"
    if [ -z "$location" ]; then
        location="seattle"
    fi

    curl http://wttr.in/$location?lang=en
}
