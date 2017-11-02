#######################################################
#
# Author: Sunil 
#
#######################################################

# Add java path
export JAVA_HOME=$(/usr/libexec/java_home)
export PYTHONPATH=/Users/Sunny/prv/tmp/caffe/python:$PYTHONPATH

# Emacs v25 path
export PATH=/Applications/Emacs.app/Contents/MacOS:$PATH

alias ..="cd .."
alias ...="cd ../.."
alias ..3="cd ../../.."
alias ..4="cd ../../../.."
alias ..5="cd ../../../../.."

# No more emacs color invert(-r) mode. I'm using customize-themes
# to set the dark mode emacs. I'm setting the geometry here and
# not in .emacs because if i set in .emacs file it will first
# open emacs in default size and then resizes it.
alias emacs="Emacs -geometry 160x60"

# export Android home
export AndroidHome=~/Library/Android/sdk/platform-tools/
export PATH=$AndroidHome:$PATH

# Google depot tools
export PATH=$PATH:/Users/Sunny/prv/google/depot_tools/

# gtag label is required for cscope to work magically.
export GTAGSLABEL=ctags

# load git auto completion file.
source ~/.git-complete
