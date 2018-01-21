#######################################################
#
# Author: Sunil 
#
#######################################################

export CONF_VERSION=1.0

# gradle
# export GRADLE_PATH=/Users/Sunny/test/gradle-4.4.1/bin
# export PATH=$GRADLE_PATH:$PATH

# Add java path
export JAVA_HOME=$(/usr/libexec/java_home)
export PYTHONPATH=/Users/Sunny/prv/tmp/caffe/python:$PYTHONPATH

# Emacs v25 path
export PATH=/Applications/Emacs.app/Contents/MacOS:$PATH

# # export Android home
export AndroidHome=~/Library/Android/sdk/platform-tools/
export PATH=$AndroidHome:$PATH

# # chromium depot_tools
# export PATH=$PATH:/Users/Sunny/prv/google/depot_tools

# gtag label is required for cscope to work magically.
export GTAGSLABEL=ctags

# # change bazel cache directory 
# export TEST_TMPDIR=~/prv/bazel-cache

# # go 
# export GOROOT=$HOME/prv/go/go-1.7.1
# export GOPATH=$HOME/prv/go/packages
# export PATH=$PATH:$GOROOT/bin

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

# # Chrmoium alias for compiling and running
# alias cchrome="ninja -C out/Default chrome:chrome_app_executable"
# alias rchrome="out/Default/Chromium.app/Contents/MacOS/Chromium"

# load git auto completion file.
source ~/.git-complete