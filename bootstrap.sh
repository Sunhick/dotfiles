#!/bin/sh
# 
# !!! Note: This script will overwrite your preferences !!!
# 

VERSION="1.0"
function version() {
    echo "config current version" $VERSION
}

function help() {
    echo "usage of bootstrap.sh."
    echo " -i install the configurations."
    echo " -u uninstall the configurations."
    echo " -v version number of current configuration."
    echo " -h print the help"
}

############################################
#
# Install the configurations. 
#
############################################
function install() {
    # create emacs root folder if it doesn't exists.
    mkdir -p $HOME/.emacs.d/

    # create a symbolic links for emacs configurations
    ln -sf $(pwd)/emacs.d/init.el $HOME/.emacs.d/
    ln -sf $(pwd)/emacs.d/settings.org $HOME/.emacs.d/

    # create symlinks for bash_profile settings
    ln -sf $(pwd)/bash_profile $HOME/.bash_profile
    ln -sf $(pwd)/git-complete.sh $HOME/.git-complete
    echo "Configuration settings installed!"
}

############################################
#
# Will take back up of existing configuration
# before modifying.
#
############################################
function backup() {
    echo "Backed up ~/backup.<date>.<time>"
}

############################################
#
# Uninstall the configurations
#
############################################
function uninstall() {
    rm -rf $HOME/.emacs.d/
    rm -rf $HOME/.bash_profile
    rm -rf $HOME/.git-complete
    echo "Configuration settings uninstalled!"
}

opt=$1
case $opt in
    -h | --help) help ;;
    -v | --version) version ;;
    -i | --install) install ;;
    -u | --uninstall) uninstall ;;
    *) echo "Incorrect argument. Exiting..."
    exit
esac
