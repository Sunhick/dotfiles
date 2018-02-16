#!/bin/sh
# 
# !!! Note: This script will overwrite your preferences !!!
# 

VERSION="1.0"
function version() {
    echo "config current version" $VERSION
}

function help() {
    echo "usage of install.sh."
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
    ln -sf $(pwd)/emacs.d/personal $HOME/.emacs.d/
    ln -sf $(pwd)/emacs.d/vendor $HOME/.emacs.d/

    # create symlinks for bash_profile settings
    ln -sf $(pwd)/bash_profile $HOME/.bash_profile
    ln -sf $(pwd)/git-complete.sh $HOME/.git-complete

    if ! grep -Fxq "source $(pwd)/bashrc" $HOME/.bashrc
    then 
        echo "\n# Invoke the bashrc from github config project." >> $HOME/.bashrc
        echo "# Use config/bashrc to modify. " >> $HOME/.bashrc
        echo "source " $(pwd)/bashrc >> $HOME/.bashrc
    fi

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
    rm -rf $HOME/.git-complete
    lineno=`grep -xFn "source  $(pwd)/bashrc" $HOME/.bashrc | sed -n 's/^\([0-9]*\)[:].*/\1/p'`
    sed -i -e "$lineno d" $HOME/.bashrc
    echo "Configuration settings uninstalled!"
}

opt=$1
case $opt in
    -h | --help)
	help
	;;
    
    -v | --version)
	version
	;;
    
    -i | --install)
	install
	;;
    
    -u | --uninstall)
	uninstall
	;;
    
    *)
	echo "Incorrect argument."
	help
	exit
esac
