#!/bin/sh

# create a symbolic links for emacs configurations
ln -sf $(pwd)/emacs.d/init.el $HOME/.emacs.d/
ln -sf $(pwd)/emacs.d/settings.org $HOME/.emacs.d/