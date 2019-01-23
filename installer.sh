#! /usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

STABLE_RELEASE="v1.3"
DEV_RELEASE=
SRC_PATH=${HOME}

COMPLETE_INSTALL="emacs zsh tmux git nano iterm2"
MINIMALISTIC_INSTALL="emacs zsh tmux"

git_download() {
    (
        local srcDir=${SRC_PATH}/.dotfiles/dotfiles
        mkdir -p ${srcDir}
        cd ${srcDir}
        git clone https://github.com/Sunhick/dotfiles.git .
        git checkout tags/${STABLE_RELEASE}
    )
}

wget_download() {
    echo "Not supported yet"
    # wget https://github.com/Sunhick/dotfiles/archive/${STABLE_RELEASE}.tar.gz -o ${SRC_PATH}/.dotfiles/dotfiles
}

curl_download() {
    # figure out curl CLI for dowloading
    echo "Not supported yet"
}

download() {
    # For now let's just assume that user has installed git.
    # wget and curl to be implemented.
    git_download
}

install() {
    # install the dotfiles using stow
    (
        cd ${SRC_PATH}/.dotfiles/dotfiles
        stow ${MINIMALISTIC_INSTALL} --target ${HOME}
    )
}

download && install
