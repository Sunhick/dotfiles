#! /usr/bin/env bash

STABLE_RELEASE="v1.3"
DEV_RELEASE=

git_download() {
    git clone https://github.com/Sunhick/dotfiles.git ~/.dotfiles/dotfiles
    git checkout ${STABLE_RELEASE}
}

wget_download() {
    wget https://github.com/Sunhick/dotfiles/archive/${STABLE_RELEASE}.tar.gz
}

curl_download() {
    # figure out curl CLI for dowloading
}

download() {

}

install() {
    # install the dotfiles using stow
}

download && install
