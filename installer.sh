#! /usr/bin/env bash

# Copyright (c) 2019-2020 Sunil
# Author: Sunil <sunhick@gmail.com>

# This file is not part of GNU Emacs.

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs; see the file COPYING.  If not, write to the
# Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.

set -o errexit
set -o nounset
set -o pipefail

STABLE_RELEASE="v1.6"
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
