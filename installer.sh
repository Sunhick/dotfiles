#!/usr/bin/env bash
#
# Copyright (C) 2018-2020 Sunhick.
#
# License: GPLv3
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
* POSSIBILITY OF SUCH DAMAGE.

#
# See https://github.com/Sunhick/dotfiles/wiki/Installation for help.
#

#
# Configurations
#
DOTFILES_REPO="https://github.com/Sunhick/dotfiles.git"
DOTFILES_HOME="$HOME/.dotfiles/dotfiles"
PACKAGES="bash emacs git htop i3 iterm2 nano tmux stow zsh"

#
# OS detection
#
get_os() {
    case "$(uname -s)" in
        Darwin)
            echo "macOS"
            ;;
        Linux)
            echo "Linux"
            ;;
        *)
            echo "Unsupported OS"
            exit 1
            ;;
    esac
}

#
# Dependency installation
#
install_dependencies() {
    local os
    os=$(get_os)

    echo "Detected OS: $os"
    echo "Installing dependencies..."

    if ! command -v stow &> /dev/null; then
        echo "stow could not be found, installing..."
        if [ "$os" == "macOS" ]; then
            if ! command -v brew &> /dev/null; then
                echo "Homebrew not found. Please install Homebrew first."
                exit 1
            fi
            brew install stow
        elif [ "$os" == "Linux" ]; then
            if command -v apt-get &> /dev/null; then
                sudo apt-get update
                sudo apt-get install -y stow
            elif command -v yum &> /dev/null; then
                sudo yum install -y stow
            else
                echo "Unsupported Linux distribution. Please install stow manually."
                exit 1
            fi
        fi
    else
        echo "stow is already installed."
    fi
}

#
# Download dotfiles
#
git_download() {
    if [ -d "$DOTFILES_HOME" ]; then
        echo "Dotfiles is already installed"
        return
    fi

    echo "Cloning dotfiles repository..."
    git clone --recursive "$DOTFILES_REPO" "$DOTFILES_HOME"
}

download() {
    git_download
}

#
# Install dotfiles
#
install() {
    cd "$DOTFILES_HOME" || exit
    echo "Stowing packages..."
    stow -v --target="$HOME" $PACKAGES
}

#
# Main
#
install_dependencies
download
install