# Welcome to Dotfiles

## Pre-requisities
* [fzf fuzzy finder](https://github.com/junegunn/fzf)
* [oh-my-zsh](https://github.com/robbyrussell/oh-my-zsh)
  * Plugins: auto-suggesstions, git
* [Emacs for macosx](https://emacsformacosx.com)
* [Tmux](https://github.com/tmux/tmux)
* [stow](https://www.gnu.org/software/stow/manual/stow.html)
* [i3 -- Linux only](https://i3wm.org)
* [rg -- ripgrep](https://github.com/BurntSushi/ripgrep)

## Installation

### One-liner:

```shell
$ wget -O - https://raw.githubusercontent.com/Sunhick/dotfiles/installer/installer.sh | bash
```

### Git lovers'
use GNU stow to install the configuration files.
```sh
$ brew install stow
$ git clone --recursive https://github.com/Sunhick/dotfiles.git ~/.dotfiles/dotfiles

$ cd ~/.dotfiles/dotfiles

$ stow -t ~ stow
$ stow bash emacs git htop i3 iterm2 nano tmux
```

## Uninstall
```sh
$ cd ~/.dotfiles/dotfiles
$ stow -D bash emacs git htop i3 iterm2 nano tmux
$ stow -D -t ~ stow
```

## More
See [Wiki](https://github.com/Sunhick/dotfiles/wiki) for installation & other information.
