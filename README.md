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

$ cd ~/.dotfiles
$ touch zshrc.local         # for local machine changes

$ cd ~/.dotfiles/dotfiles

# Install what you want. By default installs all.
$ stow emacs zsh tmux nano git -t ~/
```

## Uninstall
```sh
$ cd ~/.dotfiles/dotfiles
$ stow -D emacs zsh tmux nano config -t ~/
```

Or if you want to uninstall completely then run,
````sh
$ rm -rf ~/.dotfiles/dotfiles .emacs.d .zshrc .tmux.conf .nano .config
````

## More
See [Wiki](https://github.com/Sunhick/dotfiles/wiki) for installation & other information.
