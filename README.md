## Welcome to Dotfiles

### Pre-requisities
* [fzf fuzzy finder](https://github.com/junegunn/fzf)
* [oh-my-zsh](https://github.com/robbyrussell/oh-my-zsh)
  * Plugins: auto-suggesstions, git
* [Emacs for macosx](https://emacsformacosx.com)
* [Tmux](https://github.com/tmux/tmux)
* [stow](https://www.gnu.org/software/stow/manual/stow.html)

### Installation (MacOS)
Download and install the latest stable version of [emacs](https://emacsformacosx.com/).


use GNU stow to install the configuration files.
```sh
$ brew install stow
$ git clone --recursive git@github.com:Sunhick/dotfiles.git ~/.dotfiles/dotfiles

$ cd ~/.dotfiles
$ touch zshrc.local         # for local machine changes

$ cd ~/.dotfiles/dotfiles
$ stow emacs zsh tmux
```

To remove/ uninstall
```sh
$ cd ~/.dotfiles
$ stow -D emacs zsh tmux
```

See [Wiki](https://github.com/Sunhick/dotfiles/wiki) for installation & other information.

checkout the [screenshots](https://github.com/Sunhick/dotfiles/wiki/screenshots)
