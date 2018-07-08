# Config files
* Clone the repository and make changes to bashrc as required. It won't be tracked.
* Emacs customizations
* https://github.com/leoliu/ggtags/wiki/Install-Global-with-support-for-exuberant-ctags

# Emacs keys
```M-``` Meta key (ALT/ESC)

```C-``` Control key (CTRL)

```S-``` Shift key

# Deploy
use GNU stow to install the configuration files.
```sh
$ brew install stow
$ git clone repository ~/dotfiles
$ cd ~/dotfiles
$ stow .
```

To remove/ uninstall
```sh
$ cd cd ~/dotfiles
$ stow -D .
```