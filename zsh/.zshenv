# ~/.zshenv -- read by every zsh, including `ssh host cmd`, scp and
# `zsh -c` from IDEs. bash gets these shells through ~/.bashrc and handles
# them in the non-interactive branch of bash/bashrc; zsh reads only this
# file for them, so the same setup lives here.

# XDG Base Directory Specification
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
export XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}"

# Non-interactive shells: environment and PATH only. Interactive shells
# load the same files from ~/.config/zsh/zshrc.
if [[ ! -o interactive && -f "${XDG_CONFIG_HOME}/zsh/zshenv" ]]; then
    source "${XDG_CONFIG_HOME}/zsh/zshenv"
fi
