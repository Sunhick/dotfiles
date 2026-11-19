# ~/.zshrc -- read by interactive zsh. Like ~/.bashrc, it only hands off
# to the XDG-compliant config in ~/.config/zsh.

# Set up XDG directories if not already set (~/.zshenv normally has)
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
export XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}"

# Source the XDG-compliant zshrc
if [[ -f "${XDG_CONFIG_HOME}/zsh/zshrc" ]]; then
    source "${XDG_CONFIG_HOME}/zsh/zshrc"
else
    echo "Warning: XDG-compliant zshrc not found at ${XDG_CONFIG_HOME}/zsh/zshrc" >&2
fi
