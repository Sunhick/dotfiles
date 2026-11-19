# fzf for zsh: key bindings only (Ctrl-R, Ctrl-T, Alt-C). Mirrors fzf.bash.

export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"

# Shared options (colors, fd/bat previews)
if [[ -f "${XDG_CONFIG_HOME}/fzf/config" ]]; then
    source "${XDG_CONFIG_HOME}/fzf/config"
fi

if command -v fzf >/dev/null 2>&1; then
    _fzf_loaded=""
    for keybind_file in \
        "${HOMEBREW_PREFIX:-/opt/homebrew}/opt/fzf/shell/key-bindings.zsh" \
        "/opt/homebrew/opt/fzf/shell/key-bindings.zsh" \
        "/usr/local/opt/fzf/shell/key-bindings.zsh" \
        "/home/linuxbrew/.linuxbrew/opt/fzf/shell/key-bindings.zsh" \
        "/usr/share/fzf/key-bindings.zsh" \
        "/usr/share/doc/fzf/examples/key-bindings.zsh"; do
        if [[ -f "$keybind_file" ]]; then
            source "$keybind_file"
            _fzf_loaded=1
            break
        fi
    done
    unset keybind_file
    # No key-bindings file: fzf 0.48+ prints its own zsh integration
    if [[ -z "$_fzf_loaded" ]]; then
        source <(fzf --zsh 2>/dev/null)
    fi
    unset _fzf_loaded

    # fzf-git.sh supports zsh too
    for fzf_git_file in \
        "${XDG_CONFIG_HOME}/fzf/fzf-git.sh" \
        "/opt/homebrew/share/fzf-git/fzf-git.sh" \
        "${XDG_DATA_HOME:-$HOME/.local/share}/fzf-git/fzf-git.sh"; do
        if [[ -f "$fzf_git_file" ]]; then
            source "$fzf_git_file"
            break
        fi
    done
    unset fzf_git_file
fi
