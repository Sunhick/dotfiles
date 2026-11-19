# Zsh Configuration

XDG-compliant zsh configuration that mirrors the [bash package](../bash/README.md):
same prompt, aliases, environment, history rules and key bindings.

Install it together with the bash package: `exports`, `aliases` and
`dir_colors` are read from `~/.config/bash/` instead of being copied.

## Files

- `.zshenv` - XDG variables; environment for non-interactive shells (`ssh host cmd`)
- `.zshrc` - Hands off to `~/.config/zsh/zshrc`
- `zshrc` - Main configuration file (mirrors `bash/bashrc`)
- `zshenv` - Non-interactive setup (mirrors the non-interactive branch of `bashrc`)
- `options` - `setopt` equivalents of `bash/shopt`
- `keys` - Key bindings from `readline/inputrc`
- `history` - History settings from `bash/history`
- `prompt` - `[AWS profile] dir »` prompt from `bash/prompt`
- `darwin` - macOS settings from `bash/darwin`

## Differences from bash

- `ble.sh` is bash-only. `zsh-autosuggestions` and `zsh-syntax-highlighting`
  are loaded instead when installed.
- zsh's own completion replaces bash's git completion.
- No equivalent for `cdspell`, `dirspell`, `shift_verbose` or
  `no_empty_cmd_completion`.

## Work configuration

`~/.config/work/zsh/local` is sourced last, if present (bash:
`~/.config/work/bash/local`).
