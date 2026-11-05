# -*- mode: shell-script -*-
# ble.sh rc (XDG location of ~/.blerc), read while bash/bashrc loads ble.sh.
# Inspect current values: `bleopt`, `ble-face`. Docs: https://github.com/akinomyoga/ble.sh/wiki
#
# Readline bindings: bashrc loads ble.sh before its `bind -f` of
# readline/inputrc, so ble.sh imports that file itself.
#
# Accept a suggestion: C-f / C-e / Right / End (Miryoku NAV ' and /) take all
# of it, M-f takes one word, C-j takes it and runs. Keymap stays emacs.

set -o emacs

# Word deletes. Ctrl+Backspace: ble's default is undo; Alt+Backspace: ble's
# default only copies the word.
ble-bind -f C-BS  kill-backward-cword
ble-bind -f M-DEL kill-backward-cword
ble-bind -f M-C-h kill-backward-cword

# Suggestions: plain grey text, no background, so they read as "not typed yet"
ble-face auto_complete='fg=242'

# Quiet: no "[ble: exit N]" / "[ble: elapsed ...]" lines after commands
bleopt exec_errexit_mark=
bleopt exec_elapsed_mark=

# File names on the command line use the same LS_COLORS as `ls`
# (bash/dir_colors). LS_COLORS is only final at the end of bashrc, so read
# it on attach. The prompt (PS1) is drawn as-is; ble.sh doesn't restyle it.
_blerc_ls_colors() { bleopt filename_ls_colors="$LS_COLORS"; }
blehook ATTACH+=_blerc_ls_colors

# No underlines. Fallbacks for file types LS_COLORS doesn't cover: ble's
# default colours with the underline dropped.
ble-face command_directory='fg=109,bold'   # = di in bash/dir_colors
ble-face filename_directory='fg=109,bold'
ble-face filename_directory_sticky='fg=white,bg=26'
ble-face filename_executable='fg=green'
ble-face filename_link='fg=teal'
ble-face filename_orphan='fg=teal,bg=224'
ble-face filename_url='fg=blue'
ble-face filename_warning='fg=red'
ble-face filename_block='fg=yellow,bg=black'
ble-face filename_character='fg=white,bg=black'
ble-face filename_pipe='fg=lime,bg=black'
ble-face filename_socket='fg=cyan,bg=black'
ble-face filename_setuid='fg=black,bg=220'
ble-face filename_setgid='fg=black,bg=191'
ble-face filename_ls_colors=none
ble-face filename_other=none
