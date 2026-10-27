# Tmux Configuration

XDG-compliant tmux configuration with custom key bindings and theme.

## Files

- `.config/tmux/tmux.conf` - Main tmux configuration

## XDG Compliance

Tmux supports XDG configuration when using `~/.config/tmux/tmux.conf`.

## Key Features

- Backtick (`) as prefix key instead of Ctrl-B
- Emacs-style copy mode, with mod-free `v` / `y` selection
- Bindings tuned for Miryoku (prefix, digits and split keys share the NUM layer)
- Mouse support enabled
- Custom color theme with gruvbox-inspired colors
- Copies go to the system clipboard via OSC 52 (works over SSH)
- Increased history limit (50,000 lines)

## Key Bindings

- **Prefix**: ` (backtick)
- **Reload config**: `prefix + r`
- **Window**: `prefix + 1-9`; `prefix + n / p` (repeatable); `prefix + Tab` last window
- **Pane**: `prefix + arrows` (repeatable) or `h j k l`; `prefix + ;` last pane
- **Split**: `prefix + \` or `|` (side by side), `prefix + -` (stacked)
- **Copy mode**: `prefix + [` or `Enter`; `Shift + PgUp` without prefix

Alt+number and Alt+N/P are intentionally not bound in the root table: they
would shadow Emacs `digit-argument` and M-n/M-p history in readline and Emacs.
