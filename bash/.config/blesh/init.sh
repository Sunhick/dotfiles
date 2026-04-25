#!/usr/bin/env bash
# ble.sh configuration — fish-like experience for bash
# See: https://github.com/akinomyoga/ble.sh

# ── Autosuggestions ──────────────────────────────────────────────
# Show suggestions from history as you type (grayed out)
ble-face auto_complete='fg=242'

# Accept suggestion with right arrow
bleopt complete_auto_history=1

# ── Syntax highlighting ─────────────────────────────────────────
# Gruvbox-inspired colors
ble-face syntax_default='fg=223'          # fg1
ble-face syntax_command='fg=142'          # green
ble-face syntax_filename='fg=109'         # blue
ble-face syntax_directory='fg=109,bold'   # blue bold
ble-face syntax_quoted='fg=214'           # yellow
ble-face syntax_error='fg=167,bold'       # red
ble-face syntax_comment='fg=245,italic'   # gray
ble-face command_builtin='fg=142'         # green
ble-face command_function='fg=142'        # green
ble-face command_alias='fg=142'           # green
ble-face filename_directory='fg=109,bold' # blue bold

# ── Completion ───────────────────────────────────────────────────
# Show completion menu automatically
bleopt complete_auto_delay=300

# Limit menu height
bleopt menu_style=dense

# ── Misc ─────────────────────────────────────────────────────────
# Don't ring the bell
bleopt edit_abell=
bleopt edit_vbell=
