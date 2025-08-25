#!/usr/bin/env bash
# ~/.bash_logout: executed by bash when login shell exits

# Clear the screen for security's sake
if [ "$SHLVL" = 1 ]; then
    [ -x /usr/bin/clear_console ] && /usr/bin/clear_console -q
fi

# Clear history if desired (uncomment if needed)
# history -c
