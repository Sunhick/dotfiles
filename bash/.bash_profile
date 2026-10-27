# OSX uses bash_profile, source .bashrc for compatibility
# ~/.bash_profile
if [[ -f ~/.bashrc ]]; then
    source ~/.bashrc
fi

# Added by AIM CLI
export PATH="$HOME/.aim/mcp-servers:$PATH"
