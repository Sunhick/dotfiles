#!/usr/bin/env bash
# XDG Base Directory support structure
# Implements XDG Base Directory specification for cleaner home directory organization

# XDG Base Directory specification defaults
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
export XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}"
export XDG_RUNTIME_DIR="${XDG_RUNTIME_DIR:-/tmp/runtime-$USER}"

# Additional XDG directories for better organization
export XDG_BIN_HOME="${XDG_BIN_HOME:-$HOME/.local/bin}"
export XDG_LIB_HOME="${XDG_LIB_HOME:-$HOME/.local/lib}"

# Shell-specific XDG directories
export XDG_SHELL_CONFIG="$XDG_CONFIG_HOME/shell"
export XDG_SHELL_DATA="$XDG_DATA_HOME/shell"
export XDG_SHELL_CACHE="$XDG_CACHE_HOME/shell"
export XDG_SHELL_STATE="$XDG_STATE_HOME/shell"

# Migration tracking
XDG_MIGRATION_LOG="$XDG_STATE_HOME/shell/migrations.log"

# Initialize XDG directory structure
init_xdg_directories() {
    local directories=(
        "$XDG_CONFIG_HOME"
        "$XDG_DATA_HOME"
        "$XDG_CACHE_HOME"
        "$XDG_STATE_HOME"
        "$XDG_BIN_HOME"
        "$XDG_LIB_HOME"
        "$XDG_SHELL_CONFIG"
        "$XDG_SHELL_DATA"
        "$XDG_SHELL_CACHE"
        "$XDG_SHELL_STATE"
    )

    for dir in "${directories[@]}"; do
        if [[ ! -d "$dir" ]]; then
            if mkdir -p "$dir" 2>/dev/null; then
                debug "Created XDG directory: $dir"
            else
                warning "Failed to create XDG directory: $dir"
            fi
        fi
    done

    # Set appropriate permissions for runtime directory
    if [[ -d "$XDG_RUNTIME_DIR" ]]; then
        chmod 700 "$XDG_RUNTIME_DIR" 2>/dev/null || true
    fi

    # Initialize migration log
    mkdir -p "$(dirname "$XDG_MIGRATION_LOG")" 2>/dev/null || true
    [[ ! -f "$XDG_MIGRATION_LOG" ]] && touch "$XDG_MIGRATION_LOG"
}

# Check if a migration has already been performed
is_migrated() {
    local migration_name="$1"
    grep -q "^$migration_name:" "$XDG_MIGRATION_LOG" 2>/dev/null
}

# Log a completed migration
log_migration() {
    local migration_name="$1"
    local timestamp
    timestamp="$(date '+%Y-%m-%d %H:%M:%S')"
    echo "$migration_name:$timestamp" >> "$XDG_MIGRATION_LOG"
    info "Migration completed: $migration_name"
}

# Safe file/directory migration function
migrate_to_xdg() {
    local old_path="$1"
    local new_path="$2"
    local migration_name="$3"
    local backup="${4:-true}"

    # Skip if migration already completed
    if is_migrated "$migration_name"; then
        debug "Migration already completed: $migration_name"
        return 0
    fi

    # Skip if source doesn't exist
    if [[ ! -e "$old_path" ]]; then
        debug "Source path doesn't exist, skipping migration: $old_path"
        log_migration "$migration_name"
        return 0
    fi

    # Skip if destination already exists
    if [[ -e "$new_path" ]]; then
        warning "Destination already exists, skipping migration: $new_path"
        log_migration "$migration_name"
        return 0
    fi

    # Create destination directory
    local new_dir
    new_dir="$(dirname "$new_path")"
    if [[ ! -d "$new_dir" ]]; then
        mkdir -p "$new_dir" || {
            error "Failed to create destination directory: $new_dir"
            return 1
        }
    fi

    # Create backup if requested
    if [[ "$backup" == "true" ]]; then
        local backup_path="${old_path}.xdg-backup"
        if cp -r "$old_path" "$backup_path" 2>/dev/null; then
            debug "Created backup: $backup_path"
        else
            warning "Failed to create backup for: $old_path"
        fi
    fi

    # Perform migration
    if mv "$old_path" "$new_path" 2>/dev/null; then
        info "Migrated $old_path -> $new_path"
        log_migration "$migration_name"
        return 0
    else
        error "Failed to migrate $old_path -> $new_path"
        return 1
    fi
}

# Create symlink for legacy application compatibility
create_legacy_symlink() {
    local xdg_path="$1"
    local legacy_path="$2"
    local description="${3:-file}"

    # Skip if legacy path already exists and is not a symlink
    if [[ -e "$legacy_path" && ! -L "$legacy_path" ]]; then
        debug "Legacy path exists and is not a symlink: $legacy_path"
        return 0
    fi

    # Remove existing symlink if it's broken
    if [[ -L "$legacy_path" && ! -e "$legacy_path" ]]; then
        rm "$legacy_path"
        debug "Removed broken symlink: $legacy_path"
    fi

    # Create symlink if XDG path exists
    if [[ -e "$xdg_path" ]]; then
        if ln -sf "$xdg_path" "$legacy_path" 2>/dev/null; then
            debug "Created legacy symlink for $description: $legacy_path -> $xdg_path"
        else
            warning "Failed to create legacy symlink: $legacy_path"
        fi
    fi
}

# Common application migrations
migrate_common_applications() {
    info "Starting XDG migrations for common applications"

    # Bash history
    migrate_to_xdg "$HOME/.bash_history" "$XDG_STATE_HOME/bash/history" "bash_history"

    # Less history
    migrate_to_xdg "$HOME/.lesshst" "$XDG_STATE_HOME/less/history" "less_history"

    # Wget configuration
    migrate_to_xdg "$HOME/.wgetrc" "$XDG_CONFIG_HOME/wget/wgetrc" "wget_config"

    # Readline configuration
    migrate_to_xdg "$HOME/.inputrc" "$XDG_CONFIG_HOME/readline/inputrc" "readline_config"

    # Vim configuration (if exists)
    if [[ -d "$HOME/.vim" ]]; then
        migrate_to_xdg "$HOME/.vim" "$XDG_CONFIG_HOME/vim" "vim_config"
    fi
    if [[ -f "$HOME/.vimrc" ]]; then
        migrate_to_xdg "$HOME/.vimrc" "$XDG_CONFIG_HOME/vim/vimrc" "vim_vimrc"
    fi

    # Git configuration (if not already XDG compliant)
    if [[ -f "$HOME/.gitconfig" && ! -f "$XDG_CONFIG_HOME/git/config" ]]; then
        migrate_to_xdg "$HOME/.gitconfig" "$XDG_CONFIG_HOME/git/config" "git_config"
    fi

    # SSH configuration (careful with this one)
    if [[ -d "$HOME/.ssh" ]]; then
        info "SSH directory found - consider manually reviewing XDG compliance"
    fi
}

# Set up environment variables for XDG-aware applications
setup_xdg_environment() {
    # Bash
    export HISTFILE="$XDG_STATE_HOME/bash/history"

    # Less
    export LESSHISTFILE="$XDG_STATE_HOME/less/history"

    # Wget
    export WGETRC="$XDG_CONFIG_HOME/wget/wgetrc"

    # Readline
    export INPUTRC="$XDG_CONFIG_HOME/readline/inputrc"

    # Vim
    export VIMINIT='let $MYVIMRC = !has("nvim") ? "$XDG_CONFIG_HOME/vim/vimrc" : "$XDG_CONFIG_HOME/nvim/init.vim" | so $MYVIMRC'

    # Node.js
    export NODE_REPL_HISTORY="$XDG_STATE_HOME/node/repl_history"
    export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/npmrc"
    export NPM_CONFIG_CACHE="$XDG_CACHE_HOME/npm"

    # Python
    export PYTHONSTARTUP="$XDG_CONFIG_HOME/python/pythonrc"
    export PYTHON_HISTORY="$XDG_STATE_HOME/python/history"

    # Rust
    export CARGO_HOME="$XDG_DATA_HOME/cargo"
    export RUSTUP_HOME="$XDG_DATA_HOME/rustup"

    # Go
    export GOPATH="$XDG_DATA_HOME/go"

    # Docker
    export DOCKER_CONFIG="$XDG_CONFIG_HOME/docker"

    # GnuPG
    export GNUPGHOME="$XDG_DATA_HOME/gnupg"

    # MySQL
    export MYSQL_HISTFILE="$XDG_STATE_HOME/mysql/history"

    # PostgreSQL
    export PSQL_HISTORY="$XDG_STATE_HOME/psql/history"

    # Redis
    export REDISCLI_HISTFILE="$XDG_STATE_HOME/redis/history"

    debug "XDG environment variables configured"
}

# Create compatibility shims for legacy applications
create_compatibility_shims() {
    # Create symlinks for applications that don't support XDG but we've migrated
    create_legacy_symlink "$XDG_CONFIG_HOME/wget/wgetrc" "$HOME/.wgetrc" "wget config"
    create_legacy_symlink "$XDG_CONFIG_HOME/readline/inputrc" "$HOME/.inputrc" "readline config"

    # Create wrapper scripts for applications that need special handling
    local wrapper_dir="$XDG_BIN_HOME"
    mkdir -p "$wrapper_dir"

    # Example: MySQL wrapper that uses XDG history file
    if command -v mysql &>/dev/null && [[ ! -f "$wrapper_dir/mysql" ]]; then
        cat > "$wrapper_dir/mysql" << 'EOF'
#!/usr/bin/env bash
# MySQL wrapper with XDG support
export MYSQL_HISTFILE="${XDG_STATE_HOME:-$HOME/.local/state}/mysql/history"
mkdir -p "$(dirname "$MYSQL_HISTFILE")"
exec /usr/bin/mysql "$@"
EOF
        chmod +x "$wrapper_dir/mysql"
        debug "Created MySQL XDG wrapper"
    fi
}

# Clean up old dotfiles after successful migration
cleanup_migrated_files() {
    local cleanup_list=(
        "$HOME/.bash_history.xdg-backup"
        "$HOME/.lesshst.xdg-backup"
        "$HOME/.wgetrc.xdg-backup"
        "$HOME/.inputrc.xdg-backup"
    )

    for file in "${cleanup_list[@]}"; do
        if [[ -f "$file" ]]; then
            local age_days
            age_days=$(( ($(date +%s) - $(stat -f%m "$file" 2>/dev/null || stat -c%Y "$file" 2>/dev/null)) / 86400 ))

            # Only clean up backups older than 30 days
            if [[ $age_days -gt 30 ]]; then
                if rm "$file" 2>/dev/null; then
                    debug "Cleaned up old backup: $file"
                fi
            fi
        fi
    done
}

# Show XDG directory status
show_xdg_status() {
    echo "XDG Base Directory Status:"
    echo "=========================="
    printf "%-20s %s\n" "XDG_CONFIG_HOME:" "$XDG_CONFIG_HOME"
    printf "%-20s %s\n" "XDG_DATA_HOME:" "$XDG_DATA_HOME"
    printf "%-20s %s\n" "XDG_CACHE_HOME:" "$XDG_CACHE_HOME"
    printf "%-20s %s\n" "XDG_STATE_HOME:" "$XDG_STATE_HOME"
    printf "%-20s %s\n" "XDG_RUNTIME_DIR:" "$XDG_RUNTIME_DIR"
    echo

    echo "Directory Status:"
    echo "=================="
    local dirs=("$XDG_CONFIG_HOME" "$XDG_DATA_HOME" "$XDG_CACHE_HOME" "$XDG_STATE_HOME")
    for dir in "${dirs[@]}"; do
        if [[ -d "$dir" ]]; then
            printf "%-20s ✓ exists\n" "$(basename "$dir"):"
        else
            printf "%-20s ✗ missing\n" "$(basename "$dir"):"
        fi
    done

    echo
    echo "Migration Status:"
    echo "=================="
    if [[ -f "$XDG_MIGRATION_LOG" ]]; then
        while IFS=: read -r migration timestamp; do
            printf "%-20s %s\n" "$migration:" "$timestamp"
        done < "$XDG_MIGRATION_LOG"
    else
        echo "No migrations recorded"
    fi
}

# Main XDG initialization function
init_xdg() {
    debug "Initializing XDG Base Directory support"

    # Initialize directory structure
    init_xdg_directories

    # Set up environment variables
    setup_xdg_environment

    # Perform migrations (only on first run or when needed)
    migrate_common_applications

    # Create compatibility shims
    create_compatibility_shims

    # Clean up old backups
    cleanup_migrated_files

    info "XDG Base Directory support initialized"
}

# Initialize when sourced
if [[ "${BASH_SOURCE[0]}" != "${0}" ]]; then
    init_xdg
fi
