#!/usr/bin/env bash
# Core module loader with feature detection and dependency resolution
# Requires Bash 5+ for associative arrays and modern features

# Global module registry and feature cache (Bash 5+ associative arrays)
declare -A MODULE_REGISTRY=()
declare -A FEATURE_CACHE=()
declare -A MODULE_STATUS=()
declare -A MODULE_DEPENDENCIES=()

# Configuration
DOTFILES_ROOT="${DOTFILES_ROOT:-$(dirname "$(dirname "$(realpath "${BASH_SOURCE[0]}")")")}"
DEBUG_LOADER="${DEBUG_LOADER:-false}"
ASYNC_LOADING="${ASYNC_LOADING:-true}"

# Initialize feature detection cache
init_feature_cache() {
    local bash_version="${BASH_VERSION%%.*}"
    FEATURE_CACHE["bash_version"]="$bash_version"
    FEATURE_CACHE["bash_major"]="${BASH_VERSINFO[0]}"
    FEATURE_CACHE["bash_minor"]="${BASH_VERSINFO[1]}"

    # Detect OS type
    case "$(uname -s)" in
        Darwin) FEATURE_CACHE["os_type"]="darwin" ;;
        Linux)  FEATURE_CACHE["os_type"]="linux" ;;
        *)      FEATURE_CACHE["os_type"]="unknown" ;;
    esac

    # Cache common tool availability
    local tools=("git" "fzf" "rg" "fd" "delta" "lazygit" "exa" "bat")
    for tool in "${tools[@]}"; do
        if command -v "$tool" &>/dev/null; then
            FEATURE_CACHE["has_$tool"]="true"
        else
            FEATURE_CACHE["has_$tool"]="false"
        fi
    done

    # Check for modern Bash features
    if [[ ${BASH_VERSINFO[0]} -ge 5 ]]; then
        FEATURE_CACHE["has_associative_arrays"]="true"
        FEATURE_CACHE["has_nameref"]="true"
    else
        FEATURE_CACHE["has_associative_arrays"]="false"
        FEATURE_CACHE["has_nameref"]="false"
    fi
}

# Get cached feature detection result
has_feature() {
    local feature="$1"
    [[ "${FEATURE_CACHE[$feature]}" == "true" ]]
}

# Register a module with metadata
register_module() {
    local name="$1"
    local path="$2"
    local load_type="${3:-sync}"  # sync, async, defer
    local condition="${4:-}"      # optional condition command
    local dependencies="${5:-}"   # space-separated list

    MODULE_REGISTRY["$name"]="$path:$load_type:$condition"
    MODULE_STATUS["$name"]="registered"

    if [[ -n "$dependencies" ]]; then
        MODULE_DEPENDENCIES["$name"]="$dependencies"
    fi

    [[ "$DEBUG_LOADER" == "true" ]] && echo "[LOADER] Registered module: $name" >&2
}

# Check if module condition is met
check_module_condition() {
    local name="$1"
    local module_info="${MODULE_REGISTRY[$name]}"
    local condition="${module_info##*:}"

    # No condition means always load
    [[ "$condition" == "$module_info" ]] && return 0
    [[ -z "$condition" ]] && return 0

    # Evaluate condition
    eval "$condition" &>/dev/null
}

# Load module dependencies recursively
load_dependencies() {
    local name="$1"
    local deps="${MODULE_DEPENDENCIES[$name]:-}"

    [[ -z "$deps" ]] && return 0

    for dep in $deps; do
        if [[ "${MODULE_STATUS[$dep]}" != "loaded" ]]; then
            [[ "$DEBUG_LOADER" == "true" ]] && echo "[LOADER] Loading dependency: $dep for $name" >&2
            load_module "$dep" || {
                echo "[LOADER] Failed to load dependency $dep for $name" >&2
                return 1
            }
        fi
    done
}

# Async loading function (background process)
load_module_async() {
    local name="$1"
    local path="$2"

    {
        if [[ -f "$path" ]]; then
            source "$path"
            echo "[LOADER] Async loaded: $name" >&2
        else
            echo "[LOADER] Async load failed - file not found: $path" >&2
        fi
    } &
}

# Main module loading function
load_module() {
    local name="$1"
    local force="${2:-false}"

    # Check if already loaded
    if [[ "${MODULE_STATUS[$name]}" == "loaded" && "$force" != "true" ]]; then
        [[ "$DEBUG_LOADER" == "true" ]] && echo "[LOADER] Module already loaded: $name" >&2
        return 0
    fi

    # Check if module is registered
    if [[ -z "${MODULE_REGISTRY[$name]:-}" ]]; then
        echo "[LOADER] Module not registered: $name" >&2
        return 1
    fi

    local module_info="${MODULE_REGISTRY[$name]}"
    local path="${module_info%%:*}"
    local load_type="${module_info#*:}"
    load_type="${load_type%%:*}"

    # Resolve relative paths
    if [[ "$path" != /* ]]; then
        path="$DOTFILES_ROOT/bash/$path"
    fi

    # Check module condition
    if ! check_module_condition "$name"; then
        [[ "$DEBUG_LOADER" == "true" ]] && echo "[LOADER] Condition not met for: $name" >&2
        MODULE_STATUS["$name"]="skipped"
        return 0
    fi

    # Load dependencies first
    if ! load_dependencies "$name"; then
        echo "[LOADER] Failed to load dependencies for: $name" >&2
        MODULE_STATUS["$name"]="failed"
        return 1
    fi

    # Check if file exists
    if [[ ! -f "$path" ]]; then
        echo "[LOADER] Module file not found: $path" >&2
        MODULE_STATUS["$name"]="failed"
        return 1
    fi

    [[ "$DEBUG_LOADER" == "true" ]] && echo "[LOADER] Loading module: $name ($load_type)" >&2

    # Load based on type
    case "$load_type" in
        "async")
            if [[ "$ASYNC_LOADING" == "true" ]]; then
                load_module_async "$name" "$path"
                MODULE_STATUS["$name"]="loading"
            else
                source "$path" && MODULE_STATUS["$name"]="loaded"
            fi
            ;;
        "defer")
            # Defer loading - just mark as deferred for now
            MODULE_STATUS["$name"]="deferred"
            ;;
        *)
            # Synchronous loading
            if source "$path"; then
                MODULE_STATUS["$name"]="loaded"
                [[ "$DEBUG_LOADER" == "true" ]] && echo "[LOADER] Successfully loaded: $name" >&2
            else
                MODULE_STATUS["$name"]="failed"
                echo "[LOADER] Failed to load: $name" >&2
                return 1
            fi
            ;;
    esac

    return 0
}

# Load deferred modules (call this when needed)
load_deferred_modules() {
    local modules=()

    # Find deferred modules
    for name in "${!MODULE_STATUS[@]}"; do
        if [[ "${MODULE_STATUS[$name]}" == "deferred" ]]; then
            modules+=("$name")
        fi
    done

    # Load them
    for name in "${modules[@]}"; do
        [[ "$DEBUG_LOADER" == "true" ]] && echo "[LOADER] Loading deferred module: $name" >&2
        load_module "$name"
    done
}

# Get module status
module_status() {
    local name="$1"
    echo "${MODULE_STATUS[$name]:-unregistered}"
}

# List all modules with their status
list_modules() {
    printf "%-20s %-10s %-15s\n" "MODULE" "STATUS" "TYPE"
    printf "%-20s %-10s %-15s\n" "------" "------" "----"

    for name in "${!MODULE_REGISTRY[@]}"; do
        local module_info="${MODULE_REGISTRY[$name]}"
        local load_type="${module_info#*:}"
        load_type="${load_type%%:*}"
        local status="${MODULE_STATUS[$name]}"

        printf "%-20s %-10s %-15s\n" "$name" "$status" "$load_type"
    done
}

# Initialize the loader
init_loader() {
    # Initialize feature cache
    init_feature_cache

    # Register core modules
    register_module "error_handler" "core/error_handler.bash" "sync" "" ""
    register_module "xdg" "features/xdg.bash" "sync" "" ""
    register_module "history" "core/history.bash" "sync" "" "error_handler"
    register_module "completion" "core/completion.bash" "defer" "command -v fzf" "error_handler"
    register_module "git" "features/git.bash" "async" "command -v git" "error_handler"
    register_module "navigation" "core/navigation.bash" "sync" "" "error_handler"
    register_module "security" "core/security.bash" "sync" "" "error_handler"
    register_module "performance" "features/performance.bash" "defer" "" "error_handler"

    [[ "$DEBUG_LOADER" == "true" ]] && echo "[LOADER] Initialized with ${#MODULE_REGISTRY[@]} modules" >&2
}

# Auto-initialize when sourced
if [[ "${BASH_SOURCE[0]}" != "${0}" ]]; then
    init_loader
fi
