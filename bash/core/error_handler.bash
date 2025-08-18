#!/usr/bin/env bash
# Enhanced error handling and validation framework
# Provides centralized error handling, input validation, and debug logging

# Configuration
DEBUG_MODE="${DEBUG_MODE:-false}"
VERBOSE_MODE="${VERBOSE_MODE:-false}"
LOG_FILE="${XDG_CACHE_HOME:-$HOME/.cache}/shell/error.log"
MAX_LOG_SIZE=1048576  # 1MB

# Error severity levels
declare -A SEVERITY_LEVELS=(
    ["debug"]=0
    ["info"]=1
    ["warning"]=2
    ["error"]=3
    ["critical"]=4
)

# Color codes for different severity levels
declare -A SEVERITY_COLORS=(
    ["debug"]="\033[0;36m"      # Cyan
    ["info"]="\033[0;32m"       # Green
    ["warning"]="\033[0;33m"    # Yellow
    ["error"]="\033[0;31m"      # Red
    ["critical"]="\033[1;31m"   # Bold Red
)

# Reset color
RESET_COLOR="\033[0m"

# Initialize error handling system
init_error_handler() {
    # Create log directory if it doesn't exist
    local log_dir
    log_dir="$(dirname "$LOG_FILE")"
    [[ ! -d "$log_dir" ]] && mkdir -p "$log_dir"

    # Rotate log if it's too large
    if [[ -f "$LOG_FILE" && $(stat -f%z "$LOG_FILE" 2>/dev/null || stat -c%s "$LOG_FILE" 2>/dev/null) -gt $MAX_LOG_SIZE ]]; then
        mv "$LOG_FILE" "${LOG_FILE}.old"
    fi

    # Set up error trapping for debugging
    if [[ "$DEBUG_MODE" == "true" ]]; then
        set -eE
        trap 'error_handler "TRAP" "Command failed at line $LINENO: $BASH_COMMAND" "error" "${BASH_SOURCE[1]}" "$LINENO"' ERR
    fi
}

# Main error handling function
error_handler() {
    local module="${1:-UNKNOWN}"
    local message="${2:-Unknown error}"
    local severity="${3:-error}"
    local source_file="${4:-${BASH_SOURCE[2]}}"
    local line_number="${5:-${BASH_LINENO[1]}}"

    # Validate severity level
    if [[ -z "${SEVERITY_LEVELS[$severity]:-}" ]]; then
        severity="error"
    fi

    local timestamp
    timestamp="$(date '+%Y-%m-%d %H:%M:%S')"
    local color="${SEVERITY_COLORS[$severity]}"

    # Format the error message
    local formatted_message="[$timestamp] [$severity] [$module]"
    if [[ -n "$source_file" && "$source_file" != "unknown" ]]; then
        formatted_message+=" [$(basename "$source_file"):$line_number]"
    fi
    formatted_message+=": $message"

    # Log to file
    echo "$formatted_message" >> "$LOG_FILE" 2>/dev/null || true

    # Output to stderr based on severity and verbosity settings
    local should_output=false
    case "$severity" in
        "debug")
            [[ "$DEBUG_MODE" == "true" ]] && should_output=true
            ;;
        "info")
            [[ "$VERBOSE_MODE" == "true" || "$DEBUG_MODE" == "true" ]] && should_output=true
            ;;
        "warning"|"error"|"critical")
            should_output=true
            ;;
    esac

    if [[ "$should_output" == "true" ]]; then
        echo -e "${color}${formatted_message}${RESET_COLOR}" >&2
    fi

    # Return appropriate exit code
    case "$severity" in
        "critical") return 2 ;;
        "error") return 1 ;;
        *) return 0 ;;
    esac
}

# Convenience functions for different severity levels
debug() {
    error_handler "${FUNCNAME[1]:-SHELL}" "$1" "debug" "${BASH_SOURCE[1]}" "${BASH_LINENO[0]}"
}

info() {
    error_handler "${FUNCNAME[1]:-SHELL}" "$1" "info" "${BASH_SOURCE[1]}" "${BASH_LINENO[0]}"
}

warning() {
    error_handler "${FUNCNAME[1]:-SHELL}" "$1" "warning" "${BASH_SOURCE[1]}" "${BASH_LINENO[0]}"
}

error() {
    error_handler "${FUNCNAME[1]:-SHELL}" "$1" "error" "${BASH_SOURCE[1]}" "${BASH_LINENO[0]}"
}

critical() {
    error_handler "${FUNCNAME[1]:-SHELL}" "$1" "critical" "${BASH_SOURCE[1]}" "${BASH_LINENO[0]}"
}

# Input validation functions
validate_not_empty() {
    local value="$1"
    local field_name="${2:-field}"

    if [[ -z "$value" ]]; then
        error "Validation failed: $field_name cannot be empty"
        return 1
    fi
    return 0
}

validate_file_exists() {
    local file_path="$1"
    local description="${2:-file}"

    if [[ ! -f "$file_path" ]]; then
        error "Validation failed: $description does not exist: $file_path"
        return 1
    fi
    return 0
}

validate_directory_exists() {
    local dir_path="$1"
    local description="${2:-directory}"

    if [[ ! -d "$dir_path" ]]; then
        error "Validation failed: $description does not exist: $dir_path"
        return 1
    fi
    return 0
}

validate_command_exists() {
    local command="$1"
    local description="${2:-command}"

    if ! command -v "$command" &>/dev/null; then
        error "Validation failed: $description not found: $command"
        return 1
    fi
    return 0
}

validate_numeric() {
    local value="$1"
    local field_name="${2:-value}"

    if ! [[ "$value" =~ ^[0-9]+$ ]]; then
        error "Validation failed: $field_name must be numeric: $value"
        return 1
    fi
    return 0
}

validate_path_safe() {
    local path="$1"
    local field_name="${2:-path}"

    # Check for dangerous patterns
    if [[ "$path" =~ \.\./|^/|^\~ ]]; then
        warning "Potentially unsafe path detected: $path"
    fi

    # Sanitize path
    path="${path//[^a-zA-Z0-9._/-]/}"

    if [[ -z "$path" ]]; then
        error "Validation failed: $field_name contains only invalid characters"
        return 1
    fi

    return 0
}

# Configuration file integrity checking
check_file_integrity() {
    local file_path="$1"
    local expected_permissions="${2:-644}"

    if [[ ! -f "$file_path" ]]; then
        error "Integrity check failed: file does not exist: $file_path"
        return 1
    fi

    # Check file permissions
    local actual_permissions
    actual_permissions="$(stat -f%A "$file_path" 2>/dev/null || stat -c%a "$file_path" 2>/dev/null)"

    if [[ "$actual_permissions" != "$expected_permissions" ]]; then
        warning "File permissions mismatch for $file_path: expected $expected_permissions, got $actual_permissions"
    fi

    # Check if file is readable
    if [[ ! -r "$file_path" ]]; then
        error "Integrity check failed: file is not readable: $file_path"
        return 1
    fi

    # Basic syntax check for shell scripts
    if [[ "$file_path" =~ \.(bash|sh)$ ]]; then
        if ! bash -n "$file_path" 2>/dev/null; then
            error "Integrity check failed: syntax error in shell script: $file_path"
            return 1
        fi
    fi

    debug "File integrity check passed: $file_path"
    return 0
}

# Safe file sourcing with error handling
safe_source() {
    local file_path="$1"
    local required="${2:-true}"

    if [[ ! -f "$file_path" ]]; then
        if [[ "$required" == "true" ]]; then
            error "Cannot source required file: $file_path"
            return 1
        else
            debug "Optional file not found, skipping: $file_path"
            return 0
        fi
    fi

    # Check file integrity first
    if ! check_file_integrity "$file_path"; then
        error "File integrity check failed, refusing to source: $file_path"
        return 1
    fi

    # Source the file with error handling
    if source "$file_path"; then
        debug "Successfully sourced: $file_path"
        return 0
    else
        error "Failed to source file: $file_path"
        return 1
    fi
}

# Environment variable validation
validate_environment() {
    local required_vars=("$@")
    local missing_vars=()

    for var in "${required_vars[@]}"; do
        if [[ -z "${!var:-}" ]]; then
            missing_vars+=("$var")
        fi
    done

    if [[ ${#missing_vars[@]} -gt 0 ]]; then
        error "Missing required environment variables: ${missing_vars[*]}"
        return 1
    fi

    debug "Environment validation passed for: ${required_vars[*]}"
    return 0
}

# Cleanup function for graceful shutdown
cleanup_error_handler() {
    # Disable error trapping
    set +eE
    trap - ERR

    # Final log entry
    info "Error handler cleanup completed"
}

# Show recent errors from log
show_recent_errors() {
    local lines="${1:-20}"

    if [[ -f "$LOG_FILE" ]]; then
        echo "Recent errors from $LOG_FILE:"
        tail -n "$lines" "$LOG_FILE" | grep -E "\[(error|critical)\]" || echo "No recent errors found"
    else
        echo "No error log file found at $LOG_FILE"
    fi
}

# Clear error log
clear_error_log() {
    if [[ -f "$LOG_FILE" ]]; then
        > "$LOG_FILE"
        info "Error log cleared"
    fi
}

# Set up cleanup on exit
trap cleanup_error_handler EXIT

# Initialize when sourced
if [[ "${BASH_SOURCE[0]}" != "${0}" ]]; then
    init_error_handler
fi
