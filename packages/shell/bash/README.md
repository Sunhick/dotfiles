# Bash Infrastructure

Modern, modular Bash configuration with intelligent loading and XDG compliance.

## 🏗️ Architecture Overview

```
bash/
├── core/                    # Core infrastructure modules
│   ├── loader.bash         # Module loading system with dependency resolution
│   ├── error_handler.bash  # Centralized error handling and validation
│   ├── history.bash        # Enhanced history management
│   ├── navigation.bash     # Directory navigation utilities
│   ├── completion.bash     # Intelligent tab completion
│   └── security.bash       # Security enhancements
├── features/               # Feature-specific modules
│   ├── xdg.bash           # XDG Base Directory support
│   ├── git.bash           # Git enhancements and aliases
│   ├── performance.bash   # Performance monitoring and optimization
│   └── [more features]    # Additional feature modules
├── themes/                # Visual themes and prompts
├── platform/             # Platform-specific configurations
│   ├── darwin.bash       # macOS-specific settings
│   └── linux.bash        # Linux-specific settings
└── README.md             # This file
```

## 🚀 Core Components

### Module Loader (`core/loader.bash`)

The heart of the system, providing:

- **Feature Detection**: Caches tool availability and system capabilities
- **Dependency Resolution**: Loads modules in correct order based on dependencies
- **Conditional Loading**: Only loads modules when their requirements are met
- **Async Loading**: Non-blocking loading for performance-critical modules
- **Error Handling**: Graceful degradation when modules fail to load

#### Usage Examples

```bash
# Load a specific module
load_module "git"

# Check if a feature is available
if has_feature "has_fzf"; then
    echo "fzf is available"
fi

# List all registered modules
list_modules

# Check module status
module_status "git"  # Returns: loaded, failed, skipped, etc.
```

#### Module Registration

```bash
# Register a new module
register_module "module_name" "path/to/module.bash" "load_type" "condition" "dependencies"

# Examples:
register_module "git" "features/git.bash" "async" "command -v git" "error_handler"
register_module "completion" "core/completion.bash" "defer" "command -v fzf" "error_handler"
```

### Error Handler (`core/error_handler.bash`)

Comprehensive error handling and validation framework:

- **Severity Levels**: debug, info, warning, error, critical
- **Structured Logging**: Timestamped logs with context information
- **Input Validation**: Common validation functions for safe operations
- **File Integrity**: Configuration file validation and safety checks

#### Usage Examples

```bash
# Logging functions
debug "Debug information"
info "General information"
warning "Something might be wrong"
error "An error occurred"
critical "Critical failure"

# Validation functions
validate_not_empty "$value" "field_name"
validate_file_exists "/path/to/file" "config file"
validate_command_exists "git" "Git command"

# Safe file operations
safe_source "/path/to/config.bash" "optional"
check_file_integrity "/path/to/script.bash" "755"
```

### XDG Support (`features/xdg.bash`)

Complete XDG Base Directory specification implementation:

- **Automatic Migration**: Safely moves existing dotfiles to XDG locations
- **Directory Management**: Creates and maintains XDG directory structure
- **Environment Setup**: Configures applications to use XDG directories
- **Legacy Compatibility**: Provides shims for non-XDG applications

#### XDG Directory Structure

```
~/.config/          # XDG_CONFIG_HOME - Configuration files
~/.local/share/     # XDG_DATA_HOME - Application data
~/.cache/           # XDG_CACHE_HOME - Cache files
~/.local/state/     # XDG_STATE_HOME - State files (logs, history)
~/.local/bin/       # XDG_BIN_HOME - User binaries
```

#### Migration Examples

```bash
# Manual migration
migrate_to_xdg "$HOME/.myconfig" "$XDG_CONFIG_HOME/myapp/config" "myapp_config"

# Check migration status
is_migrated "bash_history"

# Show XDG status
show_xdg_status
```

## 🔧 Configuration

### Environment Variables

```bash
# Debug and logging
export DEBUG_MODE=true          # Enable debug logging
export DEBUG_LOADER=true        # Enable loader debug messages
export VERBOSE_MODE=true        # Enable verbose output

# Module loading
export ASYNC_LOADING=true       # Enable async module loading
export DOTFILES_ROOT="/path"    # Override dotfiles root directory

# XDG directories (optional overrides)
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_STATE_HOME="$HOME/.local/state"
```

### Module Loading Types

- **sync**: Load immediately and block until complete
- **async**: Load in background (non-blocking)
- **defer**: Load only when explicitly requested

### Dependency System

Modules can declare dependencies that must be loaded first:

```bash
# This module requires error_handler to be loaded first
register_module "git" "features/git.bash" "async" "command -v git" "error_handler"
```

## 🧪 Testing

Test the infrastructure:

```bash
# Enable debug mode
export DEBUG_MODE=true
export DEBUG_LOADER=true

# Source the loader
source bash/core/loader.bash

# Check feature detection
echo "Available features:"
for key in "${!FEATURE_CACHE[@]}"; do
    echo "  $key: ${FEATURE_CACHE[$key]}"
done

# Test module loading
load_module "xdg"
load_module "git"

# Check status
list_modules
```

## 🐛 Troubleshooting

### Common Issues

**Bash Version Compatibility**
```bash
# Check Bash version (requires 5+)
bash --version

# Check for associative array support
if [[ ${BASH_VERSINFO[0]} -ge 5 ]]; then
    echo "Bash 5+ detected - full functionality available"
else
    echo "Bash 4 or older - limited functionality"
fi
```

**Module Loading Failures**
```bash
# Check file syntax
bash -n bash/core/loader.bash

# Enable debug mode
export DEBUG_LOADER=true
source bash/core/loader.bash

# Check module status
module_status "problematic_module"
```

**XDG Migration Issues**
```bash
# Check migration log
cat ~/.local/state/shell/migrations.log

# Verify directory permissions
ls -la ~/.local/

# Manual directory creation
mkdir -p ~/.config ~/.local/share ~/.cache ~/.local/state
```

## 🔍 Performance

### Optimization Features

- **Feature Caching**: Tool availability cached on first run
- **Conditional Loading**: Modules only load when their tools are available
- **Async Loading**: Non-critical modules load in background
- **Deferred Loading**: Heavy modules load only when needed

### Performance Monitoring

```bash
# Enable performance logging
export DEBUG_MODE=true

# Load performance module
load_module "performance"

# Monitor loading times
time source bash/core/loader.bash
```

## 🤝 Contributing

### Adding New Modules

1. Create your module file in the appropriate directory
2. Follow the error handling patterns:
   ```bash
   # Use provided error functions
   debug "Module loading started"
   info "Configuration applied"
   warning "Optional feature not available"
   error "Critical configuration missing"
   ```

3. Register your module:
   ```bash
   register_module "my_module" "features/my_module.bash" "sync" "" "error_handler"
   ```

4. Test your module:
   ```bash
   export DEBUG_MODE=true
   load_module "my_module"
   module_status "my_module"
   ```

### Code Style

- Use `snake_case` for function names
- Include error handling for all operations
- Add debug logging for important operations
- Validate inputs using provided validation functions
- Follow XDG patterns for file locations

---

This infrastructure provides a solid foundation for a modern, maintainable Bash configuration that scales well and degrades gracefully across different environments.
