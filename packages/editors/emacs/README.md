# Emacs Configuration Package

This package provides an XDG-compliant Emacs configuration that follows modern standards for file organization.

## Features

- **XDG Base Directory Specification compliance**
- Modular configuration structure
- Package management with MELPA
- Sensible defaults for development
- Clean separation of config, data, and cache files

## Installation

Use GNU Stow to install this package:

```bash
stow -t ~ packages/editors/emacs
```

## XDG Compliance

This configuration is fully XDG compliant:

- **Config**: `~/.config/emacs/` - Configuration files
- **Data**: `~/.local/share/emacs/` - User data (packages, history, bookmarks)
- **Cache**: `~/.cache/emacs/` - Temporary and cache files

## Migration from ~/.emacs.d

If you have an existing `~/.emacs.d` setup, run the migration script:

```bash
./migrate-to-xdg.sh
```

This will move your existing data to the appropriate XDG locations.

## Configuration Structure

```
.config/emacs/
├── init.el              # Main entry point
├── base/                # Core configuration modules
│   ├── base-xdg.el     # XDG compliance setup
│   ├── base-init.el    # Base initialization
│   ├── base-packages.el # Package management
│   └── ...
├── packages/           # Package-specific configurations
├── themes/            # Theme configurations
├── user/              # User-specific settings
└── vendor/            # Third-party packages
```

## Dependencies

- Emacs 27.1 or later
- Internet connection for package installation

## Customization

Edit `user/user-setup.el` to customize user-specific settings like name and email.

The configuration automatically installs essential packages from MELPA on first run.
