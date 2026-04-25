# Emacs Configuration

Personal Emacs config — XDG compliant, managed with use-package, Gruvbox Dark Hard theme.

## Structure

```
~/.config/emacs/
├── init.el                  # Entry point — sets load paths, requires modules
├── base/
│   ├── base-init.el         # Loads all base modules in order
│   ├── base-xdg.el          # XDG Base Directory paths for all data/cache
│   ├── base-compile-elisp.el # Native compilation settings
│   ├── base-ui.el           # Frame, line numbers, whitespace, visual bell
│   ├── base-window.el       # Toolbar, scrollbar, fringe, pixel scroll
│   ├── base-host.el         # Font, line spacing (machine-specific)
│   ├── base-packages.el     # MELPA + use-package + diminish setup
│   └── base-emacs.el        # Core editor behavior (encoding, parens, mouse)
├── packages/
│   └── pkg-initializer.el   # All package declarations (single file)
├── user/
│   └── user-setup.el        # User identity (name, email)
└── debug.el                 # Standalone debug config (emacs -q --load debug.el)
```

## XDG Compliance

| Purpose | Location |
|---------|----------|
| Config | `~/.config/emacs/` |
| Data (packages, history, bookmarks) | `~/.local/share/emacs/` |
| Cache (backups, url cache) | `~/.cache/emacs/` |

## Packages

All managed via `use-package` in `pkg-initializer.el`. See that file for the full list.

## Keybindings

See [emacs-keybindings.md](emacs-keybindings.md) for a comprehensive reference.

## Font

Iosevka Comfy at 15px (`base-host.el`). Falls back to default if not installed.

## Theme

Gruvbox Dark Hard — loaded via use-package in `pkg-initializer.el`.

## References

- [Prelude](https://github.com/bbatsov/prelude)
- [emacs.d - Magnars](https://github.com/magnars/.emacs.d)
- [XDG Base Directory Specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
- [use-package](https://github.com/jwiegley/use-package)
