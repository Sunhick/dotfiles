# Emacs Configuration

This Emacs configuration follows the XDG Base Directory Specification for better organization and compliance with modern standards.

## XDG Compliance

This configuration stores files in the following XDG-compliant locations:

- **Config**: `~/.config/emacs/` (this directory)
- **Data**: `~/.local/share/emacs/` (packages, history, bookmarks, etc.)
- **Cache**: `~/.cache/emacs/` (backups, temporary files)

### Data Files Location

The following files are stored in `~/.local/share/emacs/`:
- Package installations (`elpa/`)
- Custom settings (`customs.el`)
- Recent files (`recentf`)
- Bookmarks (`bookmarks`)
- Save places (`places`)
- Auto-save list (`auto-save-list/`)
- Undo tree history (`undo-tree/`)
- Eshell data (`eshell/`)
- Org ID locations (`org-id-locations`)
- Tramp persistence (`tramp`)
- Various package-specific data files

### Cache Files Location

The following files are stored in `~/.cache/emacs/`:
- Backup files (`backups/`)
- URL cache (`url/`)
- Projectile cache (if used)

## Migration from ~/.emacs.d

If you have an existing `~/.emacs.d` directory, you can migrate your data:

```bash
# Create XDG directories
mkdir -p ~/.local/share/emacs ~/.cache/emacs

# Migrate data files (adjust paths as needed)
mv ~/.emacs.d/elpa ~/.local/share/emacs/ 2>/dev/null || true
mv ~/.emacs.d/recentf ~/.local/share/emacs/ 2>/dev/null || true
mv ~/.emacs.d/bookmarks ~/.local/share/emacs/ 2>/dev/null || true
mv ~/.emacs.d/.places ~/.local/share/emacs/places 2>/dev/null || true
mv ~/.emacs.d/auto-save-list ~/.local/share/emacs/ 2>/dev/null || true
mv ~/.emacs.d/eshell ~/.local/share/emacs/ 2>/dev/null || true

# Migrate cache files
mv ~/.emacs.d/backups ~/.cache/emacs/ 2>/dev/null || true
mv ~/.emacs.d/url ~/.cache/emacs/ 2>/dev/null || true

# Rename customs file
mv ~/.emacs.d/.customs.el ~/.local/share/emacs/customs.el 2>/dev/null || true
```

## References
* [Prelude](https://github.com/bbatsov/prelude)
* [emacs.d - Magnars](https://github.com/magnars/.emacs.d)
* [XDG Base Directory Specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
