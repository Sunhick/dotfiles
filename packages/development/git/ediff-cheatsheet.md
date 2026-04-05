# Ediff Cheatsheet

Quick reference for Emacs ediff used as git difftool/mergetool.

IMPORTANT: All keybindings work in the control panel only.
Use `C-x o` to switch to it from the diff buffers.

## Getting Started

```bash
# Diff uncommitted changes
git difftool

# Diff between commits
git difftool HEAD~1 HEAD

# Resolve merge conflicts
git mergetool

# Use vscode instead
git difftool -t vscode
git mergetool -t vscode
```

## Navigation

| Key | Action                              |
|-----|-------------------------------------|
| `n` | Next difference                     |
| `p` | Previous difference                 |
| `j` | Jump to a specific difference       |
| `g a` | Go to diff in buffer A            |
| `g b` | Go to diff in buffer B            |

## Diff Mode (2-way: git difftool)

| Key | Action                              |
|-----|-------------------------------------|
| `a` | Copy region from A to B             |
| `b` | Copy region from B to A             |
| `r a` | Restore region in buffer A        |
| `r b` | Restore region in buffer B        |

## Merge Mode (3-way: git mergetool)

In merge mode: A = LOCAL (your branch), B = REMOTE (incoming), C = MERGED (result).

| Key | Action                              |
|-----|-------------------------------------|
| `a` | Choose version A (LOCAL) for merge  |
| `b` | Choose version B (REMOTE) for merge |
| `d` | Choose ancestor (BASE) for merge    |
| `r` | Restore original merge region       |
| `/` | Show ancestor (BASE) in a buffer    |
| `+` | Combine A and B in merge buffer     |

### Merge Workflow

1. `C-x o` to focus the control panel
2. `n` / `p` to navigate between conflicts
3. `a` or `b` to pick a side for each conflict
4. `q` to quit and save — git picks up the result

## Display

| Key   | Action                            |
|-------|-----------------------------------|
| `|`   | Toggle vertical/horizontal split  |
| `#h`  | Toggle highlighting               |
| `#f`  | Toggle narrowing to diff region   |
| `v`   | Scroll down both buffers          |
| `V`   | Scroll up both buffers            |
| `<`   | Scroll left both buffers          |
| `>`   | Scroll right both buffers         |
| `~`   | Swap buffers A and B              |

## Window Management

| Key   | Action                            |
|-------|-----------------------------------|
| `w a` | Save buffer A                     |
| `w b` | Save buffer B                     |
| `w c` | Save buffer C (merge result)      |
| `!`   | Recompute differences             |
| `m`   | Toggle wide display (full frame)  |
| `z`   | Suspend ediff session             |

## Session Control

| Key   | Action                            |
|-------|-----------------------------------|
| `q`   | Quit ediff (saves merge result)   |
| `?`   | Show help / all key bindings      |
| `E`   | Open ediff session registry       |
| `D`   | Show raw diff output in a buffer  |
| `i`   | Show session status info          |

## Config (pkg-ediff.el)

These settings are applied via the dotfiles emacs config:

- Control panel in same frame (no popup): `ediff-setup-windows-plain`
- Side-by-side layout: `split-window-horizontally`
- Ignore whitespace: `ediff-diff-options "-w"`
