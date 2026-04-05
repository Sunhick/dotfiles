;;; pkg-ediff.el --- ediff configuration  -*- lexical-binding: t -*-
;;
;; Author: Sunil <sunhick@gmail.com>

;;; Commentary:

;; Ediff settings for a better git difftool/mergetool experience.
;; Note: ediff keybindings (n/p/q) only work in the control panel.
;; Use C-x o to switch to it.

;;; Code:

;; Keep control panel in the same frame (no separate tiny window)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Side-by-side diffs instead of top/bottom
(setq ediff-split-window-function 'split-window-horizontally)

;; Ignore whitespace differences
(setq ediff-diff-options "-w")

(provide 'pkg-ediff)

;;; pkg-ediff.el ends here
