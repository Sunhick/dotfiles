;;; base-emacs.el --- base emacs editor behaviour  -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2019 Sunil
;;
;; Author: Sunil <sunhick@gmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Emacs configuration file

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'diminish)

;; follow unix EOF
(setq require-final-newline t)

;; customs in a separate file (XDG compliant)
(setq custom-file (expand-file-name "emacs/customs.el"
                                    (or (getenv "XDG_DATA_HOME")
                                        (expand-file-name "~/.local/share"))))
(if (file-exists-p custom-file)
    (load custom-file))

;; Raise GC threshold during init for faster startup, reset after
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 20 1024 1024))))

;; save the previous locations of opened files (XDG compliant)
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name "emacs/places"
                                        (or (getenv "XDG_DATA_HOME")
                                            (expand-file-name "~/.local/share"))))

;; show matching parens
(show-paren-mode 1)

;; Dired -- specific to macos
(setq dired-use-ls-dired nil)
;; revert dired buffer on re-visting
(setq dired-auto-revert-buffer t)

;; diminish unwanted minor modes from modeline
(diminish 'visual-line-mode)
(eval-after-load "eldoc" '(diminish 'eldoc-mode))

;; osx move deleted files to trash can
(setq trash-directory "~/.Trash")

;; set file encoding format
(setq default-buffer-file-coding-system 'utf-8-unix)

;; cycle through amounts of spacing
(global-set-key (kbd "M-SPC") 'cycle-spacing)

;; keep cursor at same position when scrolling
(setq scroll-preserve-screen-position 1)

;; Enable mouse support
(xterm-mouse-mode 1)
(unless window-system
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; Terminal frames (emacs -nw / emacsclient -t in Ghostty or tmux):
;; - setSelection/getSelection: kill and yank go through the system
;;   clipboard via OSC 52 (also over SSH).  Ghostty allows both
;;   (clipboard-read/write = allow); tmux needs `set-clipboard on`.
;; - modifyOtherKeys: lets C-<punct> and C-S-<key> chords reach Emacs.
;; Read by term/xterm.el, which also loads for TERM=xterm-ghostty.
(defvar xterm-extra-capabilities)
(setq xterm-extra-capabilities '(getSelection setSelection modifyOtherKeys))

;; Terminal frames draw the side-by-side window divider with ASCII `|',
;; which shows as a dashed line.  Use the box-drawing `│' (U+2502) so it
;; joins into a solid line.  GUI frames ignore this slot.  Stacked
;; windows have no divider in a terminal; the mode line separates them.
(unless standard-display-table
  (setq standard-display-table (make-display-table)))
(set-display-table-slot standard-display-table 'vertical-border
                        (make-glyph-code ?│ 'vertical-border))

;; Run a server from the GUI so `emacsclient -t' (the `et' alias and
;; $EDITOR) opens in this session instead of starting a second daemon.
(when (display-graphic-p)
  (require 'server)
  (unless (server-running-p)
    (server-start)))

;; setup tramp (deferred — tramp is heavy)
(with-eval-after-load 'tramp
  (setq tramp-default-method "ssh"))

;; enable savehist for command history
(savehist-mode 1)

;; enable recentf for recent files
(recentf-mode 1)

;; enable repeat-mode (Emacs 28+) — repeat last key without prefix
;; e.g. C-x o o o to cycle windows, C-x ^ ^ ^ to enlarge
(repeat-mode 1)

;; follow symbolic links to the actual files when editing
(setq vc-follow-symlinks t)

(provide 'base-emacs)

;;; base-emacs.el ends here
