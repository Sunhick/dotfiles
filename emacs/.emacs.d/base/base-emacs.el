;;; base-emacs.el --- base emacs editor behaviour
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

;; customs in a separate file
(setq custom-file "~/.emacs.d/.customs.el")
(if (file-exists-p custom-file)
    (load custom-file))

;; gc to run when memory reaches 20MB
(setq gc-cons-threshold 20000000)

;; save the previous locations of opened files
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

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
(setq delete-by-moving-to-trash t)
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

;; setup tramp
(require 'tramp)
(setq tramp-default-method "ssh")
;; (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; follow symbolic links to the actual files when editing
(setq vc-follow-symlinks t)

(provide 'base-emacs)

;;; base-emacs.el ends here
