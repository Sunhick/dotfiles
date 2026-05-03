;;; base-ui.el --- base ui configurations  -*- lexical-binding: t -*-
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

;; (when (window-system)
;;   (require 'base-window)
;;   (setq initial-frame-alist
;;         '((top . 0)
;;           (left . 50)
;;           (width . 205)
;;           (height . 67))))
(require 'base-window)

;; Open frames maximized (fills screen, stays in current workspace)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; For daemon mode: apply to each new client frame
(defun my/setup-frame (&optional frame)
  "Configure FRAME as maximized and bring to foreground."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      (set-frame-parameter nil 'fullscreen 'maximized)
      (when (eq system-type 'darwin)
        (ns-do-applescript "tell application \"Emacs\" to activate")))))

(add-hook 'after-make-frame-functions #'my/setup-frame)

;; Apply host settings
(require 'base-host)

;; Emacs title bar customizations
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Flash modeline briefly instead of audible bell
(setq visible-bell nil)
(setq ring-bell-function
      (lambda ()
        (let ((orig-bg (face-background 'mode-line)))
          (set-face-background 'mode-line "#d65d0e") ; gruvbox orange
          (run-with-timer 0.1 nil
                          (lambda (bg) (set-face-background 'mode-line bg))
                          orig-bg))))

;; no start up screen please
(setq inhibit-startup-screen t)

;; no title on the title bar
(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)
(setq frame-resize-pixelwise t)

;; Get rid of tool bar and menu bar
(menu-bar-mode -1)

;; I hate typing. Especially when emacs prompts
;; me with yes/no and i type 'y' or 'n' in a hurry
;; remap yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable visual line mode
(global-visual-line-mode 1)

;; Trash can support
(setq delete-by-moving-to-trash t)

;; 80 chars is a good width.
(set-default 'fill-column 80)

;; No blinking cursor
(blink-cursor-mode 0)

;; Display line & column numbers
(setq line-number-mode t)
(setq column-number-mode t)

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(setq-default display-line-numbers-current-absolute nil)

;; use whitespaces instead of tabs
(setq-default indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; whitespace visualization (only in code/text buffers, not magit/help/etc)
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook (lambda () (setq show-trailing-whitespace t))))

;; Show empty lines indicator
(set-default 'indicate-empty-lines t)

;; Show comments in italics in programming modes
(custom-set-faces
 '(font-lock-comment-face ((t (:slant italic))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face)))))

;; ibuffer is a better choice
(defalias 'list-buffers 'ibuffer)

(provide 'base-ui)

;;; base-ui.el ends here
