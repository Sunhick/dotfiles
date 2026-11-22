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

;; Frames open at the default size (not maximized or fullscreen).
;; Toggle with M-F10 (maximize) or F11 (fullscreen) when needed.

;; ── Remember frame size and position ───────────────────────────
;; The last GUI frame's geometry is saved to
;; $XDG_DATA_HOME/emacs/frame-geometry.eld when it is closed or Emacs
;; exits, and restored on the next start: size and position for the
;; first frame, size only for later frames (e.g. `emacsclient -c'), so
;; they don't stack exactly on top of each other.
(defvar my/frame-geometry-file
  (expand-file-name "emacs/frame-geometry.eld" xdg-data-home)
  "File holding the last saved GUI frame geometry.")

(defun my/frame-geometry-save (&optional frame)
  "Save FRAME's (or the selected frame's) geometry, if it is graphical."
  (let ((frame (or frame (selected-frame))))
    (when (and (frame-live-p frame) (display-graphic-p frame))
      (let* ((fullscreen (frame-parameter frame 'fullscreen))
             (pos (frame-position frame))
             (geometry
              (if fullscreen
                  `((fullscreen . ,fullscreen))
                `((left . ,(car pos))
                  (top . ,(cdr pos))
                  (width . (text-pixels . ,(frame-text-width frame)))
                  (height . (text-pixels . ,(frame-text-height frame)))))))
        (with-temp-file my/frame-geometry-file
          (prin1 geometry (current-buffer)))))))

(defun my/frame-geometry-load ()
  "Return the saved frame geometry alist, or nil."
  (when (file-readable-p my/frame-geometry-file)
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents my/frame-geometry-file)
        (read (current-buffer))))))

(defun my/frame-geometry-save-on-delete (frame)
  "Save FRAME's geometry when it is the last GUI frame being closed."
  (when (and (display-graphic-p frame)
             (not (seq-some (lambda (f) (and (not (eq f frame))
                                             (display-graphic-p f)))
                            (frame-list))))
    (my/frame-geometry-save frame)))

(let ((geometry (my/frame-geometry-load)))
  (when geometry
    (setq initial-frame-alist (append geometry initial-frame-alist))
    (dolist (key '(width height fullscreen))
      (when-let* ((param (assq key geometry)))
        (add-to-list 'default-frame-alist param)))))

(add-hook 'kill-emacs-hook #'my/frame-geometry-save)
(add-hook 'delete-frame-functions #'my/frame-geometry-save-on-delete)

;; For daemon mode: bring each new client frame to the foreground
(defun my/setup-frame (&optional frame)
  "Bring FRAME to the foreground."
  (with-selected-frame (or frame (selected-frame))
    (when (and (display-graphic-p) (eq system-type 'darwin))
      (ns-do-applescript "tell application \"Emacs\" to activate"))))

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

;; No line numbers
;; (global-display-line-numbers-mode 1)
;; (setq display-line-numbers-type 'relative)
;; (setq-default display-line-numbers-current-absolute nil)

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

;; Winner mode — undo/redo window layout changes
;; C-c <left> to undo, C-c <right> to redo
(winner-mode 1)

;; Tab-bar mode — workspaces without the visual tab bar
;; C-x t 2: new tab, C-x t 0: close, C-x t o: next, C-x t O: prev
(setq tab-bar-show nil)              ; never show the tab bar
(setq tab-bar-new-tab-choice "*scratch*")
(tab-bar-mode 1)

;; Quick tab switching — Miryoku-friendly: leader then a single NUM-layer
;; tap (C-c t 1 .. C-c t 5), instead of holding Meta while on the NUM layer.
(dotimes (i 5)
  (global-set-key (kbd (format "C-c t %d" (1+ i)))
                  (let ((n (1+ i)))
                    (lambda () (interactive) (tab-bar-select-tab n)))))

(provide 'base-ui)

;;; base-ui.el ends here
