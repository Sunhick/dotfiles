;;; base-packages.el --- Package management with use-package  -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2026 Sunil
;;
;; Author: Sunil <sunhick@gmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Sets up package.el, MELPA, and use-package (built into Emacs 29+).
;; All package declarations live in pkg-initializer.el.

;;; Code:

;; Ensure Emacs can find system tools (git, etc.) when running as daemon
;; macOS GUI apps don't inherit shell PATH
(dolist (dir '("/opt/homebrew/bin" "/usr/local/bin" "/usr/bin"))
  (unless (member dir exec-path)
    (push dir exec-path)))
(setenv "PATH" (mapconcat #'identity exec-path ":"))

(require 'package)

(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; use-package is built into Emacs 29+
(require 'use-package)

;; Auto-install missing packages from MELPA
(setq use-package-always-ensure t)

;; Diminish — loaded early because base-emacs.el needs it
(use-package diminish)

;; Enable editorconfig support
(use-package editorconfig
  :config (editorconfig-mode 1))

(provide 'base-packages)

;;; base-packages.el ends here
