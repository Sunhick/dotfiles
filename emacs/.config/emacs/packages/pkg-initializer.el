;;; pkg-initializer.el --- All package declarations (use-package)  -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2026 Sunil
;;
;; Author: Sunil <sunhick@gmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Single file for all package configuration using use-package.
;; Each declaration handles install, defer, keybindings, and config.

;;; Code:

;; ── Completion framework (vertico + orderless + marginalia) ──────

(use-package vertico
  :init (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init (marginalia-mode))

;; ── Completion at point (corfu) ─────────────────────────────────

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match 'separator)
  :init (global-corfu-mode 1))

;; ── Git ─────────────────────────────────────────────────────────

(use-package magit
  :defer t
  :bind ("C-x g" . magit-status)
  :init
  (setq vc-handled-backends (delq 'Git vc-handled-backends))
  (setq epg-pinentry-mode 'loopback)
  :config
  ;; Limit log to 25 commits when showing signatures (prevents slow GPG verify)
  (setq magit-log-section-commit-count 25))

;; ── Search ──────────────────────────────────────────────────────

(use-package fzf
  :defer t
  :bind (("C-c f f" . fzf-git-files)
         ("C-c f g" . fzf-git)
         ("C-c f d" . fzf-directory)
         ("C-c f s" . fzf-git-grep)
         ("C-c f p" . fzf-projectile)))

(use-package rg
  :defer t
  :commands (rg rg-project rg-dwim)
  :config (rg-enable-menu))

;; ── Editing ─────────────────────────────────────────────────────

(use-package multiple-cursors
  :defer t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package smartparens
  :defer 1
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (diminish 'smartparens-strict-mode)
  (smartparens-global-mode t))

;; ── UI helpers ──────────────────────────────────────────────────

(use-package which-key
  :diminish
  :custom
  (which-key-popup-type 'minibuffer)
  (which-key-idle-delay 0.5)
  (which-key-show-early-on-C-h t)
  (which-key-idle-secondary-delay 0.05)
  :config (which-key-mode))

(use-package windmove
  :ensure nil ; built-in
  :config
  (windmove-default-keybindings)
  (setq windmove-wrap-around t))

;; ── Org mode ────────────────────────────────────────────────────

(use-package org
  :defer t)

(use-package org-bullets
  :defer t
  :hook (org-mode . org-bullets-mode))

;; ── LSP (eglot — built into Emacs 29+) ─────────────────────────

(use-package eglot
  :ensure nil ; built-in
  :defer t
  :hook (python-mode . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  :config
  (add-hook 'eglot-managed-mode-hook #'eldoc-mode))

;; ── Language modes ──────────────────────────────────────────────

;; Tree-sitter modes (Emacs 29+) — better syntax highlighting & navigation
;; Grammars install automatically on first use via treesit-auto
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Remap major modes to tree-sitter variants where available
(setq major-mode-remap-alist
      '((python-mode    . python-ts-mode)
        (js-mode        . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode      . json-ts-mode)
        (css-mode       . css-ts-mode)
        (yaml-mode      . yaml-ts-mode)
        (bash-mode      . bash-ts-mode)
        (c-mode         . c-ts-mode)
        (c++-mode       . c++-ts-mode)
        (rust-mode      . rust-ts-mode)
        (go-mode        . go-ts-mode)
        (toml-mode      . toml-ts-mode)))

;; Eglot hooks for tree-sitter modes
(dolist (hook '(python-ts-mode-hook
               rust-ts-mode-hook
               go-ts-mode-hook
               typescript-ts-mode-hook))
  (add-hook hook #'eglot-ensure))

;; Fallback non-treesit packages (used when grammar not available)
(use-package go-mode
  :defer t
  :hook (go-mode . (lambda ()
                     (add-hook 'before-save-hook #'gofmt-before-save nil t)))
  :bind (:map go-mode-map
         ("C-c C-r" . go-remove-unused-imports)
         ("C-c C-g" . go-goto-imports)
         ("C-c C-f" . gofmt)))

(use-package rust-mode
  :defer t
  :hook ((rust-mode . eglot-ensure)
         (rust-mode . (lambda ()
                        (setq indent-tabs-mode nil)
                        (setq rust-format-on-save t)))))

(use-package typescript-mode
  :defer t
  :custom (typescript-indent-level 2))

(use-package yaml-mode :defer t)
(use-package protobuf-mode :defer t)
(use-package groovy-mode :defer t)
(use-package markdown-mode :defer t)

(use-package gn-mode
  :defer t
  :mode "\\.gn\\'")

(use-package google-c-style
  :defer t
  :hook ((c-mode-common . google-set-c-style)
         (c-mode-common . google-make-newline-indent)))

;; ── Diff / Merge ────────────────────────────────────────────────

(use-package ediff
  :ensure nil ; built-in
  :defer t
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-diff-options "-w"))

;; ── Misc ────────────────────────────────────────────────────────

(use-package restclient :defer t)
(use-package gruvbox-theme :defer t)
(use-package zenburn-theme :defer t)

;; ── Time-based theme switching ──────────────────────────────────
;; Day (7am–7pm): gruvbox-dark-hard
;; Night (7pm–7am): modus-vivendi

(defun my/load-theme-by-time ()
  "Load theme based on time of day."
  (let ((hour (string-to-number (format-time-string "%H"))))
    (mapc #'disable-theme custom-enabled-themes)
    (if (and (>= hour 7) (< hour 19))
        (load-theme 'gruvbox-dark-hard t)
      (load-theme 'modus-vivendi t))))

(my/load-theme-by-time)

;; Re-check every hour
(run-at-time "01:00:00" 3600 #'my/load-theme-by-time)

;; ── Eshell prompt ────────────────────────────────────────────────

(use-package eshell
  :ensure nil ; built-in
  :defer t
  :config
  (defun my/eshell-prompt ()
    "Custom eshell prompt: > dirname >"
    (let* ((last-status eshell-last-command-status)
           (dir (abbreviate-file-name (eshell/pwd)))
           (basename (file-name-nondirectory (directory-file-name dir)))
           (basename (if (string= basename "") "~" basename))
           (green "#98971a")
           (red "#cc241d")
           (cyan "#689d6a")
           (white "#ebdbb2"))
      (concat
       (propertize ">" 'face `(:foreground ,(if (= last-status 0) green red)))
       " "
       (propertize basename 'face `(:foreground ,cyan))
       " "
       (propertize ">" 'face `(:foreground ,white))
       " ")))

  (setq eshell-prompt-function #'my/eshell-prompt)
  (setq eshell-prompt-regexp "^> .* > ")
  (setq eshell-highlight-prompt nil))

;; ── Shell-mode (fix bash prompt rendering) ──────────────────────

(use-package shell
  :ensure nil ; built-in
  :defer t
  :config
  ;; Interpret ANSI color codes so bash prompt renders correctly
  (add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on)
  ;; Track directory changes from the shell
  (add-hook 'shell-mode-hook #'dirtrack-mode)
  ;; Match our prompt pattern for dirtrack
  (add-hook 'shell-mode-hook
            (lambda ()
              (setq dirtrack-list '("^> \\(.*?\\) > " 1)))))

(provide 'pkg-initializer)

;;; pkg-initializer.el ends here
