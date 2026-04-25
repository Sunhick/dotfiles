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
  (setq vc-handled-backends (delq 'Git vc-handled-backends)))

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
(use-package diminish)

(use-package windmove
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

;; ── Eshell prompt (matches bash ❯ dirname ❯ style) ─────────────

(use-package eshell
  :ensure nil ; built-in
  :defer t
  :config
  (defun my/eshell-prompt ()
    "Custom eshell prompt matching bash prompt: ❯ dirname ❯"
    (let* ((last-status eshell-last-command-status)
           (dir (abbreviate-file-name (eshell/pwd)))
           (basename (file-name-nondirectory (directory-file-name dir)))
           (basename (if (string= basename "") "~" basename))
           (branch (when (executable-find "git")
                     (let ((b (string-trim
                               (shell-command-to-string
                                "git rev-parse --abbrev-ref HEAD 2>/dev/null"))))
                       (unless (string= b "") b))))
           ;; Gruvbox-inspired colors
           (green "#98971a")
           (red "#cc241d")
           (cyan "#689d6a")
           (blue "#458588")
           (white "#ebdbb2"))
      (concat
       ;; Status arrow
       (propertize "❯" 'face `(:foreground ,(if (= last-status 0) green red)))
       " "
       ;; Directory
       (propertize basename 'face `(:foreground ,cyan))
       ;; Git branch
       (when branch
         (concat " " (propertize (format "(%s)" branch) 'face `(:foreground ,blue))))
       ;; Final arrow
       " "
       (propertize "❯" 'face `(:foreground ,white))
       " ")))

  (setq eshell-prompt-function #'my/eshell-prompt)
  (setq eshell-prompt-regexp "^❯ .* ❯ ")
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
              (setq dirtrack-list '("^❯ \\(.*?\\) ❯ " 1)))))

(provide 'pkg-initializer)

;;; pkg-initializer.el ends here
