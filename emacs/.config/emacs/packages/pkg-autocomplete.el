;;; pkg-autocomplete.el --- Completion UI (corfu)  -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2026 Sunil
;;
;; Author: Sunil <sunhick@gmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Corfu provides a lightweight completion popup that works with
;; Emacs's built-in completion-at-point (CAPF), which eglot uses.

;;; Code:

(require 'corfu)

;; Enable corfu globally
(global-corfu-mode 1)

;; Show popup after short delay
(setq corfu-auto t)
(setq corfu-auto-delay 0.2)
(setq corfu-auto-prefix 2)

;; Quit on exact match or no match
(setq corfu-quit-no-match 'separator)

(provide 'pkg-autocomplete)

;;; pkg-autocomplete.el ends here
