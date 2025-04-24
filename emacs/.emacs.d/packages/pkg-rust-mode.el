;;; pkg-rust-mode.el --- go mode
;;
;; Copyright (c) 2024-2025 Sunil
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

(require 'base-ensure)

;; Load rust-mode for .rs files
(require 'rust-mode)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; Enable eglot for Rust
(add-hook 'rust-mode-hook #'eglot-ensure)

;; Optional: format Rust code on save
(add-hook 'rust-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq rust-format-on-save t)))


(provide 'pkg-rust-mode)

;;; pkg-rust-mode.el ends here
