;;; pkg-eglot.el --- Eglot
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

;; Enable Eglot for certain programming modes
(add-hook 'python-mode-hook #'eglot-ensure)
(add-hook 'emacs-lisp-mode-hook #'eglot-ensure)
;; (add-hook 'rust-mode-hook #'eglot-ensure)
;; (add-hook 'js-mode-hook #'eglot-ensure)
;; (add-hook 'typescript-mode-hook #'eglot-ensure)
;; (add-hook 'c-mode-hook #'eglot-ensure)

;; Optional: UI improvements
(setq eglot-autoshutdown t)            ; shuts down LSP server when no longer needed
(setq eglot-extend-to-xref t)          ; enable xref-based navigation even without project
(add-hook 'eglot-managed-mode-hook #'eldoc-mode)

(provide 'pkg-eglot)
;;; pkg-eglot.el ends here
