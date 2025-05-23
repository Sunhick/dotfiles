;;; base-compile-elisp.el --- base init configurations -*- lexical-binding: t -*-
;;
;; Copyright (c) 2025-2026 Sunil
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

;; Enable deferred native compilation
(setq native-comp-deferred-compilation t)

; ; prioritize compilation speed
(setq native-comp-speed 3)

(defun compile-emacs-d-native ()
  "Native compile all .el files under ~/.emacs.d recursively."
  (interactive)
  (when (fboundp 'native-compile-async)
    (native-compile-async "~/.emacs.d" 'recursively)))

;; (add-hook 'emacs-startup-hook #'compile-emacs-d-native)

(provide 'base-compile-elisp)

;;; base-compile-elisp.el ends here
