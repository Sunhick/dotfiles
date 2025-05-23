;;; pkg-slim-programming-mode.el --- package initializer  -*- lexical-binding: t -*-
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

(require 'base-ensure)

(defvar programming-pkgs
  '(yaml-mode
    ;; gitignore-mode
    protobuf-mode
    groovy-mode
    rust-mode
    ;; cmake-mode
    ;; rjsx-mode
    markdown-mode
    gn-mode
    google-c-style
    typescript-mode
    )
  "Support for programming in emacs")

(defun install-programming-pkgs ()
  "Install programming packages"
  (dolist (pkg programming-pkgs)
    (ensure-pkg pkg)))

(install-programming-pkgs)

(provide 'pkg-slim-programming-mode)

;;; pkg-slim-programming-mode.el ends here
