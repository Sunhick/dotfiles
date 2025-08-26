;;; base-packages.el --- base package configurations  -*- lexical-binding: t -*-
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

(require 'package)
(require 'base-ensure)

;; Package initialization happens after XDG directories are set up
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
(package-initialize)

;; add melpa to repository list
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; do not bloat this list of packages
(defvar base-packages
  '(auto-complete
    diminish
    fzf
    vertico
    marginalia
    orderless
    multiple-cursors
    org
    org-bullets
    smartparens
    windmove
    zenburn-theme
    rg
    restclient
    magit
    gruvbox-theme
    ;; modus-themes
    ;; flx-ido
    ;; solarized-theme
    ;; which-key
    ;; ido
    ;; ido-completing-read+
    ;; ido-vertical-mode
    ;; ag
    ;; switch-window
    ;; twilight-theme
    )
  "Base emacs packages to be installed from melpa")

(defun install-base-packages ()
  "Install the list of base packages"
  (dolist (pkg base-packages)
    (ensure-pkg pkg)))

(install-base-packages)

(provide 'base-packages)

;;; base-packages.el ends here
