;;; init.el --- emacs configurations
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

(defvar emacs-dir (file-name-directory load-file-name)
  "Directory hosting the emacs configuration files")

(defvar emacs-base-dir (expand-file-name "base" emacs-dir)
  "Emacs base directory housing the heart of emacs configurations")

(defvar emacs-user-dir (expand-file-name "user" emacs-dir)
  "Emacs user's directory")

(defvar emacs-themes-dir (expand-file-name "themes" emacs-dir)
  "Directory hosting the themes for emacs")

(defvar emacs-packages-dir (expand-file-name "packages" emacs-dir)
  "Emacs packages directory boostrapping the installed packages.
Most of them will be keybindings for packages installed by default.")

(add-to-list 'load-path emacs-base-dir)
(add-to-list 'load-path emacs-user-dir)
(add-to-list 'load-path emacs-themes-dir)
(add-to-list 'load-path emacs-packages-dir)

(require 'base-init)
(require 'user-setup)
(require 'setup-theme)

(require 'pkg-fzf)
;;; init.el ends here
