;;; init.el --- emacs configuration entry point
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
  "Emacs configuration root directory")

(defvar emacs-slim-dir (expand-file-name "slim" emacs-dir)
  "Emacs slim directory that hosts the base packages to be installed
and configured")

(defvar emacs-theme-dir (expand-file-name "themes" emacs-dir)
  "Emacs themes provider directory")

(defvar emacs-vendor-dir (expand-file-name "vendor" emacs-dir)
  "Emacs vendor directory hosting the packages that are installed
externally")

(defconst melpa-archive '("melpa" . "https://melpa.org/packages/"))

(mapc (lambda (dir) (add-to-list 'load-path dir))
      `(,emacs-slim-dir ,emacs-theme-dir ,emacs-vendor-dir))

(require 'slim-configurator)

(let ((emacs (slim-configurator :archives melpa-archive)))
  (slim-configure emacs))

;;; init.el ends here
