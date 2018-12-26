;;; slim-pkg-provider.el --- Slim pkg provider
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

(require 'eieio)
(require 'slim-pkg)

(defvar slim-pkg-lists
  `(,(slim-pkg :name 'ido)
    ,(slim-pkg :name 'ido-completing-read+)
    ,(slim-pkg :name 'ido-vertical-mode)
    ,(slim-pkg :name 'magit))
  "slim package list")

(cl-defstruct slim-pkg-provider "Slim package provider.
These package come preinstalled with this configuration")

(cl-defmethod provider-pkg-list ((p slim-pkg-provider))
  slim-pkg-lists)

(provide 'slim-pkg-provider)

;;; slim-pkg-provider.el ends here
