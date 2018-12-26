;;; slim-pkg.el --- base package configurations
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

(defclass slim-pkg ()
  ((name
    :type string
    :initarg :name
    :documentation "package name")

   (version
    :type string
    :initarg :version
    :initform "latest"
    :documentation "version of this package. Defaults to latest if
nothing is specified")

   (source
    :type symbol
    :initarg :source
    :initform :melpa
    :documentation "Where this package should be installed from.
Defaults to stable melpa respository"))

  "Slim package class. Captures the package, version, source of
installation e.g. Melpa, Gnu, Org etc")

(cl-defgeneric slim-pkg-configure ((pkg slim-pkg))
  "configure the given package")

;;; slim-pkg.el ends here
