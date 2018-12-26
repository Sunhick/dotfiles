;;; pkg-provider.el --- emacs configuration entry point
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

(cl-defstruct pkg-provider "Package provider")

(cl-defgeneric provider-pkg-list ((p pkg-provider))
  "List of packages to be installed if not installed at emacs start up.")

;; FIXME: may not be required
(cl-defgeneric provider-configure ((p pkg-provider))
  "Configure the provider which in turn may configure packages or other
providers.")

(provide 'pkg-provider)

;;; pkg-provider.el ends here
