;;; base-registers.el --- base registers configurations
;;
;; Copyright (c) 2019-2020 Sunil
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

;; https://www.emacswiki.org/emacs/Registers
;; key bind: C-x r j e
(set-register ?e '(file . "~/.emacs.d"))
(set-register ?b '(file . "~/.bashrc"))

;; use dir-locals.el to configure registers on per project needs
;; you can also have two git repos inside same folder using --git-dir
;; to version control dir-locals

(provide 'base-registers)

;;; base-registers.el ends here
