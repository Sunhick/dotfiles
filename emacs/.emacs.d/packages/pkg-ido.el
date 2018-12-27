;;; pkg-ido.el --- Ido configurations
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

(require 'ido)
(require 'flx-ido)
(require  'ido-vertical-mode)
(require 'ido-completing-read+)

;; configure ido
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(ido-everywhere t)
(ido-mode 1)

;; configure flx-ido
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)

;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; configure ido vertical mode
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; Stop searching for files in other directories. when all i want
;; is just create a new file. If i want to search i will fzf or
;; revert to dired buffers.
(setq ido-auto-merge-work-directories-length -1)

;; configure ido-completing-read+
(ido-ubiquitous-mode 1)

(provide 'pkg-ido)

;;; pkg-ido.el ends here
