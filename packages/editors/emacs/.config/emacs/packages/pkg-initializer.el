;;; pkg-initializer.el --- package initializer  -*- lexical-binding: t -*-
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

;; Base packages setup
(require 'pkg-fzf)
;; (require 'pkg-ido)
;; (require 'pkg-magit)
(require 'pkg-vertico)
(require 'pkg-multiple-cursors)
;; (require 'pkg-guide-key)
;; (require 'pkg-which-key)
(require 'pkg-ripgrep)
(require 'pkg-smartparens)
;; (require 'pkg-switch-windows)
(require 'pkg-windmove)
(require 'pkg-autocomplete)
(require 'pkg-eglot)
(require 'pkg-org-mode)

;; Add-on packages (uncomment if required)
(require 'pkg-slim-programming-mode)
(require 'pkg-go-mode)
;; (require 'pkg-react-native)
(require 'pkg-typescript-mode)
(require 'pkg-gn-mode)
(require 'pkg-google-c-style)
(require 'pkg-rust-mode)


(provide 'pkg-initializer)

;;; pkg-initializer.el ends here
