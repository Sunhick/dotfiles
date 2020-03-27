;;; vendor-init.el --- vendor configurations
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

(defvar vendor-packages
  '(browse-kill-ring)
  "Vendor packages to be loaded into emacs")

(load "~/.emacs.d/vendor/browse-kill-ring/browse-kill-ring")
(require 'browse-kill-ring)
(global-set-key (kbd "M-y") 'browse-kill-ring)

(when (display-graphic-p)
  (load "~/.emacs.d/vendor/framemove/framemove")
  (require 'framemove)
  (framemove-default-keybindings))

(provide 'vendor-init)

;;; vendor-init.el ends here
