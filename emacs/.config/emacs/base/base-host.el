;;; base-host.el --- base host configurations  -*- lexical-binding: t -*-
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

;; Set preferred font (with fallback)
;; Uses after-make-frame-functions for daemon mode compatibility
(defvar my/preferred-font "Aporetic Sans Mono")
(defvar my/font-size 15)

(defun my/set-font (&optional frame)
  "Set preferred font on FRAME, or current frame if nil."
  (when (display-graphic-p (or frame (selected-frame)))
    (with-selected-frame (or frame (selected-frame))
      (if (find-font (font-spec :name my/preferred-font))
          (set-frame-font (format "%s:pixelsize=%d" my/preferred-font my/font-size) t t)
        (message "Font '%s' not found, using default" my/preferred-font)))))

;; Apply font now (works for non-daemon startup)
(my/set-font)
;; Apply font when new frames are created (works for daemon + emacsclient)
(add-hook 'after-make-frame-functions #'my/set-font)

;; Improve vertical spacing
(setq-default line-spacing 2)

(provide 'base-host)

;;; base-host.el ends here
