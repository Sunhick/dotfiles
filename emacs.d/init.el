;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Author : Sunil
;; Email : sunhick@gmail.com
;;
;; copyright (c) 2018. All rights reserved.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
(package-initialize)

(setq user-full-name "Sunil"
      user-mail-address "sunhick@gmail.com")

;; start package.el with emacs
(require 'package)

;; add MELPA to repository list
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-refresh-contents)
     (package-install package)))

(use-package org
    :ensure org)

(require 'org)

(setq vc-follow-symlinks t)
(org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))