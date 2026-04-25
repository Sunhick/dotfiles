;;
;; debug.el - is only for debugging purposes and shouldn't be
;;        taken as a replacement for init.el or settings.org.
;;
;; $ emacs -q --load path/to/debug.el
;;
;; Author : Sunil
;; Email : sunhick@gmail.com
;;
;; copyright (c) 2018. All rights reserved.
;;

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
             '("melpa" . "https://melpa.org/packages/"))

(custom-set-variables
 '(inhibit-startup-screen t))
