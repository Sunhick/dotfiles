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

;; Emacs title bar customizations
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)
(setq frame-resize-pixelwise t)

;; add MELPA to repository list
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

(setq vc-follow-symlinks t)
(org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))

;; ;; Path to our custom theme
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; (if (daemonp)
;;     (add-hook 'after-make-frame-functions
;;               (lambda (frame)
;;                 (with-selected-frame frame
;;                   (load-theme 'midnight t))))
;;   (load-theme 'midnight t))

;; disable bold and weights when we are in wombat theme
;; manually do it for now.
(mapc
 (lambda (face)
        (when (eq (face-attribute face :weight) 'bold)
          (set-face-attribute face nil :weight 'normal)))
 (face-list))

(set-default-font "Monaco")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
    ("e93ae6d348e29a5aa5637201babf4b1b64444083cdeb234ba5bda2c36ca50391" default)))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (focus gtags dumb-jump dump-jump protobuf evil protobuf-mode gitignore-mode dockerfile-mode yaml-mode haskell-mode expand-region plantuml-mode markdown-mode diminish multiple-cursors gradle-mode groovy-mode multi-term cmake-mode google-c-style smooth-scrolling auto-complete-clang auto-complete-c-headers auto-complete ggtags yasnippet-snippets yasnippet org-bullets clang-format switch-window iedit autopair which-key counsel-projectile counsel magit use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
