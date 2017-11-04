;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Author : Sunil <sunhick@gmail.com>
;; Emacs customization file
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; start package.el with emacs
(require 'package)
(require 'json)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (swiper header2 magit xcscope xcode-project which-key smex objc-font-lock ninja-mode neotree markdown-mode iedit ggtags elpy dummy-h-mode cmake-mode autopair auto-complete-c-headers))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; set the emacs window size to 160x60
;; (add-to-list 'default-frame-alist '(height . 60))
;; (add-to-list 'default-frame-alist '(width . 160))

;; Get rid of tool bar and menu bar
;; (menu-bar-mode 0)
;; Yeah, I'm keeping the text menu at the top. It doesn't
;; bother me. Whereas the toolbar i never use it and just
;; occupies space.
(tool-bar-mode 0)
;;(scroll-bar-mode 1)

;; turn off annoying bell sound in Mac OS
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; I hate typing. Especially when emacs prompts
;; me with yes/no and i type 'y' or 'n' in a hurry
;; remap yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Set the display font to ubuntu. On Os's that doesn't have ubuntu font
;; download and the link below and extract the files to your system font folder.
;; You can freely download ubuntu fonts from http://font.ubuntu.com/
;; (set-default-font "Ubuntu Mono-14")

;; add MELPA to repository list
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
;;(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; initialize package.el
(package-initialize)

;; configure which key for help on key strokes
(require 'which-key)
(which-key-mode)

;; configure emacs ido package for auto completion
;; when i press C-x (CTRL+x)
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; configure emacs smex package. This is basically
;; emacs ido for M-x (ESC-x aka META-x)
(autoload 'smex "smex"
  "Smex is a M-x enhancement for Emacs, it provides a convenient interface to
  your recently and most frequently used commands.")
(global-set-key (kbd "M-x") 'smex)

;; start auto-complete with emacs
(require 'auto-complete)

;; do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; start yasnippet with emacs
(require 'yasnippet)
(yas-global-mode 1)

;; let's define a function which initializes auto-complete-c-headers and gets called for c/c++ hooks
(defun my:ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include")
  )

;; now let's call this function from c/c++ hooks
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)

;; Fix iedit bug in Mac
(define-key global-map (kbd "C-c ;") 'iedit-mode)

;; Fixing another key binding bug in iedit mode
(define-key global-map (kbd "C-c o") 'iedit-mode)

;; Enable neo tree view by default
(require 'neotree)
(neotree-toggle)
;; (setq-default neo-theme 'arrow)
;; (setq neo-window-fixed-size nil)
(setq neo-smart-open t)

;; enable auto pair mode to automatically insert closing parenthesis.
(require 'autopair)
(autopair-global-mode 1)

;; Show matching parenthensis
(show-paren-mode 1)

;; Support for Objective-C mode
(require 'dummy-h-mode)		      
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

;; configure ggtags in c/c++/java mode for faster code navigations.
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

;; change the mode line in emacs
;; (sml/setup)
;; (setq sml/theme 'light)

;; Backups at .saves folder in the current folder
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.saves"))    
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

;; Add copyright and file header
;; (autoload 'auto-make-header "header2")
;; (add-hook 'emacs-lisp-mode-hook 'auto-make-header)
;; (add-hook 'c-mode-common-hook   'auto-make-header)
;; (defun copyright() (
;; 		    insert "/* Copyright (c) 2017 Amazon.com */\n\n"))
;; (add-hook 'make-header-hook 'copyright)
;; (add-hook 'c-mode-common-hook 'copyright)

;; configure tabbar
;; (require 'tabbar)
;; (tabbar-mode 1)

;; configure swipper for easy searching and navigations.
;; counsel is required for the C-x C-f to work. swiper
;; does install the counsel but need require line to
;; make it work.
(require 'counsel)
(require 'swiper)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
