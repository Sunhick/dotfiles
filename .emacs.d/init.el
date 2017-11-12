;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Author : Sunil <sunhick@gmail.com>
;; Emacs customization file
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq user-full-name "Sunil")
(setq user-mail-address "sunhick@gmail.com")

;; start package.el with emacs
(require 'package)
(require 'json)

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
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
;;(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; initialize package.el
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
  :ensure t)

;; configure which key for help on key strokes
(require 'which-key)
(which-key-mode)

;; configure emacs ido package for auto completion
;; when i press C-x (CTRL+x)
;; (require 'ido)
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)

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
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'Objective-C)
              (ggtags-mode 1))))

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

;; -i gets alias definitions from .bash_profile
(setq shell-command-switch "-ic")

;; Don't make new frames when opening a new file with Emacs
(setq ns-pop-up-frames nil)

(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

;; auto generated by emacs 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (spacemacs-dark)))
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (org-bullets try use-package auto-complete ac-emacs-eclim auto-complete-clang xcscope xcode-project which-key spacemacs-theme smooth-scrolling smex rich-minority org objc-font-lock ninja-mode neotree markdown-mode magit iedit header2 ggtags elpy dummy-h-mode counsel cmake-mode autopair auto-complete-c-headers))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'auto-complete-clang)
(setq ac-auto-start nil)
(setq ac-quick-help-delay 0.5)
;; (ac-set-trigger-key "TAB")
;; (define-key ac-mode-map  [(control tab)] 'auto-complete)
(define-key ac-mode-map  [(control tab)] 'auto-complete)
(defun my-ac-config ()
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  ;; (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))
(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
;; ac-source-gtags
(my-ac-config)

(setq ac-clang-flags
      (mapcar (lambda (item)(concat "-I" item))
              (split-string
               "
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1
 /usr/local/include
 /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/9.0.0/include
 /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include
 /usr/include
"
               )))

;; configure and bind the keystroke
;; for magit.
(require 'magit)
(global-set-key(kbd "C-x g") 'magit-status)

;; set the magit repository
(setq magit-repository-directories '( "~/prv/github/"))

;; configure emacs org mode
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
