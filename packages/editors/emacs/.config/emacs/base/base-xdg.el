;;; base-xdg.el --- XDG Base Directory Specification compliance  -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2024 Sunil
;;
;; Author: Sunil <sunhick@gmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; XDG Base Directory Specification compliance for Emacs
;; Ensures all data, cache, and config files follow XDG standards

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

;; XDG Base Directory variables (already defined in init.el, but ensure they exist)
(defvar xdg-config-home
  (or (getenv "XDG_CONFIG_HOME")
      (expand-file-name "~/.config"))
  "XDG config directory")

(defvar xdg-data-home
  (or (getenv "XDG_DATA_HOME")
      (expand-file-name "~/.local/share"))
  "XDG data directory")

(defvar xdg-cache-home
  (or (getenv "XDG_CACHE_HOME")
      (expand-file-name "~/.cache"))
  "XDG cache directory")

;; Ensure directories exist
(make-directory (expand-file-name "emacs" xdg-data-home) t)
(make-directory (expand-file-name "emacs" xdg-cache-home) t)

;; Package archives cache
(setq package-user-dir (expand-file-name "emacs/elpa" xdg-data-home))

;; Auto-save files
(setq auto-save-list-file-prefix (expand-file-name "emacs/auto-save-list/.saves-" xdg-data-home))

;; Backup files
(setq backup-directory-alist
      `(("." . ,(expand-file-name "emacs/backups" xdg-cache-home))))

;; Undo tree files (if undo-tree is used)
(setq undo-tree-history-directory-alist
      `(("." . ,(expand-file-name "emacs/undo-tree" xdg-data-home))))

;; Recent files
(setq recentf-save-file (expand-file-name "emacs/recentf" xdg-data-home))

;; Bookmarks
(setq bookmark-default-file (expand-file-name "emacs/bookmarks" xdg-data-home))

;; Eshell
(setq eshell-directory-name (expand-file-name "emacs/eshell" xdg-data-home))

;; URL cache
(setq url-cache-directory (expand-file-name "emacs/url" xdg-cache-home))

;; Tramp
(setq tramp-persistency-file-name (expand-file-name "emacs/tramp" xdg-data-home))

;; Org mode
(setq org-id-locations-file (expand-file-name "emacs/org-id-locations" xdg-data-home))

;; Projectile (if used)
(setq projectile-cache-file (expand-file-name "emacs/projectile.cache" xdg-cache-home))
(setq projectile-known-projects-file (expand-file-name "emacs/projectile-bookmarks.eld" xdg-data-home))

;; Magit (if used)
(setq magit-repository-directories-file (expand-file-name "emacs/magit-repos" xdg-data-home))

;; Company (if used)
(setq company-statistics-file (expand-file-name "emacs/company-statistics-cache.el" xdg-data-home))

;; Helm (if used)
(setq helm-adaptive-history-file (expand-file-name "emacs/helm-adaptive-history" xdg-data-home))

;; Ivy/Counsel (if used)
(setq ivy-views-file (expand-file-name "emacs/ivy-views" xdg-data-home))

;; Vertico (already configured, but ensure persistence)
(setq vertico-save-file (expand-file-name "emacs/vertico-save" xdg-data-home))

;; Savehist
(setq savehist-file (expand-file-name "emacs/history" xdg-data-home))

;; ELPA keyring
(setq package-gnupghome-dir (expand-file-name "emacs/gnupg" xdg-data-home))

(provide 'base-xdg)

;;; base-xdg.el ends here
