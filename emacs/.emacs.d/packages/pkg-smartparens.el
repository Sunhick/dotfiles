;;; pkg-smartparens.el --- smartparens configurations
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

(require 'smartparens)
(require 'diminish)
(require 'smartparens-config)

;; (global-set-key (kbd "M-m m j") 'sp-down-sexp)
;; (global-set-key (kbd "M-m m k") 'sp-backward-up-sexp)
;; (global-set-key (kbd "M-m m h") 'sp-backward-down-sexp)
;; (global-set-key (kbd "M-m m l") 'sp-up-sexp)
;; (global-set-key (kbd "M-m m f") 'sp-forward-sexp)
;; (global-set-key (kbd "M-m m b") 'sp-backward-sexp)
;; (global-set-key (kbd "M-m m a") 'sp-beginning-of-sexp)
;; (global-set-key (kbd "M-m m e") 'sp-end-of-sexp)
;; (global-set-key (kbd "M-m m n") 'sp-next-sexp)
;; (global-set-key (kbd "M-m m p") 'sp-previous-sexp)
;; (global-set-key (kbd "M-m m >") 'sp-forward-barf-sexp)
;; (global-set-key (kbd "M-m m <") 'sp-backward-barf-sexp)
;; (global-set-key (kbd "M-m m )") 'sp-forward-slurp-sexp)
;; (global-set-key (kbd "M-m m (") 'sp-backward-slurp-sexp)
;; (global-set-key (kbd "M-m m x") 'sp-transpose-sexp)
;; (global-set-key (kbd "M-m m d") 'sp-kill-sexp)
;; (global-set-key (kbd "M-m m y") 'sp-copy-sexp)
;; (global-set-key (kbd "M-m m u") 'sp-unwrap-sexp)
;; (global-set-key (kbd "M-m m U") 'sp-backward-unwrap-sexp)
;; (global-set-key (kbd "M-m m C") 'sp-convolute-sexp)
;; (global-set-key (kbd "M-m m r") 'sp-raise-sexp)
;; (global-set-key (kbd "M-m m s") 'sp-split-sexp)
;; (global-set-key (kbd "M-m m R") 'sp-rewrap-sexp)
;; (global-set-key (kbd "M-m m S") 'sp-splice-sexp)
;; (global-set-key (kbd "M-m m F") 'sp-splice-sexp-killing-forward)
;; (global-set-key (kbd "M-m m B") 'sp-splice-sexp-killing-backward)
;; (global-set-key (kbd "M-m m A") 'sp-splice-sexp-killing-around)

(diminish smartparens-mode)
(diminish smartparens-strict-mode)

(smartparens-global-mode)

(provide 'pkg-smartparens)

;;; pkg-smartparens.el ends here
