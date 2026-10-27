;;; base-keys.el --- keyboard bindings tuned for Miryoku  -*- lexical-binding: t -*-
;;
;; Copyright (c) 2026 Sunil
;;
;; Author: Sunil <sunhick@gmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Bindings tuned for a Totem running stock Miryoku (QWERTY alphas,
;; default NAV and clipboard options), shared by macOS and Linux.
;;
;; Miryoku facts these bindings rely on:
;;   - Home-row mods GACS on both hands: A/' = GUI (Cmd / Super),
;;     S/L = Alt (Meta), D/K = Ctrl, F/J = Shift.  Both hands send the
;;     *left* modifier; Right Alt only comes from AltGr (hold X or .).
;;   - NAV layer (hold Space): arrows on J K L ', Home/PgDn/PgUp/End on
;;     M , . /, with Ctrl/Alt/Shift still on the left home row.  So
;;     MOD+arrow and MOD+Home/End are cheap opposite-hand chords.
;;   - Default clipboard keys send <undo>, <again>, S-<insert>,
;;     C-<insert> and S-<delete>.
;;
;; Rule of thumb: hold a modifier on the hand opposite the key it
;; modifies (M-x = L + x, C-c = K + c), and prefer a leader followed by
;; plain keys over chords that hold two home-row mods at once.

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

;; ── Modifiers (macOS) ──────────────────────────────────────────
;; Match Linux: Alt (ring finger) = Meta, GUI (pinky) = Super.
;; Right Option stays a plain Option key so AltGr (hold X or .) can
;; still type accented characters, matching the iTerm2 profile.
(defvar mac-option-modifier)
(defvar mac-right-option-modifier)
(defvar mac-command-modifier)
(defvar ns-option-modifier)
(defvar ns-right-option-modifier)
(defvar ns-command-modifier)

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta
        mac-right-option-modifier 'none
        mac-command-modifier 'super
        ns-option-modifier 'meta
        ns-right-option-modifier 'none
        ns-command-modifier 'super))

;; ── Window navigation ──────────────────────────────────────────
;; S-<arrow>: hold Space (NAV), hold Shift on left index, arrow on the
;; right hand.  Works in GUI and terminal frames (tmux sets xterm-keys).
(windmove-default-keybindings 'shift)

;; Org binds S-<arrow> itself; fall back to windmove where Org has
;; nothing to do (the setup recommended in the Org manual, "Conflicts").
(add-hook 'org-shiftup-final-hook #'windmove-up)
(add-hook 'org-shiftleft-final-hook #'windmove-left)
(add-hook 'org-shiftdown-final-hook #'windmove-down)
(add-hook 'org-shiftright-final-hook #'windmove-right)

;; ── Miryoku clipboard keys ─────────────────────────────────────
;; <undo> -> undo, S-<insert> -> yank, C-<insert> -> kill-ring-save and
;; S-<delete> -> kill-region are already Emacs defaults.  Map Redo
;; (<again>, and Cmd-Shift-Z for a MIRYOKU_CLIPBOARD=MAC build) when
;; `undo-redo' exists (Emacs 28+).
(when (fboundp 'undo-redo)
  (global-set-key (kbd "<again>") #'undo-redo)
  (global-set-key (kbd "<redo>") #'undo-redo)
  (global-set-key (kbd "s-Z") #'undo-redo))

;; Buffer start/end: M-< and M-> need Alt + Shift + , / . (two mods on one
;; hand).  C-<home> / C-<end> do the same and are built in: NAV + left
;; Ctrl + Home (M) / End (/).  Nothing to bind; noted here for reference.

(provide 'base-keys)

;;; base-keys.el ends here
