;;; init-kbd.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Daniel Lucas
;;
;; Author: Daniel Lucas <dclucas@protonmail.com>
;; Maintainer: Daniel Lucas <dclucas@protonmail.com>
;; Created: June 23, 2022
;; Modified: June 23, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/danielclucas/init-kbd
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Install all required packages and setup key bindings.
;;
;;  Description
;;
;;; Code:

(require 'init-packages)

(use-package general)

(general-create-definer leader-def
  :keymaps 'override
  :prefix "C-c"
  :prefix-command 'prefix-command
  :prefix-map 'prefix-map)

(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

(defvar kbd-escape-hook nil
  "A hook run after \\[keyboard-quit] is pressed.

Triggers `kbd-escape'.

If any hook returns non-nil, all hooks after it are ignored.")

(defun kbd-escape ()
  "Run the `kbd-escape-hook'."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((cl-find-if #'funcall kbd-escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((keyboard-quit))))

(global-set-key [remap keyboard-quit] #'kbd-escape)

(provide 'init-kbd)
;;; init-kbd.el ends here
