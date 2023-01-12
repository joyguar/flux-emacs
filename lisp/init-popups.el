;;; init-popups.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Daniel Lucas
;;
;; Author: Daniel Lucas <dclucas@protonmail.com>
;; Maintainer: Daniel Lucas <dclucas@protonmail.com>
;; Created: June 24, 2022
;; Modified: June 24, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/danielclucas/init-window
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Making Emacs look nice. See also `early-init'.
;;
;;  Description
;;
;;; Code:

;; (use-package popper
;;   :hook (emacs-startup . popper-mode)
;;   :custom (setq popper-window-height 40)
;;   :init
;;   (setq popper-reference-buffers
;;         '(("^\\Compile-Log\\*$" . hide)
;;           ("^\\*compilations\\*" . hide)
;;           "^\\*Messages\\*$"
;;           "^\\*Completions"
;;           "^\\*Calc:"
;;           "^\\*undo-tree\\*"
;;           "^\\*info\\*$"
;;           "^\\*eldoc\\*"
;;           "^\\*Local variables\\*$"
;;           "^\\*\\(?:Wo\\)?Man "
;;           "^\\*\\([Hh]elp\\|Apropos\\)"
;;           "[Oo]utput\\*$"
;;           "^\\*TeX errors\\*"
;;           "^\\*TeX Help\\*"
;;           "^\\*Warnings"
;;           "^\\*Backtrace"
;;           "^\\*CPU-Profiler-Report "
;;           "^\\*Memory-Profiler-Report "
;;           "^\\*Process List\\*"
;;           "\\*Async Shell Command\\*"
;;           "\\*Shell Command Output\\*"
;;           "\\*Agenda Commands\\*"
;;           "\\*Org Select\\*"
;;           ))
;;   :config
;;   (advice-add #'kbd-escape :before #'popper-close-window-hack))

;; (use-package popup-mode
;;   :demand t
;;   :hook (after-init . +popup-mode)
;;   :straight (popup-mode :host github :repo "aaronjensen/emacs-popup-mode"))

(provide 'init-popups)
;;;init-popups.el ends here
