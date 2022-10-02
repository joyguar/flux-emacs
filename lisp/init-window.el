;;; init-window.el --- Description -*- lexical-binding: t; -*-
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

(use-package popper
  :hook (emacs-startup . popper-mode)
  :init
  (setq popper-reference-buffers
	'("\\*Messages\\*"
	  "\\*Help\\*"
	  "Output\\*$"
	  "\\*Compile-Log\\*"
	  "\\*Warnings\\*"
	  "\\*Async Shell Command\\*"
	  help-mode
	  helpful-mode
	  compilation-mode
	  Buffer-menu-mode
	  "\\*Agenda Commands\\*"
	  "\\*Org Select\\*"))
  :config
  (advice-add #'kbd-escape :before #'popper-close-window-hack))

(provide 'init-window)
;;;init-window.el ends here
