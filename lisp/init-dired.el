;;; init-dired.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Daniel Lucas
;;
;; Author: Daniel Lucas <dclucas@protonmail.com>
;; Maintainer: Daniel Lucas <dclucas@protonmail.com>
;; Created: June 23, 2022
;; Modified: June 23, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/danielclucas/lib-org-agenda
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Configurations for `dired'.
;;
;;  Description
;;
;;; Code:

(use-package dired
  :straight nil
  :init
  (setq
   dired-listing-switches "-alh --group-directories-first"
   dired-recursive-copies 'always
   dired-recursive-deletes 'top
   dired-auto-revert-buffer t
   dired-hide-details-hide-symlink-targets nil))

(provide 'init-dired)
;;; init-dir-editor.el ends here

