;;; config-organum.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Daniel Lucas
;;
;; Author: Daniel Lucas <dclucas@protonmail.com>
;; Maintainer: Daniel Lucas <dclucas@protonmail.com>
;; Created: June 23, 2022
;; Modified: June 23, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/danielclucas/config-org
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'config-path)

(defvar organum-directory (expand-file-name "organum" path-home-dir))

(defvar organum-inbox-file (expand-file-name "gtd/inbox.org" organum-directory))

(defvar organum-bibliography-directory (expand-file-name "pkm/bibliographies" organum-directory))

(defvar organum-bibliography-file (expand-file-name "bibliography.bib" organum-bibliography-directory))

(defvar organum-references-directory (expand-file-name "pkm/refnotes" organum-directory))

(provide 'config-organum)
;;; config-org.el ends here
