;;; init-project.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Daniel Lucas
;;
;; Author: Daniel Lucas <dclucas@protonmail.com>
;; Maintainer: Daniel Lucas <dclucas@protonmail.com>
;; Created: June 23, 2022
;; Modified: June 23, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/danielclucas/init-org
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code::

(require 'config-path)
(require 'lib-project)

(use-package project
  :straight nil  
  :general
  (leader-def
    "p" '(nil :which-key "project...")
    "p!" '(project-run-shell-command :which-key "Run shell cmd in project root")
    "ps" '(project-run-shell :which-key "Run shell in project root")
    "p/" '(project-find-regexp :which-key "Grep the project")
    "pf" '(project-find-file :which-key "Find file in project")
    "pp" '(project-switch :which-key "Switch project")
    "pc" '(project-compile :which-key "Compile project"))
  :init
  (setq project-list-file (expand-file-name "projects" path-etc-dir))
  
  (defalias 'project-switch #'project-switch-project))

(provide 'init-project)
;;; init-project.el ends here
