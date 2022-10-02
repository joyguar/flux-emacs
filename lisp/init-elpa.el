;;; init-elpa.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Daniel Lucas
;;
;; Author: Daniel Lucas <dclucas@protonmail.com>
;; Maintainer: Daniel Lucas <dclucas@protonmail.com>
;; Created: May 26, 2022
;; Modified: May 26, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/danielclucas/init-elpa
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Setup Emacs for installing packages from MELPA and Git
;; repositories. Enable configuration via `use-package'.
;;
;;  Description
;;
;;; Code:

(require 'config-path)

(defvar elpa-bootstrap-p nil)



(setq package-user-dir
      (expand-file-name
       "elpa/"
       path-packages-dir))


;; bootstrap straight.el

(setq-default
 straight-repository-bxranch "develop"
 straight-check-for-modifications nil
 straight-use-package-by-default t
 straight-base-dir path-packages-dir
 straight-profiles (list
                    (cons nil
                          (expand-file-name
                           "versions/default.el"
                           path-emacs-dir))))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         path-packages-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         (concat "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el")
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; use-package

(setq-default
 use-package-enable-imenu-support t)
(straight-use-package 'use-package)



(use-package el-patch
  :straight t)



;; popular packages

(use-package s)
(use-package dash)
(use-package async)
(use-package f)




;; profiler
(use-package esup :defer t)



(provide 'init-elpa)
;;; init-elpa.el ends here
