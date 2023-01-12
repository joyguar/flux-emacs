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
;; repositories. Enable configuration via `elpa-use-package'.
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

;; bootstrap `elpaca'

(declare-function elpaca-generate-autoloads "elpaca")
(defvar elpaca-directory (expand-file-name "elpaca/" path-packages-dir))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(when-let ((elpaca-repo (expand-file-name "repos/elpaca/" elpaca-directory))
           (elpaca-build (expand-file-name "elpaca/" elpaca-builds-directory))
           (elpaca-target (if (file-exists-p elpaca-build) elpaca-build elpaca-repo))
           (elpaca-url  "https://www.github.com/progfolio/elpaca.git")
           ((add-to-list 'load-path elpaca-target))
           ((not (file-exists-p elpaca-repo)))
           (buffer (get-buffer-create "*elpaca-bootstrap*")))
  (condition-case-unless-debug err
      (progn
        (unless (zerop (call-process "git" nil buffer t "clone" elpaca-url elpaca-repo))
          (error "%s" (list (with-current-buffer buffer (buffer-string)))))
        (byte-recompile-directory elpaca-repo 0 'force)
        (require 'elpaca)
        (elpaca-generate-autoloads "elpaca" elpaca-repo)
        (kill-buffer buffer))
    ((error)
     (delete-directory elpaca-directory 'recursive)
     (with-current-buffer buffer
       (goto-char (point-max))
       (insert (format "\n%S" err))
       (display-buffer buffer)))))
(require 'elpaca-autoloads)
(autoload 'elpaca--queue "elpaca")      ; needed because of byte-compilation of this file
(elpaca (elpaca :host github :repo "progfolio/elpaca"))

(when elpa-bootstrap-p
  (elpaca-generate-autoloads "init" (expand-file-name "lisp/" path-emacs-dir)))


(defun elpa-block-until-ready ()
  "Block Emacs until all packages are installed.

Unfortunately, `elpaca' is asynchronous-only, but there are
flows (like scripts using `init'), where you need to perform
some actions *when* environment is ready."
  (if env-graphic-p
      (add-hook 'after-init-hook #'elpaca-process-queues)
    (elpaca-process-queues))
  (unless env-graphic-p
    (while (cl-find 'incomplete (reverse elpaca--queues) :key #'elpaca-q<-status)
      (message "waiting for installation to complete...")
      (sit-for 0.2))))

(defmacro elpa-require (pkg)
  "Bootstrap PKG and require it."
  `(elpaca ,pkg (require ',(elpaca--first pkg))))

(defalias #'elpa-use-package #'elpaca-use-package)

(defcustom elpa-package-form-regexp-eval
  `(concat
    ,(eval-when-compile
       (concat "^\\s-*("
               (regexp-opt '("elpa-use-package" "elpa-require" "require") t)
               "\\s-+(?\\("))
    (or
     (bound-and-true-p lisp-mode-symbol-regexp)
     "\\(?:\\sw\\|\\s_\\|\\\\.\\)+")
    "\\)")
  "Sexp providing regexp for finding elpa-* package forms in user files."
  :type 'sexp
  :group 'elpa-use-package)

(defcustom elpa-package-enable-imenu-support t
  "If non-nil, cause imenu to see `use-package' declarations.
This is done by adjusting `lisp-imenu-generic-expression' to
include support for finding `use-package' and `require' forms.
Must be set before loading use-package."
  :type 'boolean
  :set
  #'(lambda (sym value)
      (eval-after-load 'lisp-mode
        (if value
            `(add-to-list 'lisp-imenu-generic-expression
                          (list "Packages" ,elpa-package-form-regexp-eval 2))
          `(setq lisp-imenu-generic-expression
                 (remove (list "Packages" ,elpa-package-form-regexp-eval 2)
                         lisp-imenu-generic-expression))))
      (set-default sym value))
  :group 'elpa-use-package)

;; critical packages

(setq-default use-package-enable-imenu-support t)

(elpa-require use-package)
(elpa-require s)
(elpa-require dash)

;; popular packages
(elpa-use-package async
                  :defer t)

(elpa-use-package ts
                  :defer t)

(elpa-use-package request
                 :defer t
                 :init
                 (setq-default
                  request-storage-directory (expand-file-name "request" path-cache-dir)))

(elpa-use-package request-deferred
                  :defer t)

;; profiler
(elpa-use-package esup
                  :defer t
                  :init
                  ;; https://github.com/progfolio/elpaca/issues/23
                  (setq esup-death 0))

(provide 'init-elpa)
;;; init-elpa.el ends here
