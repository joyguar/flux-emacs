;;; init-packages.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Daniel Lucas
;;
;; Author: Daniel Lucas <dclucas@protonmail.com>
;; Maintainer: Daniel Lucas <dclucas@protonmail.com>
;; Created: May 26, 2022
;; Modified: May 26, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/danielclucas/init-packages
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
 straight-repository-branch "develop"
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


;; setup.el

(straight-use-package '(setup :type git
                              :host nil
                              :repo "https://git.sr.ht/~pkal/setup"))
(require 'setup)

(with-eval-after-load 'imenu
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setf (map-elt imenu-generic-expression "Setup")
                    (list (rx line-start (0+ blank)
                              "(setup" (1+ blank)
                              (or (group-n 1 (1+ (or (syntax word)
                                                     (syntax symbol))))
                                  ;; Add here items that can define a feature:
                                  (seq "(:" (or "straight" "require" "package")
                                       (1+ blank)
                                       (group-n 1 (1+ (or (syntax word)
                                                          (syntax symbol)))))))
                          1)))))

(setup-define :straight
  (lambda (recipe)
    `(unless (straight-use-package ',recipe)
       ,(setup-quit)))
  :documentation
  "Install RECIPE with `straight-use-package'.
This macro can be used as HEAD, and will replace itself with the
first RECIPE's package."
  :repeatable t
  :shorthand (lambda (sexp)
               (let ((recipe (cadr sexp)))
                 (if (consp recipe)
                     (car recipe)
                   recipe))))

(setup-define :straight-when
  (lambda (recipe condition)
    `(if ,condition
         (straight-use-package ',recipe)
       ,(setup-quit)))
  :documentation
  "Install RECIPE with `straight-use-package' when CONDITION is met.
If CONDITION is false, stop evaluating the body.  This macro can
be used as HEAD, and will replace itself with the RECIPE's
package.  This macro is not repeatable."
  :repeatable nil
  :indent 1
  :shorthand (lambda (sexp)
               (let ((recipe (cadr sexp)))
                 (if (consp recipe) (car recipe) recipe))))

(setup-define :delay
   (lambda (&rest time)
     `(run-with-idle-timer ,(or time 1)
                           nil ;; Don't repeat
                           (lambda () (require ',(setup-get 'feature)))))
   :documentation "Delay loading the feature until a certain amount of idle time has passed.")

(setup-define :load-after
    (lambda (features &rest body)
      (let ((body `(progn
                     (require ',(setup-get 'feature))
                     ,@body)))
        (dolist (feature (if (listp features)
                             (nreverse features)
                           (list features)))
          (setq body `(with-eval-after-load ',feature ,body)))
        body))
    :documentation "Load the current feature after FEATURES.")

(setup-define :advise
    (lambda (symbol where function)
      `(advice-add ',symbol ,where ,function))
  :documentation "Add a piece of advice on a function.
See `advice-add' for more details."
  :after-loaded t
  :debug '(sexp sexp function-form)
  :ensure '(nil nil func)
  :repeatable t)


(setup (:straight el-patch))
  

;; popular packages

(setup (:straight s))
(setup (:straight dash))
(setup (:straight async))
(catch 'setup-quit
  (if
      (straight-use-package 'f)
      nil
    (throw 'setup-quit nil)))


;; profiler
(catch 'setup-quit
  (if
      (straight-use-package 'esup)
      nil
    (throw 'setup-quit nil))
  (run-with-idle-timer 1 nil
                       (function
                        (lambda nil
                          (require 'esup)))))


(provide 'init-packages)
;;; init-packages.el ends here
