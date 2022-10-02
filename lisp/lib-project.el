;;; lib-project.el --- Description -*- lexical-binding: t; -*-
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

(require 'lib-eval)

(require 'project)
(require 'magit)

(defun project-p ()
  "Return non-nil when located in a project."
  (project-current))

;;;###autoload
(defun project-run-shell-command ()
  "Invoke `shell-command' in the project's root."
  (interactive)
  (if-let* ((project (project-current))
            (root (project-root project)))
      (eval-with-default-dir root
                             (call-interactively #'shell-command))
    (user-error "You are not in project")))

;;;###autoload
(defun project-run-shell ()
  "Invoke `shell' in the project's root."
  (interactive)
  (if-let* ((project (project-current))
            (root (project-root project)))
      (eval-with-default-dir root (shell))
    (user-error "You are not in project")))

;;;###autoload
(defun project-magit ()
  "Start `magit-status' in the current project's root directory."
  (interactive)
  (magit-status-setup-buffer (project-root (project-current t))))

(provide 'lib-project)
;;; lib-project.el ends here
