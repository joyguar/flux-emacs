;;; lib-start.el --- Description -*- lexical-binding: t; -*-
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
;; Utility to restart emacs.
;;
;;  Description
;;
;;; Code:

(defun flux-launch-separate-emacs-in-terminal ()
  (suspend-emacs "fg ; emacs -nw"))

(defun flux-launch-separate-emacs-under-x ()
  (call-process "sh" nil nil nil "-c" "emacs &"))

;;;###autoload
(defun flux-restart-emacs ()
  (interactive)
  ;; We need the new emacs to be spawned after all kill-emacs-hooks
  ;; have been processed and there is nothing interesting left
  (let ((kill-emacs-hook (append kill-emacs-hook (list (if (display-graphic-p)
                                                           #'flux-launch-separate-emacs-under-x
                                                         #'flux-launch-separate-emacs-in-terminal)))))
    (save-buffers-kill-emacs)))

;;;lib-start.el ends here
