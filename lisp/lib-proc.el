;;; lib-proc.el --- Description -*- lexical-binding: t; -*-
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
;; Utilities for working with processes
;;
;;  Description
;;
;;; Code::

(defun add-process-sentinel (sentinel &optional process)
  "Add SENTINEL to PROCESS.
PROCESS defaults to the process of the current buffer.
Use this function with care.
If there is already a process sentinel SENTINEL is used as after-advice.
That can fail if the process sentinel is reset by some other function."
  (unless process
    (setq process (get-buffer-process (current-buffer))))
  (let ((old (process-sentinel process)))
    (cond
     ((symbolp old)
      (advice-add old :after sentinel))
     ((null old)
      (set-process-sentinel process sentinel))
     (t (warn "Cannot set sentinel %S for process %S." sentinel process)))))

(provide 'lib-proc)
;;; lib-proc.el ends here
