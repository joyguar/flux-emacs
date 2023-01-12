;;; init-shell.el --- Description -*- lexical-binding: t; -*-
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

(require 'init-elpa)
(require 'lib-proc)

(defun flux-shell-mode-hook ()
  "Custom `shell-mode' behaviours."
  ;; Kill the buffer when the shell process exits.
  (add-process-sentinel
   (lambda (process _event)
     (and (memq (process-status process) '(exit signal))
          (buffer-live-p (process-buffer process))
          (kill-buffer (process-buffer process))))))

(elpa-use-package shell
  :hook ((shell-mode . flux-shell-mode-hook)))

(elpa-use-package coterm
  :config
  (coterm-mode 1))
  
(elpa-use-package eshell
  :defer t
  :init
  (setq
   eshell-history-file-name (expand-file-name "eshell/history" path-cache-dir)
   eshell-last-dir-ring-file-name nil))

(elpa-use-package sh-script
  :commands (sh-shell-process)
  :config
  ;; setup indentation
  (setq sh-indent-after-continuation 'always
        sh-basic-offset tab-width))

(defun sh-repl ()
  "Open a shell REPL."
  (let* ((dest-sh (symbol-name sh-shell))
         (sh-shell-file dest-sh)
         (dest-name (format "*shell [%s]*" dest-sh)))
    (sh-shell-process t)
    (with-current-buffer "*shell*"
      (rename-buffer dest-name))
    (get-buffer dest-name)))

(provide 'init-shell)
;;; init-shell.el ends here
