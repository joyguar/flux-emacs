;;; lib-window.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Daniel Lucas
;;
;; Author: Daniel Lucas <dclucas@protonmail.com>
;; Maintainer: Daniel Lucas <dclucas@protonmail.com>
;; Created: June 24, 2022
;; Modified: June 24, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/danielclucas/init-ui
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Making window and popups behave nicely
;;
;;  Description
;;
;;; Code:

;; (require 'popper)

;; ;;;###autoload
;; (defun popper-close-window-hack (&rest _)
;;       "Close popper window via `C-g'."
;;       ;; `C-g' can deactivate region
;;       (when (and (called-interactively-p 'interactive)
;;                  (not (region-active-p))
;;                  popper-open-popup-alist)
;;         (let ((window (caar popper-open-popup-alist)))
;;           (when (window-live-p window)
;;             (delete-window window)))))

(provide 'lib-window)
;;; lib-window.el ends here
