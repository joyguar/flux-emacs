;;; init-ui.el --- Description -*- lexical-binding: t; -*-
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
;; Making Emacs look nice. See also `early-init'.
;;
;;  Description
;;
;;; Code:

(require 'winner)

;; no startup message
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; initial buffer
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

;; frame title
(setq frame-title-format "Flux – Flux Emacs")

;; no file dialog
(setq use-file-dialog nil)

;; no dialog box
(setq use-dialog-box nil)

;; empty line indicators
(setq indicate-empty-lines t)

;; no cursor in inactive windows
(setq cursor-in-non-selected-windows nil)

(setq initial-scratch-message nil)
(setq inhibit-default-init t)

;; start easy with few dependencies to load
(setq initial-major-mode #'fundamental-mode)

;; yet keep `text-mode' as default major mode
(setq default-major-mode #'text-mode)

;; maximum font lock
(setq font-lock-maximum-decoration t)

;; no confirmation for visiting non-existing files
(setq confirm-nonexistent-file-or-buffer nil)

;; disable cursor blinking
(blink-cursor-mode -1)

;; y/n instead of yes/no
(fset #'yes-or-no-p #'y-or-n-p)

;; no beeping and no blinking
(setq ring-bell-function #'ignore)
(setq visible-bell nil)

;; remove visual clutter
(setq indicate-buffer-boundaries nil)
(setq indicate-empty-lines nil)

;; don't resize emacs in steps
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)

;; enable bidirectional text
(setq bidi-disable-reordering 'left-to-right)

(elpa-use-package all-the-icons
  :if (display-graphic-p))

(elpa-use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq
   doom-modeline-hight 1
   doom-modeline-window-width-limit 90
   doom-modeline-minor-modes t))

(elpa-use-package doom-themes
 :ensure t
 :config
 (setq doom-themes-enable-bold t
       doom-themes-enable-italic t)
 (load-theme 'doom-one t)

 (doom-themes-visual-bell-config)
 (doom-themes-org-config))

(elpa-use-package solaire-mode
  :config
  (solaire-global-mode +1))

(elpa-use-package dashboard
                  :functions (winner-undo)
                  :init
                  (setq
                   dashboard-banner-logo-title "FLUX EMACS"
                   dashboard-startup-banner (expand-file-name "banner.txt" user-emacs-directory)
                   dashboard-center-content t
                   dashboard-show-shortcuts t
                   dashboard-projects-backend 'project-el
                   dashboard-items '((recents . 10)
  		                             (bookmarks . 5)
  		                             (projects . 5))

                   dashboard-set-init-info t
                   dashboard-heading-icons '((recents   . "history")
                                             (bookmarks . "bookmark")
                                             (agenda    . "calendar")
                                             (projects  . "briefcase")
                                             (registers . "database"))

                   dashboard-set-footer nil)
                  
                  (dashboard-setup-startup-hook))

(elpa-use-package frames-only-mode
  :hook (after-init . frames-only-mode))

(elpa-use-package rainbow-delimeters
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'init-ui)
;;; init-ui.el ends here
