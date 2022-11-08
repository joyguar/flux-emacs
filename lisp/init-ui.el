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

(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq
   doom-modeline-hight 1
   doom-modeline-window-width-limit 90
   doom-modeline-minor-modes t))

(use-package modus-themes
  :init
  (setq modus-themes-operandi-color-overrides '((fg-main . "#000000")
						(bg-main . "#faf8f5")
						(bg-region . "#efdfff")
						(bg-inactive . "#e6e4e1")
						(bg-hl-line . "#e6e4e1")))
  (setq modus-themes-vivendi-color-overrides '((fg-main . "#fdf3ec")
					       (bg-main . "#24242d")
					       (bg-region . "#4f3d88")
					       (bg-inactive . "#2f2f3b")
					       (bg-hl-line . "#2f2f3b"))))

(use-package doom-themes
 :ensure t
 :config
 (setq doom-themes-enable-bold t
       doom-themes-enable-italic t)
 (load-theme 'doom-one t)

 (doom-themes-visual-bell-config)
 (doom-themes-org-config))

(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(use-package dashboard
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

(setup (:straight rainbow-delimiters))

(setup (:straight origami))

(provide 'init-ui)
;;; init-ui.el ends here
