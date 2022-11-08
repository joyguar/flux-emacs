;;; init.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Daniel Lucas
;;
;; Author: Daniel Lucas <dclucas@protonmail.com>
;; Maintainer: Daniel Lucas <dclucas@protonmail.com>
;; Created: May 26, 2022
;; Modified: May 26, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/danielclucas/finit
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; These are my personal Emacs configurations. Please refer to the README for information on how to run and modify them.
;;
;;  Description
;;
;;; Code:

;; Since we might be running in CI or other environments, stick to
;; XDG_CONFIG_HOME value if possible.
(let ((emacs-home (if-let ((xdg (getenv "XDG_CONFIG_HOME")))
                      (expand-file-name "flux-emacs/" xdg)
                    user-emacs-directory)))
  ;; Add Lisp directory to `load-path'.
  (add-to-list 'load-path (expand-file-name "lisp" emacs-home)))

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold
                             normal-gc-cons-threshold))))

;; hello world
(setq-default user-full-name "Daniel Lucas"
              user-mail-address "dclucas@protonmail.com")

;; bootstrap
(require 'config-path)
(require 'init-packages)

;; Setup `custom-file`.
(setq custom-file (concat path-local-dir "custom.el"))

;; load autoloads file
(unless elpa-bootstrap-p
  (unless (file-exists-p path-autoloads-file)
    (error "Autoloads file doesn't exist, please run '%s'"
           "eru install emacs"))
  (load path-autoloads-file nil 'nomessage))

;; core
(require 'init-env)
(require 'init-kbd)
(require 'init-editor)
(require 'init-ui)
(require 'init-buffer)
(require 'init-window)

;; utilities
(require 'init-organum)
(require 'init-completion)
(require 'init-dired)
(require 'init-magit)
(require 'init-debug)
(require 'init-pdf)
(require 'init-term)
(require 'init-shell)
(require 'init-project)
(require 'init-grep)
(require 'init-refs)
(require 'init-gpg)
(require 'init-notmuch)

;; languages
(require 'init-python)

;; I don't use `customize' interface, but .dir-locals.el put 'safe'
;; variables into `custom-file'. And to be honest, I hate to allow
;; them every time I restart Emacs.
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(provide 'init)
;;; init.el ends here
