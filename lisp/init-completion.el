;;; init-selection.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Daniel Lucas
;;
;; Author: Daniel Lucas <dclucas@protonmail.com>
;; Maintainer: Daniel Lucas <dclucas@protonmail.com>
;; Created: June 22, 2022
;; Modified: June 22, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/danielclucas/init-selection
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'config-path)

(use-package vertico
  :init
  (vertico-mode)

  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :commands (marginalia-mode
             marginalia-cycle)
  :init
  (marginalia-mode)

  (setq-default marginalia-annotators
                '(marginalia-annotators-heavy
                  marginalia-annotators-light
                  nil)))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
	    completion-category-defaults nil))

(use-package consult
  :general
  (leader-def
    "pr" '(consult-ripgrep :which-key "Ripgrep the project")
    "bb" '(consult-buffer :which-key "Switch buffer")))
    
(provide 'init-completion)
;;; init-selection.el ends here
