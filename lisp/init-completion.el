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

(elpa-use-package vertico
  :init
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t)
  :config
  (vertico-mode +1))

(elpa-use-package savehist
  :init
  (savehist-mode))

(elpa-use-package marginalia
  :commands (marginalia-mode
             marginalia-cycle)
  :init
  (marginalia-mode)

  (setq-default marginalia-annotators
                '(marginalia-annotators-heavy
                  marginalia-annotators-light
                  nil)))

(elpa-use-package orderless
  :init
  (setq completion-styles '(orderless basic)
	    completion-category-defaults nil))

(elpa-use-package consult
  :preface
  (global-set-key [remap apropos] #'consult-apropos)
  (global-set-key [remap bookmark-jump] #'consult-bookmark)
  ;; (global-set-key [remap evil-show-marks] #'consult-mark)
  ;; (global-set-key [remap evil-show-jumps] #'+vertico/jump-list)
  ;; (global-set-key [remap evil-show-registers] #'consult-register)
  (global-set-key [remap goto-lines] #'consult-goto-line)
  (global-set-key [remap imenu] #'consult-imenu)
  (global-set-key [remap locate] #'consult-locate)
  (global-set-key [remap load-theme] #'consult-theme)
  (global-set-key [remap man] #'consult-man)
  (global-set-key [remap recentf-open-files] #'consult-recent-file)
  (global-set-key [remap switch-to-buffer] #'consult-buffer)
  (global-set-key [remap switch-to-buffer-other-window] #'consult-buffer-other-window)
  (global-set-key [remap switch-to-buffer-other-frame] #'consult-buffer-other-frame)
  (global-set-key [remap yank-pop] #'consult-yank-pop)
  ;; (global-set-key [remap persp-switch-to-buffer] #'+vertico/switch-workspace-buffer)
  (global-set-key (kbd "C-x /") #'consult-ripgrep)
  :config
  (setq consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay 0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1)

  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult--source-buffer consult-xref consult--source-recent-file
   consult--source-bookmark consult--source-project-buffer
   :preview-key (kbd "C-SPC")
   consult-theme
   :preview-key (list :debounce 0.5 (kbd "C-SPC")))

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (setq register-preview-delay 1.0
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window))

(elpa-use-package consult-dir
  :preface
  (global-set-key (kbd "C-x C-d") #'consult-dir)
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x c-j" . consult-dir-jump-file)))
  
(elpa-use-package consult-recoll
  :preface
  (global-set-key (kbd "M-s /") #'consult-recoll)
  :config
  (setq consult-recoll-inline-snippets nil)
  (consult-customize consult-recoll :preview-key '(kbd "C-SPC")))

(elpa-use-package corfu
  :config
  (setq
   corfu-cycle t
   corfu-auto t
   corfu-separator ?\s
   corfu-quit-at-boundary nil
   corfu-quit-no-match nil
   corfu-preselect-first nil
   corfu-on-exact-match nil))

(provide 'init-completion)
;;; init-selection.el ends here
