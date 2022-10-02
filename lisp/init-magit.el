;;; init-magit.el --- Description -*- lexical-binding: t; -*-
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
;;; Code:

(leader-def
  "g" '(nil :which-key "git...")
  "gs" '(magit-stage-file :which-key "stage file")
  "gu" '(magit-unstage-file :which-key "unstage file")
  "g[" '(git-gutter:previous-hunk :which-key "previous hunk")
  "g]" '(git-gutter:next-hunk :which-key "next hunk")
  "gd" '(git-dispatch :which-key "dispatch")
  "gf" '(git-find-file :which-key "find-file")
  "gg" '(git-status :which-key "status")
  "gi" '(magit-init :which-key "initialize repo")
  "gt" '(git-timemachine-toggle :which-key "time machine"))

(use-package magit
  :defer t
  :commands (magit-stage-file
             magit-unstage-file)
  :init
  (setq-default magit-git-executable (executable-find "git"))
  ;; Must be set early to prevent ~/.config/flux/transient from being created
  (setq transient-levels-file (expand-file-name "transient/levels" path-etc-dir)
        transient-values-file (expand-file-name "transient/values" path-etc-dir)
        transient-history-file (expand-file-name "transient/history" path-etc-dir)))

(provide 'init-magit)
;;; init-vcs.el ends here
