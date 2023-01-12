;;; init-org.el --- Description -*- lexical-binding: t; -*-
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

(require 'config-path)
(require 'init-elpa)
(require 'init-env)
(require 'config-organum)

(elpa-use-package org
  :hook ((before-save . organum-pre-save-hook)
         (org-insert-heading . organum-insert-heading-hook))
  :init
  (setq org-directory organum-directory
        org-id-locations-file (expand-file-name ".orgids" organum-directory))

  ;; formatting
  (setq
   org-adapt-indentation nil
   org-hidden-keywords nil
   org-hide-emphasis-markers nil
   org-hide-leading-stars nil
   org-pretty-entities nil
   org-startup-folded nil)

  ;; less is more
  (setf org-blank-before-new-entry '((heading . auto) (plain-list-item . auto)))
  
  ;; do not allow invisible edits
  (setq-default org-fold-catch-invisible-edits 'smart)

  ;; formatting for properties
  (setq org-property-format "%-24s %s")
  (setq org-log-done 'time)

  (setq org-list-allow-alphabetical t)
  (setq org-enforce-todo-dependencies t)

  ;; Display properties
  (setq org-cycle-separator-lines 0)
  (setq org-latex-prefer-user-labels t)
  (setq org-tags-column 0)

  ;; Set default column view headings; Task Effort Clock_Summary
  (setq org-columns-default-format (concat "%40ITEM(Task) "
                                           "%Effort(EE){:} "
                                           "%CLOCKSUM(Time Spent) "
                                           "%SCHEDULED(Scheduled) "
                                           "%DEADLINE(Deadline)"))

  ;; set tags with fast selection
  (setq org-tag-alist '(("@errand" . ?e)
                        ("@home" . ?h)
                        ("@transit" . ?t)
                        ("journals" . ?j)
                        ("fc" .?f))
        org-use-tag-inheritance t
        org-tags-exclude-from-inheritance '("people",
                                            "agenda"))

  ;; setup todo keywords
  (setq org-use-fast-todo-selection t)
  (setq org-todo-keywords
        '((sequence "TODO(t!)" "NEXT(n!)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "SOMEDAY(s)" "|" "CANCELLED(c@/!)" "MEETING")))

  ;; setup todo faces
  (setq org-todo-keyword-faces
        '(("TODO" :foreground "goldenrod2" :weight bold)
          ("NEXT" :foreground "dodger blue" :weight bold)
          ("DONE" :foreground "medium sea green" :weight bold)
          ("WAITING" :foreground "coral" :weight bold)
          ("HOLD" :foreground "medium orchid" :weight bold)
          ("SOMEDAY" :foreground "medium turquoise" :weight bold)
          ("CANCELLED" :foreground "indian red" :weight bold)
          ("MEETING" :foreground "papaya whip" :weight bold)))

  ;; setup state triggers
  (setq org-todo-state-tags-triggers
        '(("CANCELLED" ("CANCELLED" . t))
          ("WAITING" ("SOMEDAY") ("HOLD") ("WAITING" . t))
          ("HOLD" ("WAITING") ("SOMEDAY") ("HOLD" . t))
          ("SOMEDAY" ("WAITING") ("HOLD") ("SOMEDAY" . t))
          (done ("WAITING") ("HOLD") ("SOMEDAY"))
          ("TODO" ("WAITING") ("CANCELLED") ("HOLD") ("SOMEDAY"))
          ("NEXT" ("WAITING") ("CANCELLED") ("HOLD") ("SOMEDAY"))
          ("DONE" ("WAITING") ("CANCELLED") ("HOLD") ("SOMEDAY"))))

  ;; use drawer for state changes
  (setq org-log-into-drawer 1)
  (setq org-clock-into-drawer "CLOCK")

  ;; Latex
  (setq org-format-latex-options
        '( :foreground default
           :background default
           :scale 2.75
           :html-foreground "Black"
           :html-background "Transparent"
           :html-scale 1.0
           :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

  :config
  (advice-add 'org-agenda :before #'organum-agenda-files-update)
  
  ;; Use RET to open org-mode links, including those in quick-help.org
  (setq org-return-follows-links t))

(elpa-use-package org-clock  
  :defer t
  :commands (org-clock-save)
  :init
  (setq
   org-agenda-clockreport-parameter-plist
   '(:link t :maxlevel 6 :fileskip 0 t :compact t :score 0)
   org-clock-idle-time 15
   org-time-stamp-rounding-minutes (quote (0 5))
   org-clock-out-when-done t
   org-clock-out-remove-zero-time-clocks t
   org-clock-persist-file (expand-file-name "org-clock-save.el" org-directory)
   org-clock-persist t)

  (org-clock-persistence-insinuate))

(elpa-use-package org-refile
  :defer t
  :init
  (setq
   org-outline-path-complete-in-steps nil
   org-refile-targets '((nil :maxlevel . 4)
                        (org-agenda-files :maxlevel . 4))
   org-refile-use-outline-path 'file
   org-refile-allow-creating-parent-nodes 'confirm
   org-refile-target-verify-function #'organum-refile-verify-target)
  :config
  (advice-add 'org-refile :after
        (lambda (&rest _)
          (organum-refile-save-buffers))))

(elpa-use-package org-agenda
  :defer t
  :general
  (leader-def
    "oa" '(org-agenda :which-key "agenda dispatch")
    "op" '(organum-agenda-person :which-key "person"))
  :config
  (advice-add 'org-agenda :before #'organum-agenda-files-update)

  (setq
   org-agenda-tags-column 'auto
   org-agenda-sticky t
   org-agenda-inhibit-startup nil
   org-agenda-dim-blocked-tasks nil

   org-agenda-compact-blocks nil

   org-agenda-time-grid
   (quote
    ((daily today remove-match)
     (800 1000 1200 1400 1600 1800 2000)
     "......" "----------------"))

   org-agenda-tags-todo-honor-ignore-options t
   org-deadline-warning-days 14

   org-element-use-cache nil

   org-agenda-prefix-format
   '((agenda . " %i %(organum-agenda-category 12)%?-12t% s")
	  (todo . " %i %(organum-agenda-category 12) ")
	  (tags . " %i %(organum-agenda-category 12) ")
	  (search . " %i %(organum-agenda-category 12) "))

   org-agenda-window-setup 'current-window
   
   org-agenda-custom-commands
   `(("h" "Habits" agenda "STYLE=\"habit\""
      ((org-agenda-overriding-header "Habits")
       (org-agenda-sorting-strategy
	'(todo-state-down effort-up category-keep))))
      ("p" "Primary View"
       (,organum-agenda-cmd-today-schedule
	,organum-agenda-cmd-refile
	,organum-agenda-cmd-projects
	,organum-agenda-cmd-project-next-tasks
	,organum-agenda-cmd-stuck-projects
	,organum-agenda-cmd-standalone-tasks
	,organum-agenda-cmd-waiting-postponed-tasks
	,organum-agenda-cmd-someday-projects-and-tasks)))))

(elpa-use-package org-roam
  :defer t
  :commands (org-roam-db-autosync-enable
             org-roam-db-sync)
  :general
  (leader-def
    "n" '(nil :which-key "org-roam...")
    "nf" '(org-roam-node-find :which-key "find")
    "ni" '(org-roam-node-insert :which-key "insert")
    "ndt" '(organum-journal-goto-today :which-key "today")
    "ndn" '(org-roam-dailies-goto-next :which-key "next")
    "ndp" '(org-roam-dailies-goto-previous :which-key "previous")
    "nf" '(org-roam-node-find :which-key "find")
    "ni" '(org-roam-node-insert :which-key "insert"))
  :init
  (setq
   org-roam-v2-ack t
   org-roam-directory org-directory
   org-roam-dailies-directory (expand-file-name "writings/journals" organum-directory)
   org-roam-db-location (expand-file-name "org-roam.db" organum-directory)
   org-roam-completion-everywhere t)
   
  (setq org-roam-node-display-template (concat "${title:*}" (propertize "${tags:20}" 'face 'org-tag)))
  :config
  (ignore-errors (org-roam-db-autosync-enable)))

(elpa-use-package consult-org-roam
  :ensure t
  :after org-roam
  :config
  (setq
   consult-org-roam-grep-func #'consult-ripgrep
   consult-org-roam-buffer-narrow-key ?r
   consult-org-roam-buffer-after-buffers t)

  (consult-customize
   consult-org-roam-forward-links
   :preview-key (kbd "C-SPC"))

  (consult-org-roam-mode 1))

(elpa-use-package org-capture
  :defer t
  :general
  (leader-def
    "c" '(nil :which-key "capture...")
    "cX" '(org-capture :which-key "dispatch")
    "ci" '(organum-capture-inbox :which-key "inbox")
    "cl" '(org-store-link :which-key "link")
    "cm" '(organum-capture-meeting :which-key "meeting")
    "cs" '(organum-capture-slipbox :which-key "slipbox"))
  :init
  (setq-default org-capture-bookmark nil)
  :config
  (organum-capture-setup))

(elpa-use-package oc
  :general
  (leader-def
    "r" '(org-cite-insert :which-key "insert"))
  :init
  (setq org-cite-global-bibliography `(,organum-bibliography-file))

  (setq org-cite-export-processors
        '((md . (csl "chicago-full-note-bibliography.csl"))
          (latex . biblatex)
          (odt . (csl "chicago-fullnote-bibliography.csl"))
          (t . (csl "modern-language-association.csl")))))

(elpa-use-package citar
  :init
  (setq
   org-cite-insert-processor 'citar
   org-cite-follow-processor 'citar
   org-cite-activate-processor 'citar
   citar-bibliography org-cite-global-bibliography
   citar-notes-paths `(,organum-references-directory))

  (setq
   citar-templates
   '((main . "${author editor:18}     ${date year issued:4}     ${title:30}")
     (suffix . "          ${=key= id:14}    ${=type=:8}    ${tags keywords:*}")
     (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
     (note . "Notes on ${author editor}, ${title}"))))

(elpa-use-package citar-org-roam
  :after citar org-roam
  :no-require
  :config (citar-org-roam-mode))

(elpa-use-package anki-editor
  :after org-capture
  :bind (:map org-mode-map
               ("C-c C-=" . anki-editor-cloze-region-auto-incr)
               ("C-c C--" . anki-editor-cloze-region-dont-incr)
               ("C-c C-0" . anki-editor-reset-cloze-number)
               ("C-c C-p" . anki-editor-push-tree))
  :hook (org-capture-after-finalize . anki-editor-reset-cloze-number) ; Reset cloze-number after each capture.
  :config
  (setq anki-editor-create-decks 't
        anki-editor-org-tags-as-anki-tags t)

  ;; Initialize
  (anki-editor-reset-cloze-number))

(elpa-use-package olivetti
  :bind
  ("C-c O" . olivetti-mode)
  :config
  (setq-default
   olivetti-body-width 0.80
   olivetti-minimum-body-width 72
   olivetti-recall-visual-line-mode-entry-state t))

(provide 'init-organum)
;;; init-org.el ends here
