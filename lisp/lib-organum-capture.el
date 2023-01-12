;;; lib-organum-capture.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Daniel Lucas
;;
;; Author: Daniel Lucas <dclucas@protonmail.com>
;; Maintainer: Daniel Lucas <dclucas@protonmail.com>
;; Created: June 23, 2022
;; Modified: June 23, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/danielclucas/lib-organum-capture
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'config-organum)

(require 'org)
(require 'org-capture)
(require 'org-roam)
(require 'org-roam-dailies)
(require 'citar)
(require 'citar-format)


;;;###autoload
(defun organum-capture-setup ()
  (setq
   org-capture-templates
   `(("i" "Inbox" entry
      (file organum-inbox-file)
      ,(concat "* TODO %?\n"
               "CREATED: %U")
      :clock-in t
      :clock-resume t
      :kill-buffer t
      :unnarrowed t)
     ("s" "Slipbox" entry
      (file organum-slipbox-file)
      "* %?\n")
     ("m" "Meeting" entry
      (function organum-capture-meeting-target)
      (function organum-capture-meeting-template)
      :clock-in t
      :clock-resume t)
     ("f" "Basic Flashcard" entry
      (file organum-flashcards-file)
      (function organum-capture-flashcard)
      :unnarrowed t))
   org-roam-capture-templates
   `(("d" "default" plain "%?"
      :target (file+head "%(organum-subdir-select)/%<%Y%m%dT%H%M%S>.org"
                         ,(string-join '("#+title: ${title}\n"
                                         "#+filetags:\n"
                                         "#+timestamp: %<%Y%m%dT%H%M%S>\n")))
      :unnarrowed t))
   org-roam-dailies-capture-templates
   '(("d" "Dream" plain ""
      :target (file+head+olp "%<%Y-%m>.org"
                             "#+title: %<%B %Y>\n#+filetags:\n"
                             ("%<%Y-%m-%d %A>" "dreams" "%<%H:%M>\n"))
      :empty-lines-after 1)
     ("l" "Log" plain ""
      :target (file+head+olp "%<%Y-%m>.org"
                             "#+title: %<%B %Y>\n#+filetags:\n"
                             ("%<%Y-%m-%d %A>" "logs" "%<%H:%M>\n"))
      :empty-lines-after 1)
     ("r" "Reflection" plain ""
      :target (file+head+olp "%<%Y-%m>.org"
                             "#+title: %<%B %Y>\n#+filetags:\n"
                             ("%<%Y-%m-%d %A>" "reflections" "%<%H:%M>\n"))
      :empty-lines-after 1))))

;;;###autoload
(defun organum-capture-inbox ()
  (interactive)
  (org-capture nil "i"))

;;;###autoload
(defun organum-capture-slipbox ()
  (interactive)
  (org-capture nil "s"))

;;;###autoload
(defun organum-capture-meeting ()
  "Capture a meeting."
  (interactive)
  (org-capture nil "m"))

;;;###autoload
(defun organum-journal-goto-today ()
  "Goto journal for today."
  (interactive)
  (org-roam-dailies-capture-today t "d"))

(defun organum-capture-meeting-target ()
  "Return a target for a meeting capture."
  (let ((person (org-capture-get :meeting-person)))
    (if (org-roam-node-file person)
        (let ((path (org-roam-node-file person))
              (headline "Meetings"))
          (set-buffer (org-capture-target-buffer path))
          ;; Org expects the target file to be in Org m ode, otherwise
          ;; it throws an error. However, the default notes files
          ;; should work out of the box. In this case, we switch it to
          ;; Org mode.
          (unless (derived-mode-p 'org-mode)
            (org-display-warning
             (format
              "Capture requirement: switching buffer %S to Org mode"
              (current-buffer)))
            (org-mode))
          (org-capture-put-target-region-and-position)
          (widen)
          (goto-char (point-min))   
          (if (re-search-forward
               (format org-complex-heading-regexp-format
                       (regexp-quote headline))
               nil t)
              (beginning-of-line)
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert "* " headline "\n")
            (beginning-of-line 0)))
      (let ((path organum-inbox-file))
        (set-buffer (org-capture-target-buffer path))
        (org-capture-put-target-region-and-position)
        (widen)))))

(defun organum-capture-meeting-template ()
  "Return a template for meeting capture."
  (let ((person (org-roam-node-read
                 nil
                 (lambda (node)
                   (member "people" (org-roam-node-tags node)))
                 (lambda (completion-a completion-b)
                   (< (length (org-roam-node-title (cdr completion-a)))
                      (length (org-roam-node-title (cdr completion-b))))))))
    (org-capture-put :meeting-person person)
    (if (org-roam-node-file person)
        "* MEETING [%<%Y-%m-%d %a>] :REFILE:MEETING:\n%U\n\n%?"
      (concat "* MEETING with "
              (org-roam-node-title person)
              " on [%<%Y-%m-%d %a>] :MEETING:\n%U\n\n%?"))))

;;;###autoload
(defun organum-roam-node-from-cite (citekey)
  (interactive (list (citar-select-ref)))
  (let ((title (citar-format--entry
                "${author editor} :: ${title}" (citar-get-entry citekey))))
    (org-roam-capture- :templates
                       '(("r" "reference" plain "%?" :if-new
                          (file+head "pkm/refnotes/${citekey}.org"
                                     ":PROPERTIES:
:ROAM_REFS: [cite:@${citekey}]
:END:
#+title: ${title}\n")
                          :immediate-finish t
                          :unnarrowed t))
                       :info (list :citekey citekey)
                       :node (org-roam-node-create :title title)
                       :props '(:finalize find-file))))

(defun organum-capture-flashcard ()
  "Return a template for flashcard capture."
  (let ((note-type (completing-read "Choose a note type: "
                                    '("Basic" "Cloze"))))
    (if (string-equal note-type "Basic")
        (string-join '("* %^{Heading}\n"
                       ":PROPERTIES:\n"
                       ":ANKI_NOTE_TYPE: Basic\n"
                       ":ANKI_DECK: Default\n"
                       ":END:\n"
                       "** Front\n"
                       "%?\n"
                       "** Back\n\n"))
      (string-join '("* %^{Heading}\n"
                     ":PROPERTIES:\n"
                     ":ANKI_NOTE_TYPE: Cloze\n"
                     ":ANKI_DECK: Default\n"
                     ":END:\n"
                     "** Text\n\n"
                     "** Extra\n")))))

(provide 'lib-organum-capture)
;;; lib-organum-capture.el ends here
