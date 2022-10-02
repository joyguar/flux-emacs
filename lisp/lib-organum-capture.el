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
      (file "pkm/slipbox.org")
      "* %?\n")
     ("m" "Meeting" entry
      (function organum-capture-meeting-target)
      (function organum-capture-meeting-template)
      :clock-in t
      :clock-resume t))
   org-roam-capture-templates
   `(("d" "default" plain "%?"
      :target (file+head "%(organum-subdir-select)/%<%Y%m%dT%H%M%S>.org"
                         ,(string-join '("#+title: ${title}\n"
                                         "#+filetags:\n"
                                         "#+timestamp: %<%Y%m%dT%H%M%S>\n")))
      :unnarrowed t))
   org-roam-dailies-capture-templates
   '(("d" "Dream" entry ""
      :target (file+head+olp "%<%Y-%m>.org"
                             "#+title: %<%A, %d %B %Y>\n#+filetags: journal\n"
                             ("%<%Y-%m-%d %A>" "dreams" "%<%H:%M>\n"))
      :empty-lines-after 1)
     ("l" "Log" plain ""
      :target (file+head+olp "%<%Y-%m>.org"
                             "#+title: %<%A, %d %B %Y>\n#+filetags: journal\n"
                             ("%<%Y-%m-%d %A>" "logs" "%<%H:%M>\n"))
      :empty-lines-after 1)
     ("r" "Reflection" plain ""
      :target (file+head+olp "%<%Y-%m>.org"
                             "#+title: %<%A, %d %B %Y>\n#+filetags: journal\n"
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

(defun organum-capture-journal-target-heading ()
  "Return a file target for a journal capture."
  (let ((date-heading (format-time-string "%Y-%m-%d %A"))
        (journal-heading (org-capture-get :journal-type)))
    (goto-char (point-min))
    (if (re-search-forward
         (format org-complex-heading-regexp-format
                 (regexp-quote date-heading))
         nil t)
        (beginning-of-line)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "* " date-heading "\n")
      (beginning-of-line 0))
    (if (re-search-forward
         (format org-complex-heading-regexp-format
                 (regexp-quote journal-heading))
         nil t)
        (beginning-of-line)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "* " journal-heading "\n")
      (beginning-of-line 0))))

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

(provide 'lib-organum-capture)
;;; lib-organum-capture.el ends here
