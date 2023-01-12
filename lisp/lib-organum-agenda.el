;;; lib-organum-agenda.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Daniel Lucas
;;
;; Author: Daniel Lucas <dclucas@protonmail.com>
;; Maintainer: Daniel Lucas <dclucas@protonmail.com>
;; Created: June 23, 2022
;; Modified: June 23, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/danielclucas/lib-org-agenda
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'init-elpa)
(require 'config-organum)
(require 'lib-organum)

(require 'org)
(require 'org-habit)

(defvar organum-agenda-hide-scheduled-and-waiting-next-tasks t
  "Non-nil means to hide scheduled and waiting tasks.

Affects the following commands:

- `organum-agenda-cmd-project-next-tasks'
- `organum-agenda-cmd-project-subtasks'
- `organum-agenda-cmd-standalone-tasks'
- `organum-agenda-cmd-waiting-postponed-tasks'")

;;;###autoload
(defun organum-agenda-main ()
  "Show main `org-agenda' view."
  (interactive)
  (org-agenda nil "p"))

;;;###autoload
(defun organum-agenda-person ()
  (interactive)
  (let* ((person (org-roam-node-read
               nil
               (lambda (node)
                 (member "people" (org-roam-node-tags node)))
               (lambda (completion-a completion-b)
                 (< (length (org-roam-node-title (cdr completion-a)))
                    (length (org-roam-node-title (cdr completion-b)))))
               t))
         (names (cons (org-roam-node-title person)
                      (org-roam-node-aliases person)))
         (tags (seq-map #'organum--title-to-tag names))
         (query (string-join tags "|")))
    (dlet ((org-agenda-overriding-arguments (list t query)))
      (org-agenda nil "M"))))

;;;###autoload
(defun organum-project-files ()
    "Return a list of note files containing \\='agenda' tag." ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
        :from tags
        :left-join nodes
        :on (= tags:node-id nodes:id)
        :where (like tag (quote "%\"agenda\"%"))]))))

;;;###autoload
(defun organum-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (seq-uniq (append (list organum-inbox-file)
                                           (organum-project-files)))))

;;;; Agenda commands

;;;###autoload
(defconst organum-agenda-cmd-today-schedule
  '(agenda "" ((org-agenda-overriding-header "Today's Schedule:")
	       (org-agenda-span 'day)
	       (org-agenda-ndays 1)
	       (org-agenda-start-on-weekday nil)
	       (org-agenda-start-day "+0d")))
  "A block showing a 1 day schedule.")

;;;###autoload
(defconst organum-agenda-cmd-weekly-log
  '(agenda "" ((org-agenda-overriding-header "Weekly Log")))
  "A block showing my schedule and logged tasks for this week.")

;;;###autoload
(defconst organum-agenda-cmd-previous-calendar-data
  '(agenda "" ((org-agenda-overriding-header "Previous Calendar Data (last 3 weeks)")
	       (org-agenda-start-day "-21d")
	       (org-agenda-span 21)
	       (org-agenda-start-on-weekday nil)))
  "A block showing my schedule and logged tasks for the last few weeks.")

;;;###autoload
(defconst organum-agenda-cmd-upcoming-calendar-data
  '(agenda "" ((org-agenda-overriding-header "Upcoming Calendar Data (next 2 weeks)")
	       (org-agenda-start-day "0d")
	       (org-agenda-span 14)
	       (org-agenda-start-on-weekday nil)))
  "A block showing my schedule for the next couple weeks.")

;;;###autoload
(defconst organum-agenda-cmd-refile
  '(tags "REFILE"
	 ((org-agenda-overriding-header "To refile")
	  (org-tags-match-list-sublevels nil))))

;;;###autoload
(defconst organum-agenda-cmd-stuck-projects
  '(tags-todo "-CANCELLED-SOMEDAY/!"
              ((org-agenda-overriding-header "Stuck Projects")
               (org-agenda-skip-function 'organum-agenda-skip-non-stuck-projects)
               (org-agenda-sorting-strategy
                '(category-keep)))))

;;;###autoload
(defconst organum-agenda-cmd-projects
  '(tags-todo "-HOLD-CANCELLED-SOMEDAY/!"
              ((org-agenda-overriding-header "Projects")
               (org-agenda-skip-function 'organum-agenda-skip-non-projects)
               (org-tags-match-list-sublevels 'indended)
               (org-agenda-sorting-strategy
                '(category-keep)))))

;;;###autoload
(defconst organum-agenda-cmd-project-next-tasks
  '(tags-todo "-CANCELLED-SOMEDAY/!NEXT"
              ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                     (if organum-agenda-hide-scheduled-and-waiting-next-tasks
                                                         ""
                                                       " (including WAITING and SCHEDULED tasks)")))
               (org-agenda-skip-function 'organum-agenda-skip-projects-and-habits-and-single-tasks)
               (org-tags-match-list-sublevels t)
               (org-agenda-todo-ignore-with-date organum-agenda-hide-scheduled-and-waiting-next-tasks)
               (org-agenda-sorting-strategy
                '(category-keep)))))

;;;###autoload
(defconst organum-agenda-cmd-project-subtasks
   '(tags-todo "-REFILE-CANCELLED-WAITING-HOLD-SOMEDAY/!"
              ((org-agenda-overriding-header (concat "Project Subtasks"
                                                     (if organum-agenda-hide-scheduled-and-waiting-next-tasks
                                                         ""
                                                       " (including WAITING and SCHEDULED tasks)")))
               (org-agenda-skip-function 'organum-agenda-skip-non-project-tasks)
               (org-agenda-todo-ignore-with-date organum-agenda-hide-scheduled-and-waiting-next-tasks)
               (org-agenda-sorting-strategy
                '(category-keep)))))

;;;###autoload
(defconst organum-agenda-cmd-standalone-tasks
  '(tags-todo "-REFILE-CANCELLED-WAITING-HOLD-SOMEDAY/!"
             ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                    (if organum-agenda-hide-scheduled-and-waiting-next-tasks
                                                        ""
                                                      " (including WAITING and SCHEDULED tasks)")))
              (org-agenda-skip-function 'organum-agenda-skip-project-tasks)
              (org-agenda-todo-ignore-with-date organum-agenda-hide-scheduled-and-waiting-next-tasks)
              (org-agenda-sorting-strategy
               '(category-keep)))))

;;;###autoload
(defconst organum-agenda-cmd-waiting-postponed-tasks
  '(tags-todo "-CANCELLED+WAITING|HOLD/!"
             ((org-agenda-overriding-header (concat "Waiting Tasks"
                                                    (if organum-agenda-hide-scheduled-and-waiting-next-tasks
                                                        ""
                                                      " (include WAITING and SCHEDULED tasks)")))
              (org-agenda-skip-function 'organum-agenda-skip-non-tasks)
              (org-tags-match-list-sublevels nil)
              (org-agenda-todo-ignore-scheduled organum-agenda-hide-scheduled-and-waiting-next-tasks)
              (org-agenda-todo-ignore-deadlines organum-agenda-hide-scheduled-and-waiting-next-tasks))))

;;;###autoload
(defconst organum-agenda-cmd-someday-projects-and-tasks
  '(tags-todo "SOMEDAY"
              ((org-agenda-overriding-header "Someday Projects and Tasks")
               (org-tags-match-list-sublevels nil))))


;; Utilities to build agenda comands -- skip

;;;###autoload

(defun organum-agenda-skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (organum-agenda-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

;;;###autoload
(defun organum-agenda-skip-non-projects ()
  "Skip trees that are not projects"
  (if (save-excursion (organum-agenda-skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((organum-agenda-project-p)
            nil)
           ((and (organum-agenda-project-subtree-p) (not (organum-agenda-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

;;;###autoload
(defun organum-agenda-skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((organum-agenda-task-p)
        nil)
       (t
        next-headline)))))

;;;###autoload
(defun organum-agenda-skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((organum-agenda-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

;;;###autoload
(defun organum-agenda-skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and organum-agenda-hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags)))
        next-headline)
       ((organum-agenda-project-p)
        next-headline)
       ((and (organum-agenda-task-p) (not (organum-agenda-project-subtree-p)))
        next-headline)
       (t
        nil)))))

;;;###autoload
(defun organum-agenda-skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((organum-agenda-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((organum-agenda-project-subtree-p)
        subtree-end)
       (t
        nil)))))

;;;###autoload
(defun organum-agenda-skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((organum-agenda-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (organum-agenda-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (organum-agenda-project-subtree-p))
        subtree-end)
       (t
        nil)))))

;;;###autoload
(defun organum-agenda-skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (organum-agenda-subproject-p)
        nil
      next-headline)))


;; Utilities to build agenda commands -- predicates

;;;###autoload
(defun organum-agenda-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

;;;###autoload
(defun organum-agenda-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (organum-agenda-find-project-task)
      (if (equal (point) task)
          nil
        t))))

;;;###autoload
(defun organum-agenda-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

;;;###autoload
(defun organum-agenda-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))


;; Utilities to build agenda commands -- search

;;;###autoload
(defun organum-agenda-find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))


;; Utilities to navigate the agenda

;; Search for a "=" and go to the next line
;;;###autoload
(defun organum-agenda-next-section ()
  "Go to the next section in an org agenda buffer."
  (interactive)
  (if (search-forward "===" nil t 1)
      (forward-line 1)
    (goto-char (point-max)))
  (beginning-of-line))

;; Search for a "=" and go to the previous line
;;;###autoload
(defun organum-agenda-prev-section ()
  "Go to the next section in an org agenda buffer."
  (interactive)
  (forward-line -2)
  (if (search-forward "===" nil t -1)
      (forward-line 1)
    (goto-char (point-min))))


;; Utilities to process agenda entries

;;;###autoload
(defun organum-agenda-process-inbox ()
  "Called in org-agenda-mode, processes all inbox items."
  (interactive)
  (org-agenda-bulk-mark-regexp "REFILE")
  (organum-agenda-bulk-process-entries))

(defvar organum-agenda-current-effort "1:00"
  "Current effort for agenda items.")

(defun organum-agenda-set-effort (effort)
  "Set the effort property for the current headline."
  (interactive
   (list (read-string (format "Effort [%s]: " organum-agenda-current-effort) nil nil organum-agenda-current-effort)))
  (setq organum-agenda-current-effort effort)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-fold-show-context 'agenda)
        (funcall-interactively 'org-set-effort nil organum-agenda-current-effort)
        (end-of-line 1)
        (setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker))))

(defun organum-agenda-process-inbox-item ()
  "Process a single item in the org-agenda."
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority)
   (call-interactively 'org-agenda-set-effort)
   (org-agenda-refile nil nil t)))

(defun organum-agenda-bulk-process-entries ()
  (if (not (null org-agenda-bulk-marked-entries))
      (let ((entries (reverse org-agenda-bulk-marked-entries))
            (processed 0)
            (skipped 0))
        (dolist (e entries)
          (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
            (if (not pos)
                (progn (message "Skipping removed entry at %s" e)
                       (cl-incf skipped))
              (goto-char pos)
              (let (org-loop-over-headlines-in-active-region) (funcall 'organum-agenda-process-inbox-item))
              ;; `post-command-hook' is not run yet.  We make sure any
              ;; pending log note is processed.
              (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                        (memq 'org-add-log-note post-command-hook))
                (org-add-log-note))
              (cl-incf processed))))
        (org-agenda-redo)
        (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
        (message "Acted on %d entries%s%s"
                 processed
                 (if (= skipped 0)
                     ""
                   (format ", skipped %d (disappeared before their turn)"
                           skipped))
                 (if (not org-agenda-persistent-marks) "" " (kept marked)")))))


;;;###autoload
(defun organum-agenda-category (&optional len)
  "Get category of item at point for agenda.

Category is defined by one of the following items:

- CATEGORY property
- TITLE keyword
- TITLE property
- filename without directory and extension

When LEN is a number, resulting string is padded right with
spaces and then truncated with ... on the right if result is
longer than LEN.

Usage example:

  (setq org-agenda-prefix-format
        \\='((agenda . \" %(org-agenda-category) %?-12t %12s\")))

Refer to `org-agenda-prefix-format' for more information."
  (let* ((file-name (when buffer-file-name
                      (file-name-sans-extension
                       (file-name-nondirectory buffer-file-name))))
         (title (organum-buffer-prop-get "title"))
         (category (org-get-category))
         (result
          (or (if (and
                   title
                   (string-equal category file-name))
                  title
                category)
              "")))
    (if (numberp len)
        (s-truncate len (s-pad-right len " " result))
      result)))


(provide 'lib-organum-agenda)
;;; lib-org-agenda.el ends here
