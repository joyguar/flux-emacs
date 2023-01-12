;;; lib-organum.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Daniel Lucas
;;
;; Author: Daniel Lucas <dclucas@protonmail.com>
;; Maintainer: Daniel Lucas <dclucas@protonmail.com>
;; Created: June 23, 2022
;; Modified: June 23, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/danielclucas/lib-dl-org
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
(require 'lib-directory)
(require 'config-organum)

(require 'org)
(require 'org-roam)
(require 'org-roam-db)
(require 'org-roam-dailies)

(defun organum-buffer-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (eq major-mode 'org-mode)
       (string-suffix-p "org" buffer-file-name)
       (string-prefix-p
        (expand-file-name (file-name-as-directory org-roam-directory))
        (file-name-directory buffer-file-name))))

(defun organum-agenda-p ()
  "Return non-nil if current buffer has any todo entry.
TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks. The only exception is headings tagged as REFILE."
  (org-element-map
      (org-element-parse-buffer 'headline)
      'headline
    (lambda (h)
      (let ((todo-type (org-element-property :todo-type h)))
        (or
         ;; any headline with some todo keyword
         (eq 'todo todo-type)
         ;; any headline with REFILE tag
         (let ((hl-tags (org-get-tags (org-element-property :contents-begin h)))
               (agenda-tags (list "REFILE" "SOMEDAY")))
           (seq-some (lambda (tag) (seq-contains-p hl-tags tag)) agenda-tags))
         ;; any non-todo headline with an active timestamp
         (and
          (not (eq 'done todo-type))
          (org-element-property :contents-begin h)
	        (save-excursion
	          (goto-char (org-element-property :contents-begin h))
	          (let ((end (save-excursion
			                   ;; we must look for active timestamps only 
			                   ;; before then next heading, even if it's 
			                   ;; child, but org-element-property 
			                   ;; :contents-end includes all children 
			                   (or 
			                    (re-search-forward org-element-headline-re 
					                                   (org-element-property :contents-end h) 
					                                   ':noerror) 
			                    (org-element-property :contents-end h))))) 
	            (re-search-forward org-ts-regexp end 'noerror)))))))
    nil 'first-match))

(defun organum-insert-handle (id)
  "Hook to be called on NOTE after `org-roam-node-insert'."
  (when-let* ((node (org-roam-node-from-id id))
              (title (org-roam-node-title node))
              (tags (org-roam-node-tags node)))
    (when (seq-contains-p tags "people")
      (save-excursion
        (ignore-errors
          (org-back-to-heading)
          (when (eq 'todo (org-element-property
                           :todo-type
                           (org-element-at-point)))
            (org-set-tags
             (seq-uniq
              (cons
               (organum--title-to-tag title)
               (org-get-tags nil t))))))))))

(defun organum-tags-add ()
  "Add a tag to current note."
  (interactive)
  (org-with-point-at 1
  (when (call-interactively #'org-roam-tag-add)
    (organum-ensure-filetag))))

;;;###autoload
(defun organum-ensure-filetag ()
  "Add missing FILETAGS to the current note."
  (interactive)
  ;; add base-dir within organum directory as tag
  (let* ((file (buffer-file-name))
         (base-dir-tag
          (when file
            (seq-filter
             (lambda (x) (not (string-empty-p x)))
             (list (file-name-nondirectory
                    (directory-file-name
                     (string-remove-prefix
                      organum-directory
                      (file-name-directory file))))))))
          (original-tags (organum-buffer-tags-get))
          (tags (append original-tags base-dir-tag)))

    ;; process people
    (when (seq-contains-p tags "people")
      (let ((tag (organum--title-as-tag)))
        (unless (seq-contains-p tags tag)
          (setq tags (cons tag tags)))))

    ;; process projects
    (if (organum-agenda-p)
        (setq tags (cons "agenda" tags))
      (setq tags (remove "agenda" tags)))

    (setq tags (seq-uniq tags))

     ;; update tags if changed
    (when (or (seq-difference tags original-tags)
              (seq-difference original-tags tags))
      (apply #'organum-buffer-tags-set (seq-uniq tags)))))

(defun organum-buffer-timestamp-get ()
  "Return timestamp value in current buffer."
  (organum-buffer-prop-get "timestamp"))

(defun organum-buffer-title-get ()
  "Return title value in current buffer."
  (organum-buffer-prop-get "title"))

(defun organum-buffer-tags-get ()
  "Return filetags value in current buffer."
  (organum-buffer-prop-get-list "filetags" "[ :]"))

(defun organum-buffer-tags-set (&rest tags)
  "Set TAGS in current buffer.
If filetags value is already set, replace it."
  (if tags
      (organum-buffer-prop-set
       "filetags" (concat ":" (string-join tags ":") ":"))
    (organum-buffer-prop-remove "filetags")))

(defun organum-buffer-tags-add (tag)
  "Add a TAG to filetags in current buffer."
  (let* ((tags (organum-buffer-tags-get))
         (tags (append tags (list tag))))
    (apply #'organum-buffer-tags-set tags)))

(defun organum-buffer-tags-remove (tag)
  "Remove a TAG from filetags in current buffer."
  (let* ((tags (organum-buffer-tags-get))
         (tags (delete tag tags)))
    (apply #'organum-buffer-tags-set tags)))

(defun organum-buffer-prop-set (name value)
  "Set a file property called NAME to VALUE in buffer file.
If the property is already set, replace its value."
  (setq name (downcase name))
  (org-with-point-at 1
    (let ((case-fold-search t))
      (if (re-search-forward (concat "^#\\+" name ":\\(.*\\)")
                             (point-max) t)
          (replace-match (concat "#+" name ": " value) 'fixedcase)
        (while (and (not (eobp))
                    (looking-at "^[#:]"))
          (if (save-excursion (end-of-line) (eobp))
              (progn
                (end-of-line)
                (insert "\n"))
            (forward-line)
            (beginning-of-line)))
        (insert "#+" name ": " value "\n")))))

(defun organum-buffer-prop-set-list (name values &optional separators)
  "Set a file property called NAME to VALUES in current buffer.
VALUES are quoted and combined into single string using
`combine-and-quote-strings'.
If SEPARATORS is non-nil, it should be a regular expression
matching text that separates, but is not part of, the substrings.
If nil it defaults to `split-string-default-separators', normally
\"[ \f\t\n\r\v]+\", and OMIT-NULLS is forced to t.
If the property is already set, replace its value."
  (organum-buffer-prop-set
   name (combine-and-quote-strings values separators)))

(defun organum-buffer-prop-get (name)
  "Get a buffer property called NAME as a string."
  (org-with-point-at 1
    (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                             (point-max) t)
      (buffer-substring-no-properties
       (match-beginning 1)
       (match-end 1)))))

(defun organum-buffer-prop-get-list (name &optional separators)
  "Get a buffer property NAME as a list using SEPARATORS.
If SEPARATORS is non-nil, it should be a regular expression
matching text that separates, but is not part of, the substrings.
If nil it defaults to `split-string-default-separators', normally
\"[ \f\t\n\r\v]+\", and OMIT-NULLS is forced to t."
  (let ((value (organum-buffer-prop-get name)))
    (when (and value (not (string-empty-p value)))
      (split-string-and-unquote value separators))))

(defun organum-buffer-prop-remove (name)
  "Remove a buffer property called NAME."
  (org-with-point-at 1
    (when (re-search-forward (concat "\\(^#\\+" name ":.*\n?\\)")
                             (point-max) t)
      (replace-match ""))))

;;;###autoload
(defun organum-subdir-select ()
  "Select notes subdirectory."
  (interactive)
  (let ((dirs (cons
               "."
               (seq-map
                (lambda (p)
                  (string-remove-prefix organum-directory p))
                (directory-subdirs organum-directory 'recursive)))))
    (concat organum-directory (completing-read "Subdir: " dirs nil t))))

(defun organum--title-as-tag ()
  "Return title of the current note as tag."
  (organum--title-to-tag (organum-buffer-prop-get "title")))

(defun organum--title-to-tag (title)
  "Convert TITLE to tag."
  (concat "@" (s-replace " " "" title)))

;;;###autoload
(defun organum-pre-save-hook ()
  (when (and (not (active-minibuffer-window))
             (organum-buffer-p))
    (organum-ensure-filetag)))

;;'(org-todo-regexp)
;;             (string-match-p org-todo-regexp (org-get-heading)))

;;;###autoload
(defun organum-insert-heading-hook ()
  (let ((cmnd this-command)
        (hdngs '(org-insert-todo-heading-respect-content org-insert-todo-subheading)))
    (when (memq cmnd hdngs)
      (insert (format-time-string
                    (concat "\nCREATED: "
                            (concat "[" (substring (cdr org-time-stamp-formats) 1 -1) "]"))))
    (org-back-to-heading)
    (move-end-of-line()))))

(provide 'lib-organum)
;;; lib-organum.el ends here
