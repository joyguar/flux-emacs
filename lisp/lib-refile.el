;;; lib-refile.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Daniel Lucas
;;
;; Author: Daniel Lucas <dclucas@protonmail.com>
;; Maintainer: Daniel Lucas <dclucas@protonmail.com>
;; Created: June 26, 2022
;; Modified: June 26, 2022
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/danielclucas/lib-refile.el
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'org)

(defvar organum-projects-dir (expand-file-name "projects/" org-directory))

(defvar organum-archive-dir (expand-file-name "archive/" org-directory))


(defvar organum-refile-ignored-tags '("REFILE")
  "List of tags to ignore during refile.")

;;;###autoload
(defun organum-refile-verify-target ()
  "Exclude todo keywords with a done state from refile targets."
  (let ((tags-at (org-get-tags)))
    (and
     ;; doesn't have done keyword
     (not (member (nth 2 (org-heading-components)) org-done-keywords))

     ;; doesn't have blacklisted tag
     (or (null tags-at)
         (cl-member-if-not
          (lambda (x)
            (member (if (listp x) (car x) x)
                    organum-refile-ignored-tags))
          tags-at)))))

;; Copied from this article:
;; <http://www.howardism.org/Technical/Emacs/getting-even-more-boxes-done.
;; html#fn.1>.
(defun organum-refile-subtree-as-file (dir)
  "Archive the org-mode subtree and create an entry in the directory folder
specified by DIR. The formatting, since it is an archive, isn't quite what I
want, but it gets it going."
    (let* ((header (substring-no-properties (org-get-heading)))
           (title (if (string-match ": \\(.*\\)" header)
                      (match-string 1 header)
                    header))
           (filename (replace-regexp-in-string "\s+" "-" (downcase title)))
           (filepath (format "%s/%s.org" dir filename))
           (org-archive-location (format "%s::" filepath)))
      (org-archive-subtree)
      (find-file-other-window filepath)))

;;;###autoload
(defun organum-refile-to-projects-dir ()
  "Move the current subtree to a file in the `projects' directory."
  (interactive)
  (organum-refile-subtree-as-file organum-projects-dir))

;;;###autoload
(defun organum-refile-save-buffers ()
  "Save `org-agenda-files' buffers without user confirmation.
See also `org-save-all-org-buffers'"
  (interactive)
  (message "Saving org-agenda-files buffers...")
  (save-some-buffers t (lambda () 
             (when (member (buffer-file-name) org-agenda-files) 
               t)))
  (message "Saving org-agenda-files buffers... done"))

(provide 'lib-refile)
;;; lib-refile.el ends here
