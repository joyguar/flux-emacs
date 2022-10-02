;;; lib-organum-roam.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Daniel Lucas
;;
;; Author: Daniel Lucas <dclucas@protonmail.com>
;; Maintainer: Daniel Lucas <dclucas@protonmail.com>
;; Created: June 23, 2022
;; Modified: June 23, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/danielclucas/lib-organum-roam
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'org-roam)

;;;###autoload
(cl-defun organum-roam-node--filter-by-tags (node &optional included-tags excluded-tags)
  "Filter org-roam-node by tags."
  (let* ((tags (org-roam-node-tags node))
         (file-path (org-roam-node-file node))
         (rel-file-path (f-relative file-path org-roam-directory))
         (parent-directories (butlast (f-split rel-file-path)))
         (tags (cl-union tags parent-directories)))
    (if (or
         ;; (and included-tags (cl-notevery (lambda (x) (cl-member x tags :test #'string=)) included-tags))
         (and included-tags (not (cl-intersection included-tags tags :test #'string=)))
         (and excluded-tags (cl-intersection excluded-tags tags :test #'string=))
         ) nil t)))

;;;###autoload
(cl-defun organum-roam-node-find (included-tags excluded-tags)
  "Modded org-roam-node-find which filters nodes using tags."
  (interactive)
  (org-roam-node-find nil nil
                      (lambda (node) (organum-roam-node--filter-by-tags node included-tags excluded-tags))))

;;;###autoload
(cl-defun organum-roam-node-insert (included-tags excluded-tags)
  "Modded org-roam-node-insert which filters nodes using tags."
  (interactive)
  (org-roam-node-insert
   (lambda (node) (organum-roam-node--filter-by-tags node included-tags excluded-tags))))

(provide 'lib-organum-roam)
;;; lib-organum-roam.el ends here
