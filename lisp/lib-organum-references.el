;;; lib-organum-references.el -*- lexical-binding: t; -*-
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

(defun acm-pdf-url (doi)
  "Retrieve a DOI pdf from the ACM."
  (concat "https://dl.acm.org/doi/pdf/" doi))

(defun ieee-pdf-url (doi)
  "Retrieve a DOI pdf from the IEEE."
  (when (string-match "\\.\\([0-9]*\\)$" doi)
    (let ((doi-bit (match-string 1 doi)))
      (concat "https://ieeexplore.ieee.org/stampPDF/getPDF.jsp?tp=&arnumber=" doi-bit "&ref="))))

(defun springer-pdf-url (doi)
  "Retrieve a DOI pdf from the Springer."
  (concat "https://link.springer.com/content/pdf/" doi ".pdf"))

(defun arxiv-pdf-url (eprint)
  "Download an arXiv pdf based on it's EPRINT number."
  (concat "https://arxiv.org/pdf/" eprint ".pdf"))

(defun scihub-pdf-url (doi)
  "Retrieve a doi pdf from Sci-hub."
  (concat "https://sci-hub.se/https://doi.org/" doi))

(defun download-pdf-from-link (link key)
  (url-copy-file link
                 (concat (car ebib-file-search-dirs) "/" key ".pdf")))

(defun ebib-download-pdf-from-doi ()
  "Download a PDF for the current entry."
  (interactive)
  (let* ((key (ebib--get-key-at-point))
         (doi (ebib-get-field-value "doi" key ebib--cur-db 'noerror 'unbraced 'xref)))
    (unless key (error "[Ebib] No key assigned to entry"))
    (download-pdf-from-doi key doi)))

(defun download-pdf-from-doi (key &optional doi publisher eprint journal organization url)
  "Download pdf from DOI with KEY name."
  (let ((pub  (or publisher ""))
	(epr  (or eprint ""))
	(jour (or journal ""))
	(org  (or organization ""))
	(link (or url "")))
    (url-copy-file (cond
                    ((not doi) link)
		            ((or (string-match "ACM" (s-upcase pub))
			             (string-match "association for computing machinery" (s-downcase pub)))
		             (acm-pdf-url doi))
		            ((string-match "arxiv" (s-downcase pub))
		             (arxiv-pdf-url epr))
		            ((or (string-match "IEEE" (s-upcase pub))
			             (string-match "IEEE" (s-upcase jour))
			             (string-match "IEEE" (s-upcase org)))
		             (ieee-pdf-url doi))
		            ((string-match "springer" (s-downcase pub))
		             (springer-pdf-url doi))
		            (t
                     (scihub-pdf-url doi)))
		           (concat (car ebib-file-search-dirs) "/" key ".pdf"))))

(defun ebib-check-file ()
  "Check if current entry has a file associated with it."
  (interactive)
  (let ((key (ebib--get-key-at-point)))
    (unless (file-exists-p (concat (car ebib-file-search-dirs) "/" key ".pdf"))
      (error "[Ebib] No PDF found."))))
