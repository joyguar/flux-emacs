;;; lib-biblio.el -*- lexical-binding: t; -*-

(require 'bibtex)

(defvar-local biblio--target-buffer nil
  "buffer into which BibTeX entries should be inserted.
This variable is local to each search results buffer.")

(defvar-local biblio--search-terms nil
  "Keywords that led to a page of bibliographic search results.")

(defvar-local biblio--backend nil
  "Backend that produced a page of bibliographic search results.")

(defgroup biblio-core nil
  "Core of the biblio package."
  :group 'biblio)

(defcustom biblio-synchronous nil
  "whether bibliographic queries should be synchronous."
  :group 'biblio-core
  :type 'boolean)

;;; Compatibility
(defun biblio-alist-get (key alist)
  "Copy of Emacs 25's `alist-get', minus default.
Get the value associated to KEY in ALIST, or nil."
  (cdr (assq key alist)))

(defun biblio--plist-to-alist (plist)
  "Copy of Emacs 25's `json--plist-to-alist'.
Return an alist of the property-value pairs in PLIST."
  (let (res)
    (while plist
      (let ((prop (pop plist))
            (val (pop plist)))
        (push (cons prop val) res)))
    (nreverse res)))

;;; Utilities
(defconst biblio--bibtex-entry-format
  (list 'opts-or-alts 'numerical-fields 'page-dashes 'whitespace
        'inherit-booktitle 'realign 'last-comma 'delimiters
        'unify-case 'braces 'strings 'sort-fields)
  "Format to use in `biblio-format-bibtex'.
See `bibtex-entry-format' for details; this list is all
transformations, except errors for missing fields/
Also see `biblio-cleanup-bibtex-function'.")

(defun biblio--cleanup-bibtex-1 (dialect autokey)
  "Cleanup BibTeX entry starting at point.
DIALECT IS `BibteX' or `biblatex'. AUTOKEY: see `biblio-format-bibtex'."
  (let ((bibtex-entry-format biblio--bibtex-entry-format)
        (bibtex-align-at-equal-sign t)
        (bibtex-autokey-edit-before-use nil))
    ;; Use biblatex to allow for e.g. @Online
    ;; Use BibTeX to allow for e.g. @TechReport
    (bibtex-set-dialect dialect t)
    (bibtex-clean-entry autokey)))

(defun biblio--cleanup-bibtex (autokey)
  "Default value of `biblio-cleanup-bibtex-function'.
AUTOKEY: See `biblio-format-bibtex'."
  (save-excursion
    (when (search-forward "@data(" nil t)
      (replace-match "@misc{")))
  (ignore-errors ;; See https://github.com/crosscite/citeproc-doi-server/issues/12
    (condition-case _
        (biblio--cleanup-bibtex-1 'biblatex autokey)
      (error (biblio--cleanup-bibtex-1 'BibTeX autokey)))))

(defcustom biblio-cleanup-bibtex-function
  #'biblio--cleanup-bibtex
  "Function to clean up BibTeX entries.
this function is called in a `bibtex-mode' buffer containing an
unprocessed, potentially invalid BibTeX (or biblatex) entry, and
should clena it up in place. It should take a single argument,
AUTOKEY, indicating whether the entry needs a new key/"
  :group 'biblio
  :type 'function)


;;; lib-biblio.el ends here
