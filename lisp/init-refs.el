;;; init-refs.el -*- lexical-binding: t; -*-
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

(use-package bibtex
  :straight nil
  :init
  (setq bibtex-dialect 'biblatex
        bibtex-auto-key-edit-before-use t
        bibtex-maintain-sorted-entries 'crossref
        bibtex-autokey-name-case-convert-function #'capitalize
        bibtex-autokey-titleword-case-convert-function #'capitalize
        bibtex-autokey-titleword-length 9
        bibtex-autokey-titlewords 5
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator ""
        bibtex-autokey-year-length 4))
  
(use-package ebib
  :init
  (setq
   ebib-default-directory organum-bibliography-directory
   ebib-preload-bib-files (list organum-bibliography-file))

  ;; let's not get stuck in the 80s
  (setq ebib-bibtex-dialect 'biblatex)

  ;; log time of creation
  (setq ebib-use-timestamp t)

  ;; citations
  (setq ebib-citations-insert-multiple t)

  ;; notes
  (setq
   ebib-notes-directory organum-references-directory
   ebib-notes-locations organum-references-directory
   ebib-notes-storage 'one-note-per-file)

  (setq ebib-notes-display-max-lines 10)

  (setq ebib-file-search-dirs (list "/home/loki/Sync/library/books"
                                    "/home/loki/Sync/library/papers"
                                    "/home/loki/Sync/library/articles")))

(use-package biblio)

(use-package ebib-biblio
  :straight nil
  :after (ebib biblio))

(provide 'init-refs)
;;; init-refs.el ends here
