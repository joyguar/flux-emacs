;;; init-editor.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Daniel Lucas
;;
;; Author: Daniel Lucas <dclucas@protonmail.com>
;; Maintainer: Daniel Lucas <dclucas@protonmail.com>
;; Created: June 23, 2022
;; Modified: June 23, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/danielclucas/init-editor.el
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; much of this is taken from the doom core-editor
;; https://github.com/doomemacs/doomemacs/blob/master/core/core-editor.el
;;
;;; Code:

(require 'config-path)

;;
;;; File handling

;; Resolve symlinks when opening files, so that any operations are conducted
;; from the file's true directory (like `find-file').
(setq find-file-visit-truename t
      vc-follow-symlinks t)

;; Create missing directories when we open a file that doesn't exist under a
;; directory tree that may not exist.
(add-hook 'find-file-not-found-functions
          (defun flux-create-missing-directories ()
            "Automatically create missing directories when creating new files."
            (unless (file-remote-p buffer-file-name)
              (let ((parent-directory (file-name-directory buffer-file-name)))
                (and (not (file-directory-p parent-directory))
                     (y-or-n-p (format "Directory `%s' does not exist! Create it?"
                                       parent-directory))
                     (progn (make-directory parent-directory 'parents)
                            t))))))

;; Disable the warning "X and Y are the same file". It's fine to ignore this
;; warning as it will redirect you to the existing buffer anyway.
(setq find-file-suppress-same-file-warnings t)

;; Don't generate backups or lockfiles. While auto-save maintains a copy so long
;; as a buffer is unsaved, backups create copies once, when the file is first
;; written, and never again until it is killed and reopened. This is better
;; suited to version control, and I don't want world-readable copies of
;; potentially sensitive material floating around our filesystem.
(setq create-lockfiles nil
      make-backup-files nil)

;; auto-save files
(setq auto-save-default t
      auto-save-include-big-deletions t
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" path-cache-dir)
      auto-save-file-name-transforms `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "/tmp/\\2" t)))
 
;;
;;; Formatting

;; indenting
(setq-default indent-tabs-mode nil
              tab-width 4
              tab-always-indent nil)

;; fill column
(setq-default fill-column 80)

;; Line wrapping
(setq-default word-wrap t)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)

;; Relic of typewriters
(setq sentence-end-double-space nil)

;; Posix compliance
(setq require-final-newline t)


;;
;;; Clipboard / kill-ring

;; Make yanking work properly on selected regions
(delete-selection-mode 1)

;; Cull duplicates in the kill ring to reduce bloat
(setq kill-do-not-save-duplicates t)


;;
;;; Built-in plugins

(setq bookmark-default-file (expand-file-name "bookmarks" path-etc-dir))

(use-package recentf
  :hook (after-init . recentf-mode)
  :commands recentf-open-files
  :init
  (setq recentf-save-file (expand-file-name "recentf" path-cache-dir))
  :config
  (setq recentf-auto-cleanup nil
        recentf-max-saved-items 100)

  ;; The most sensible time to clean up your recent files list is when you quit
  ;; Emacs (unless this is a long-running daemon session).
  (setq recentf-auto-cleanup (if (daemonp) 300))
  (add-hook 'kill-emacs-hook #'recentf-cleanup))

(use-package savehist
  :init
  (setq savehist-file (concat path-cache-dir "savehist")
        savehist-savemini-buffer-history t
        savehist-autosave-interval nil
        avehist-additional-variables
        '(kill-ring
          mark-ring global-mark-ring
          search-ring regexp-search-ring)))

(provide 'init-editor)
;;; init-editor.el ends here
