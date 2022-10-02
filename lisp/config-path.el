;;; config-path.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Daniel Lucas
;;
;; Author: Daniel Lucas <dclucas@protonmail.com>
;; Maintainer: Daniel Lucas <dclucas@protonmail.com>
;; Created: May 26, 2022
;; Modified: May 26, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/danielclucas/config-path
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This module defines path constants used across other modules.
;;
;;  Description
;;
;;; Code:

(defconst path-home-dir (file-name-as-directory (getenv "HOME"))
  "Path to user home directory.
In a nutshell, it's just a value of $HOME.")

(defconst path-config-dir
  (file-name-as-directory
   (or (getenv "XDG_CONFIG_HOME")
       (concat path-home-dir ".config")))
  "The root directory for personal configurations.")

(defconst path-emacs-dir
  (file-name-as-directory
   (expand-file-name "flux-emacs/" path-config-dir))
  "The path to this Emacs directory.")

(defconst path-autoloads-file
  (expand-file-name "lisp/init-autoloads.el" path-emacs-dir)
  "The path to personal autoloads file.")

(defconst path-emacs-private-dir
  (concat path-home-dir "Sync/apps/Emacs/")
  "The root directory for private configurations.")

(defconst path-local-dir
  (concat path-emacs-dir ".local/")
  "The root directory for local Emacs files.
Use this as permanent storage for files that are safe to share
across systems.")

(defconst path-etc-dir (concat path-local-dir "etc/")
  "Directory for non-volatile storage.
Use this for files that don't change much, like servers binaries,
external dependencies or long-term shared data.")

(defconst path-cache-dir (concat path-local-dir "cache/")
  "Directory for volatile storage.
Use this for files that change often, like cache files.")

(defconst path-packages-dir
  (expand-file-name (format "packages/%s.%s/"
                            emacs-major-version
                            emacs-minor-version)
                    path-local-dir)
  "Where packages are stored.")

(provide 'config-path)
;;; config-path.el ends here
