;;; init-notmuch.el -*- lexical-binding: t; -*-
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

(require 'sendmail)

(defvar mail-directory "~/.mail")

(defvar gmail-directory "~/.mail/gmail")

(defvar fastmail-directory "~/.mail/fastmail")

(setq sendmail-program "/usr/bin/msmtp"
      mail-user-agent 'message-user-agent
      mail-specify-envelope-from t)

(elpa-use-package message
  :config
  (setq
   message-directory mail-directory
   mail-user-agent 'message-user-agent
   message-send-mail-function 'message-send-mail-with-sendmail
   mail-header-separator (purecopy "*****")
   message-elide-ellipsis "\n[,,,]\n\n"
   compose-mail-user-agent-warnings nil
   message-mail-user-agent t
   mail-signature "Daniel Lucas"
   message-signature "Daniel Lucas"
   message-specify-envelope-from t
   message-ignored-cited-headers "."
   message-confirm-send nil
   message-kill-buffer-on-exit t
   message-wide-reply-confirm-recipients t))

(elpa-use-package notmuch
  :commands notmuch
  :bind (("C-x m" . notmuch-mua-new-mail)
         ("C-c m" . notmuch-jump-search)
         :map notmuch-search-mode-map
         ("/" . notmuch-search-filter)
         ("r" . notmuch-search-reply-to-thread)
         ("R" . notmuch-search-reply-to-thread-sender)
         :map notmuch-show-mode-map
         ("r" . notmuch-show-reply)
         ("R" . notmuch-show-reply-sender))
  :hook ((notmuch-message-mode . turn-off-auto-fill)
         (notmuch-mua-send . notmuch-mua-attachment-check))
  :config
  (setq notmuch-fcc-dirs `(("danielchlucas@gmail.com" . "gmail/sent")
                           ("dclucas@fastmail.com" . "fastmail/sent")))
   ;;; UI
  (setq
   notmuch-show-logo nil
   notmuch-column-control t
   notmuch-hello-auto-refresh t
   notmuch-recent-searches-max 10
   notmuch-hello-thousands-seperator " "
   notmuch-hello-sections '(notmuch-hello-insert-header
                            notmuch-hello-insert-saved-searches
                            notmuch-hello-insert-search
                            notmuch-hello-insert-recent-searches
                            notmuch-hello-insert-alltags
                            notmuch-hello-insert-footer)
   notmuch-show-all-tags-list t)

  (setq
   notmuch-search-oldest-first nil
   notmuch-search-result-format
   '(("date" . "%12s  ")
     ("count" . "%-7s  ")
     ("authors" . "%-20s  ")
     ("subject" . "%s  ")
     ("tags" . "(%s)"))
   notmuch-tree-result-format
   '(("date" . "%12s  ")
     ("authors" . "%-20s  ")
     ((("tree" . "%s")
       ("subject" . "%s"))
      . " %-54s  ")
     ("tags" . "(%s)"))
   notmuch-show-empty-saved-searches t
   notmuch-saved-searches
   `(( :name "gmail inbox"
       :query "folder:gmail/inbox"
       :sort-order newest-first
       :key ,(kbd "i"))
     ( :name "gmail unread (inbox)"
       :query "tag:unread AND folder:gmail/inbox"
       :sort-order newest-first
       :key ,(kbd "u"))
     ( :name "gmail unread all"
       :query "tag:gmail AND tag:unread"
       :sort-order newest-first
       :key ,(kbd "U"))
     ( :name "mailing lists"
       :query "tag:list AND_NOT tag:archive"
       :sort-order newest-first
       :key ,(kbd "m")))))
  

(provide 'init-notmuch)
;;; init-notmuch.el ends here
