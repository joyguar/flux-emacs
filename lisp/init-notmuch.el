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

(use-package message
  :straight nil
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
  
(use-package notmuch
  :general
  (leader-def
    "m" '(notmuch :which-key "Notmuch"))
  :config
  (setq
   notmuch-fcc-dirs `(("danielchlucas@gmail.com" . "gmail/sent")
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
       :key ,(kbd "m"))))

  ;;; tags
  (setq
   notmuch-archive-tags '("-inbox")
   notmuch-message-replied-tags '("+replied")
   notmuch-message-forwardwed-tags '("+forwarded")
   notmuch-show-mark-read-tags '("-unread")
   notmuch-draft-tags '("+draft")
   notmuch-draft-folder "drafts"
   notmuch-draft-save-plaintext 'ask)

  ;;; email composition
  (setq
   notmuch-mua-compose-in 'current-window
   notmuch-mua-hidden-headers nil
   notmuch-address-command 'internal
   notmuch-always-prompt-for-sender t
   notmuch-mua-cite-function 'message-cite-original-without-signature
   notmuch-mua-reply-insert-header-p-function 'notmuch-show-reply-insert-header-p-never
   notmuch-mua-user-agent-function nil
   notmuch-maildir-use-notmuch-insert t
   notmuch-crypto-process-time t
   notmuch-crypto-get-keys-asynchronously t
   notmuch-mua-attachment-regexp "\\b\\(attache?ment\\|attached\\|attach\\|pi[èe]ce +jointe?\\)\\b")

  ;;; reading messages
  (setq
   notmuch-show-relative-dates t
   notmuch-show-all-multipart/alternative-parts nil
   notmuch-show-indent-messages-width 0
   notmuch-show-indent-multipart nil
   notmuch-show-part-button-default-action 'notmuch-show-view-part
   notmuch-show-text/html-blocked-images "."
   notmuch-wash-wrap-lines-length nil
   notmuch-unthread-show-out t
   notmuch-message-headers '("To" "Cc" "Subject" "Date")
   notmuch-message-headers-visible t)

  ;;; hooks
  (add-hook 'notmuch-mua-send-hook #'notmuch-mua-attachment-check)

  ;;; key bindings
  (let ((map global-map))
    (define-key map (kbd "C-c m") #'notmuch)
    (define-key map (kbd "C-x m") #'notmuch-mua-new-mail)) ; override `compose-mail'
  (let ((map notmuch-search-mode-map))
    (define-key map (kbd "/") #'notmuch-search-filter)
    (define-key map (kbd "r") #'notmuch-search-reply-to-thread)
    (define-key map (kbd "R") #'notmuch-search-reply-to-thread-sender))
  (let ((map notmuch-show-mode-map))
    (define-key map (kbd "r") #'notmuch-show-reply)
    (define-key map (kbd "R") #'notmuch-show-reply-sender))
  (define-key notmuch-hello-mode-map (kbd "C-<tab>") nil))

(provide 'init-notmuch)
;;; init-notmuch.el ends here
