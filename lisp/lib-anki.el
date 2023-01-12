;;; lib-anki.el --- Description -*- lexical-binding: t; -*-
(require 'anki-editor)

;;;###autoload
(defun anki-editor-cloze-region-auto-incr ()
  "Cloze region without hint and increase card number."
  (interactive)
  (anki-editor-cloze-region my-anki-editor-cloze-number "")
  (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
  (forward-sexp))

;;;###autoload
(defun anki-editor-cloze-region-dont-incr ()
  "Cloze region without hint using the previous card number."
  (interactive)
  (anki-editor-cloze-region (1- my-anki-editor-cloze-number) "")
  (forward-sexp))

;;;###autoload
(defun anki-editor-reset-cloze-number (&optional arg)
  "Reset cloze number to ARG or 1"
  (interactive)
  (setq my-anki-editor-cloze-number (or arg 1)))

;;;###autoload
(defun anki-editor-push-tree ()
  "Push all notes under a tree."
  (interactive)
  (anki-editor-push-notes '(4))
  (anki-editor-reset-cloze-number))

(provide 'lib-anki)
;;; lib-anki.el ends here
