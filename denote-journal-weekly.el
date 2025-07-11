;;; denote-journal-weekly.el --- Weekly journaling functions for Denote -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/denote-journal
;; Version: 0.1.1
;; Package-Requires: ((emacs "28.1") (denote "4.0.0") (denote-journal "0.1.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This package extends denote-journal with weekly journaling functionality.
;; Weekly entries start on Monday and provide navigation between weeks.
;; Supports multiple journal directories for different contexts (work/personal).

;;; Code:

(require 'denote-journal)
(require 'calendar)
(eval-when-compile (require 'cl-lib))

(defgroup denote-journal-weekly nil
  "Weekly journaling functions for Denote."
  :group 'denote-journal)

(defcustom denote-journal-weekly-contexts nil
  "Alist of weekly journal contexts with their configurations.
When nil, weekly journals will be stored in the `denote-journal-directory'
with the keyword \"weekly\", similar to how `denote-journal.el' works.

Each entry is of the form (CONTEXT-NAME . PLIST) where PLIST contains:
  :directory - Directory for this context's weekly journals
  :keyword   - Additional keyword for this context (added to `denote-journal-keyword')

Example:
  ((work
    :directory \"/path/to/work/weekly\"
    :keyword \"work-weekly\")
   (personal
    :directory \"/path/to/personal/weekly\"
    :keyword \"personal-weekly\"))"
  :group 'denote-journal-weekly
  :type '(choice (const :tag "Use denote-journal-directory with \"weekly\" keyword" nil)
                 (alist :key-type symbol
                        :value-type (plist :key-type keyword
                                           :value-type (choice string directory)))))

(defcustom denote-journal-weekly-filename-format 'week-signature
  "Format for weekly journal entry filenames.
The value can be:
- `week-signature': Use week number in signature like \"w13\" (recommended)
- `date-only': Use only the Monday date without week indication
- A string: Custom signature format string for `format-time-string' with %V for week number

When set to `week-signature', the signature will be \"w13\" where 13 is
the ISO week number. Note that Denote automatically converts signatures
to lowercase, so \"W13\" becomes \"w13\" in the actual filename."
  :group 'denote-journal-weekly
  :type '(choice (const :tag "Week in signature (w13)" week-signature)
                 (const :tag "No week indication" date-only)
                 (string :tag "Custom signature format string")))

(defvar denote-journal-weekly--context-stack nil
  "Stack of recently used contexts for weekly journal operations.")

;;;; Helper functions

(defun denote-journal-weekly-contexts ()
  "Return the list of available weekly journal contexts.
If no contexts are configured, return '(default)."
  (if (null denote-journal-weekly-contexts)
      '(default)
    (mapcar #'car denote-journal-weekly-contexts)))

(defun denote-journal-weekly--get-context (&optional context)
  "Get context, updating the context stack."
  (let ((ctx (or context (car denote-journal-weekly--context-stack) 'default)))
    (setq denote-journal-weekly--context-stack 
          (cons ctx (remove ctx denote-journal-weekly--context-stack)))
    ctx))

(defun denote-journal-weekly-context-directory (&optional context)
  "Return directory for CONTEXT, defaulting to current context.
If no contexts are configured, use the `denote-journal-directory'."
  (if (null denote-journal-weekly-contexts)
      (denote-journal-directory)
    (let* ((ctx (denote-journal-weekly--get-context context))
           (config (alist-get ctx denote-journal-weekly-contexts))
           (directory (plist-get config :directory)))
      (if directory
          (progn
            (when (not (file-directory-p directory))
              (make-directory directory :parents))
            (file-name-as-directory (expand-file-name directory)))
        (denote-journal-directory)))))

(defun denote-journal-weekly-context-keyword (&optional context)
  "Return additional keyword for CONTEXT, defaulting to current context.
If no contexts are configured, return \"weekly\"."
  (if (null denote-journal-weekly-contexts)
      "weekly"
    (let* ((ctx (denote-journal-weekly--get-context context))
           (config (alist-get ctx denote-journal-weekly-contexts)))
      (or (plist-get config :keyword) "weekly"))))

(defun denote-journal-weekly--get-monday-of-week (date)
  "Return the Monday of the week containing DATE."
  (let* ((decoded-time (decode-time date))
         (day-of-week (nth 6 decoded-time)) ; 0=Sunday, 1=Monday, etc.
         (days-since-monday (if (= day-of-week 0) 6 (1- day-of-week)))
         (monday-time (time-subtract date (days-to-time days-since-monday))))
    monday-time))

(defun denote-journal-weekly--get-signature (date)
  "Return signature for weekly journal based on DATE and format setting."
  (let ((monday (denote-journal-weekly--get-monday-of-week date)))
    (pcase denote-journal-weekly-filename-format
      ('week-signature
       (format "w%02d" (string-to-number (format-time-string "%V" monday))))
      ('date-only "")
      ((pred stringp)
       (format-time-string denote-journal-weekly-filename-format monday))
      (_ ""))))

(defun denote-journal-weekly--title-format (&optional date)
  "Return appropriate title for weekly journal entry.
With optional DATE, use it instead of the present date."
  (let* ((internal-date (or (denote-valid-date-p date) (current-time)))
         (monday (denote-journal-weekly--get-monday-of-week internal-date))
         (specifiers (pcase denote-journal-title-format
                       ((pred null)
                        (cons
                         (denote-title-prompt (format-time-string "%F" monday) "New weekly journal file TITLE")
                         :skip))
                       ((and (pred stringp) (pred string-blank-p))
                        (cons "" :skip))
                       ((pred stringp) denote-journal-title-format)
                       ('day "Week of %A")
                       ('day-date-month-year "Week of %A %e %B %Y")
                       ('day-date-month-year-24h "Week of %A %e %B %Y %H:%M")
                       ('day-date-month-year-12h "Week of %A %e %B %Y %I:%M %^p"))))
    (if (consp specifiers)
        (car specifiers)
      (format-time-string specifiers monday))))

(defun denote-journal-weekly--get-combined-keywords (&optional context)
  "Return combined journal and weekly keywords for CONTEXT."
  (append (denote-journal-keyword) 
          (list (denote-journal-weekly-context-keyword context))))

(defun denote-journal-weekly--keyword-regex (&optional context)
  "Return regex that matches both journal and weekly keywords for CONTEXT."
  (let* ((all-keywords (denote-journal-weekly--get-combined-keywords context))
         ;; Apply the same slugification that Denote uses for keywords
         (keywords-slugified (denote-sluggify-keywords-and-apply-rules all-keywords))
         (keywords-sorted (mapcar #'regexp-quote (denote-keywords-sort keywords-slugified))))
    (concat "_" (string-join keywords-sorted ".*_"))))

(defun denote-journal-weekly--filename-date-regexp (&optional date context)
  "Regular expression to match weekly journal entries for DATE's week in CONTEXT.
DATE has the same format as that returned by `denote-valid-date-p'."
  (let* ((internal-date (or (denote-valid-date-p date) (current-time)))
         (monday (denote-journal-weekly--get-monday-of-week internal-date))
         ;; Use proper Denote identifier format (date-time only)
         (identifier (format "%sT[0-9]\\{6\\}" (format-time-string "%Y%m%d" monday)))
         (signature (denote-journal-weekly--get-signature internal-date))
         (keyword-pattern (denote-journal-weekly--keyword-regex context)))
    ;; Build regex to match files with the Monday date and correct keywords
    ;; Handle both with and without signature for backward compatibility
    (if (string-empty-p signature)
        ;; No signature case - match identifier followed by keywords
        (format "%s.*?%s" identifier keyword-pattern)
      ;; With signature case - match identifier, signature, and keywords
      ;; The signature gets sluggified by Denote, so match the actual result
      (format "%s==%s.*?%s" identifier (regexp-quote signature) keyword-pattern))))

(defun denote-journal-weekly--entry-for-week (&optional date context)
  "Return list of files matching a weekly journal for DATE's week in CONTEXT.
DATE has the same format as that returned by `denote-valid-date-p'."
  (let ((denote-journal-directory (denote-journal-weekly-context-directory context)))
    (denote-directory-files (denote-journal-weekly--filename-date-regexp date context))))


(defun denote-journal-weekly--file-is-weekly-p (file &optional context)
  "Return non-nil if FILE is a weekly journal entry in CONTEXT."
  (and (denote-journal-file-is-journal-p file)
       (string-match-p (regexp-quote (denote-journal-weekly-context-keyword context))
                       (file-name-nondirectory file))))

(defun denote-journal-weekly--current-file-context-and-monday ()
  "Return (CONTEXT . MONDAY) for current weekly journal file, or nil."
  (when buffer-file-name
    (let ((contexts (denote-journal-weekly-contexts)))
      (cl-loop for context in contexts
               when (and (denote-journal-weekly--file-is-weekly-p buffer-file-name context)
                         (string-prefix-p (denote-journal-weekly-context-directory context)
                                          buffer-file-name))
               return (cons context
                            (when-let* ((identifier (denote-retrieve-filename-identifier buffer-file-name))
                                        ;; Use proper Denote identifier format (YYYYMMDD only)
                                        (date-string (substring identifier 0 8))
                                        (year (string-to-number (substring date-string 0 4)))
                                        (month (string-to-number (substring date-string 4 6)))
                                        (day (string-to-number (substring date-string 6 8))))
                              (encode-time 0 0 0 day month year)))))))

(defun denote-journal-weekly--prompt-for-context ()
  "Prompt user to select a weekly journal context.
If no contexts are configured, return 'default."
  (if (null denote-journal-weekly-contexts)
      'default
    (let ((contexts (denote-journal-weekly-contexts)))
      (if (= (length contexts) 1)
          (car contexts)
        (intern (completing-read "Weekly journal context: "
                                 (mapcar #'symbol-name contexts)
                                 nil t))))))

;;;; Main functions

;;;###autoload
(defun denote-journal-weekly-new-entry (&optional date context)
  "Create a new weekly journal entry starting on Monday.
Use the variable `denote-journal-keyword' plus context-specific keyword
as keywords for the newly created file. Set the title according to the value
of the user option `denote-journal-title-format', but adapted for weekly format.

Uses proper Denote identifier format with optional week signature.

With optional DATE as a prefix argument, prompt for a date. The weekly
entry will be created for the week containing that date, starting on Monday.

With optional CONTEXT, use that context. When called interactively with
double prefix argument, prompt for context.

When called from Lisp DATE is a string and has the same format as
that covered in the documentation of the `denote' function."
  (interactive
   (list (when (member current-prefix-arg '((4) (16))) (denote-date-prompt))
         (when (equal current-prefix-arg '(16)) (denote-journal-weekly--prompt-for-context))))
  (let* ((ctx (denote-journal-weekly--get-context context))
         (internal-date (or (denote-valid-date-p date) (current-time)))
         (monday (denote-journal-weekly--get-monday-of-week internal-date))
         (monday-string (format-time-string "%Y-%m-%d" monday))
         (signature (denote-journal-weekly--get-signature internal-date))
         (denote-directory (denote-journal-weekly-context-directory ctx)))
    (denote
     (denote-journal-weekly--title-format internal-date)
     (denote-journal-weekly--get-combined-keywords ctx)
     nil nil monday-string
     (denote-journal--get-template)
     signature)))

;;;###autoload
(defun denote-journal-weekly-path-to-new-or-existing-entry (&optional date context)
  "Return path to existing or new weekly journal file.
With optional DATE, do it for that week, else do it for current week.
With optional CONTEXT, use that context.
DATE is a string and has the same format as that covered in the
documentation of the `denote' function.

If there are multiple weekly journal entries for the week, prompt for
one among them using minibuffer completion. If there is only one,
return it. If there is no weekly journal entry, create it."
  (let* ((ctx (denote-journal-weekly--get-context context))
         (internal-date (or (denote-valid-date-p date) (current-time)))
         (files (denote-journal-weekly--entry-for-week internal-date ctx))
         (denote-kill-buffers nil)
         (denote-directory (denote-journal-weekly-context-directory ctx)))
    (if files
        (denote-journal-select-file-prompt files)
      (save-window-excursion
        (denote-journal-weekly-new-entry date ctx)
        (save-buffer)
        (buffer-file-name)))))

;;;###autoload
(defun denote-journal-weekly-new-or-existing-entry (&optional date context)
  "Locate an existing weekly journal entry or create a new one.
A weekly journal entry is one that has both the `denote-journal-keyword'
and context-specific keyword as part of its file name and starts
on Monday of the relevant week.

If there are multiple weekly journal entries for the current week,
prompt for one using minibuffer completion. If there is only
one, visit it outright. If there is no weekly journal entry, create one
by calling `denote-journal-weekly-new-entry'.

With optional DATE as a prefix argument, prompt for a date. The weekly
entry will be for the week containing that date.

With optional CONTEXT, use that context. When called interactively with
double prefix argument, prompt for context.

When called from Lisp, DATE is a string and has the same format
as that covered in the documentation of the `denote' function."
  (interactive
   (list (when (member current-prefix-arg '((4) (16))) (denote-date-prompt))
         (when (equal current-prefix-arg '(16)) (denote-journal-weekly--prompt-for-context))))
  (let ((ctx (denote-journal-weekly--get-context context)))
    (find-file (denote-journal-weekly-path-to-new-or-existing-entry date ctx))))

;;;###autoload
(defun denote-journal-weekly-link-or-create-entry (&optional date context id-only)
  "Use `denote-link' on weekly journal entry, creating it if necessary.
A weekly journal entry is one that has both the `denote-journal-keyword'
and context-specific keyword as part of its file name and starts
on Monday of the relevant week.

If there are multiple weekly journal entries for the current week,
prompt for one using minibuffer completion. If there is only
one, link to it outright. If there is no weekly journal entry, create one
by calling `denote-journal-weekly-new-entry' and link to it.

With optional DATE as a prefix argument, prompt for a date. The weekly
entry will be for the week containing that date.

With optional CONTEXT, use that context.

When called from Lisp, DATE is a string and has the same format
as that covered in the documentation of the `denote' function.

With optional ID-ONLY as a prefix argument create a link that
consists of just the identifier. Else try to also include the
file's title. This has the same meaning as in `denote-link'."
  (interactive
   (pcase current-prefix-arg
     ('(16) (list (denote-date-prompt) (denote-journal-weekly--prompt-for-context) :id-only))
     ('(4) (list (denote-date-prompt)))))
  (let* ((ctx (denote-journal-weekly--get-context context))
         (path (denote-journal-weekly-path-to-new-or-existing-entry date ctx)))
    (denote-link path
                 (denote-filetype-heuristics (buffer-file-name))
                 (denote-get-link-description path)
                 id-only)))

;;;; Navigation functions

;;;###autoload
(defun denote-journal-weekly-previous-entry ()
  "Navigate to the previous weekly journal entry.
This function works when called from within a weekly journal entry.
If no previous entry exists, create one."
  (interactive)
  (if-let* ((context-and-monday (denote-journal-weekly--current-file-context-and-monday))
            (context (car context-and-monday))
            (current-monday (cdr context-and-monday)))
      (let* ((previous-monday (time-subtract current-monday (days-to-time 7)))
             (previous-date-string (format-time-string "%Y-%m-%d" previous-monday)))
        (denote-journal-weekly-new-or-existing-entry previous-date-string context))
    (user-error "Current buffer is not a weekly journal entry")))

;;;###autoload
(defun denote-journal-weekly-next-entry ()
  "Navigate to the next weekly journal entry.
This function works when called from within a weekly journal entry.
If no next entry exists, create one."
  (interactive)
  (if-let* ((context-and-monday (denote-journal-weekly--current-file-context-and-monday))
            (context (car context-and-monday))
            (current-monday (cdr context-and-monday)))
      (let* ((next-monday (time-add current-monday (days-to-time 7)))
             (next-date-string (format-time-string "%Y-%m-%d" next-monday)))
        (denote-journal-weekly-new-or-existing-entry next-date-string context))
    (user-error "Current buffer is not a weekly journal entry")))

;;;###autoload
(defun denote-journal-weekly-goto-current (&optional context)
  "Go to current week's journal entry, creating it if necessary.
With optional CONTEXT, use that context. When called interactively with
prefix argument, prompt for context."
  (interactive
   (list (when current-prefix-arg (denote-journal-weekly--prompt-for-context))))
  (let ((ctx (denote-journal-weekly--get-context context)))
    (denote-journal-weekly-new-or-existing-entry (format-time-string "%Y-%m-%d" (current-time)) ctx)))

(provide 'denote-journal-weekly)
;;; denote-journal-weekly.el ends here
