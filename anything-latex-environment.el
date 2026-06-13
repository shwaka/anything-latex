;;; anything-latex-environment.el --- Environment utilities for anything-latex -*- lexical-binding: t; -*-

;;; Commentary:
;; Environment completion and insertion utilities for anything-latex.
;;
;; This file intentionally depends on some utility functions defined in
;; anything-latex-functions.el:
;;
;; - al-load-data
;; - al-add-data
;;
;; These dependencies are declared by `declare-function' to avoid
;; byte-compile warnings.

;;; Code:

(require 'rx)

(declare-function al-load-data "anything-latex-functions")
(declare-function al-add-data "anything-latex-functions")

(defvar al-indent-string)

(defvar al-additional-environment-list
  '()
  "Environment list added by a user.")

(defvar al-itemize-like-environment-list
  '("itemize" "enumerate")
  "Environments similar to itemize or enumerate.")

(defvar al-popular-environments-path
  (locate-file "anything-latex-popular-environments" load-path)
  "File containing a list of frequently used LaTeX environments.")

(defvar al-environments-history-path
  "~/.emacs.d/anything-latex-environments-history"
  "File to save history of LaTeX environments.")

(defvar al-additional-environment-argument-alist
  '(("minipage" . ((:type optional :default "t")
                   (:type mandatory :default "0.5\\linewidth")))

    ("tabular" . ((:type mandatory :default "")))
    ("array" . ((:type mandatory :default "")))
    ("thebibliography" . ((:type mandatory :default "99")))

    ("frame" . ((:type optional :default "")
                (:type mandatory :default "")))
    ("block" . ((:type mandatory :default "title")))
    ("exampleblock" . ((:type mandatory :default "title")))
    ("alertblock" . ((:type mandatory :default "title")))
    ("columns" . ((:type optional :default "t")))
    ("column" . ((:type mandatory :default ".5\\textwidth")))

    ("lstlisting" . ((:type optional :default "")))
    ("tcolorbox" . ((:type optional :default "")))
    ("tikzpicture" . ((:type optional :default "")))

    ("figure" . ((:type optional :default "htbp")))
    ("table" . ((:type optional :default "htbp"))))
  "Alist of environment argument specs added by a user.

Each element is of the form:

  (ENVNAME . ARG-SPECS)

where ENVNAME is a string and ARG-SPECS is a list of plists.

Each plist has the form:

  (:type optional :default DEFAULT)

or

  (:type mandatory :default DEFAULT)

For example:

  (setq al-additional-environment-argument-alist
        '((\"frame\" . ((:type optional :default \"\")
                       (:type mandatory :default \"\")))
          (\"block\" . ((:type mandatory :default \"title\")))
          (\"minipage\" . ((:type optional :default \"t\")
                           (:type mandatory :default \"\\\\linewidth\")))))

Then `al-insert-environment' inserts, for example:

  \\begin{block}{title}

  \\end{block}")

(defvar al-user-environment-list
  '()
  "Environment list found from the current TeX buffer.")

(defvar al-user-environment-argument-alist
  '()
  "Alist of environment argument specs found from \\newenvironment.")

(defvar al-environment-list
  '()
  "All environment names used by anything-latex.")

(defvar al-environment-argument-alist
  '()
  "All environment argument specs used by anything-latex.")


;;; Environment list

(defun al-get-environment-list ()
  "Return popular environment names."
  (delete-dups
   (append
    (when al-popular-environments-path
      (with-temp-buffer
        (insert-file-contents al-popular-environments-path)
        (split-string (buffer-string) "\n" t))))))

(defun al-search-environment (buffer)
  "Search \\newenvironment and \\renewenvironment names in BUFFER.

This function returns only environment names.  Argument information is
handled separately by `al-search-environment-arguments'."
  (let ((environment-pattern
         (rx "\\"                         ; backslash
             (? "re")                     ; optional \"re\"
             "newenvironment"
             (* space)
             "{"
             (* space)
             (group (+ (any alpha digit "@:_-"))
                    (? "*"))              ; environment name
             (* space)
             "}"))
        (environment-list nil))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward environment-pattern nil t)
          (push (match-string-no-properties 1) environment-list))
        (reverse environment-list)))))


;;; Parsing argument information of \newenvironment

(defun al-environment--skip-spaces ()
  "Skip spaces after \\newenvironment components."
  (skip-chars-forward " \t\n"))

(defun al-environment--read-bracket-arg ()
  "Read [...] at point and move point after it.

Return the contents without brackets, or nil if point is not at an
optional bracket argument.

This parser is intentionally simple and is intended for arguments such
as [2] and [default]."
  (al-environment--skip-spaces)
  (when (looking-at "\\[\\([^]\n]*\\)\\]")
    (let ((arg (match-string-no-properties 1)))
      (goto-char (match-end 0))
      arg)))

(defun al-environment--make-arg-specs (num-args optional-default)
  "Make environment argument specs from NUM-ARGS and OPTIONAL-DEFAULT.

Examples:

  [2]          -> {}{}
  [2][default] -> [default]{}
  [1][default] -> [default}"
  (let ((n (or num-args 0))
        (res nil))
    (when optional-default
      (push (list :type 'optional :default optional-default) res)
      (setq n (1- n)))
    (dotimes (_ n)
      (push (list :type 'mandatory :default "") res))
    (nreverse res)))

(defun al-search-environment-arguments (buffer)
  "Search \\newenvironment argument specs in BUFFER.

This supports the usual LaTeX forms:

  \\newenvironment{foo}
  \\newenvironment{foo}[2]
  \\newenvironment{foo}[2][default]

The return value is an alist of the form:

  ((\"foo\" . ((:type optional :default \"default\")
              (:type mandatory :default \"\"))))"
  (let ((environment-pattern
         (rx "\\"                         ; backslash
             (? "re")                     ; optional \"re\"
             "newenvironment"
             (* space)
             "{"
             (* space)
             (group (+ (any alpha digit "@:_-"))
                    (? "*"))              ; environment name
             (* space)
             "}"))
        (res nil))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward environment-pattern nil t)
          (let* ((envname (match-string-no-properties 1))
                 (num-args-str (al-environment--read-bracket-arg))
                 (num-args (when num-args-str
                             (string-to-number num-args-str)))
                 (optional-default
                  (when num-args
                    (al-environment--read-bracket-arg)))
                 (arg-specs
                  (al-environment--make-arg-specs
                   num-args optional-default)))
            (when arg-specs
              (push (cons envname arg-specs) res))))
        (reverse res)))))


;;; Initialization

(defun al-environment-init (buffer)
  "Initialize environment-related variables from BUFFER."
  (setq al-user-environment-list
        (al-search-environment buffer))
  (setq al-user-environment-argument-alist
        (al-search-environment-arguments buffer))
  (setq al-environment-list
        (delete-dups
         (append (al-load-data al-environments-history-path t)
                 al-user-environment-list
                 al-additional-environment-list
                 (al-get-environment-list))))
  (setq al-environment-argument-alist
        (append al-user-environment-argument-alist
                al-additional-environment-argument-alist)))


;;; Insertion

(defun al-environment-argument-string (envname)
  "Return argument string inserted after \\begin{ENVNAME}."
  (let ((arg-specs
         (alist-get envname
                    al-environment-argument-alist
                    nil nil #'equal)))
    (mapconcat
     (lambda (arg-spec)
       (let ((type (plist-get arg-spec :type))
             (default (or (plist-get arg-spec :default) "")))
         (pcase type
           ('optional
            (format "[%s]" default))
           ('mandatory
            (format "{%s}" default))
           (_
            ""))))
     arg-specs
     "")))

(defun al-insert-environment-func (envname &optional indent-increase itemize-like)
  "Insert ENVNAME environment.

If ENVNAME has argument specs in `al-environment-argument-alist',
insert them after \\begin{ENVNAME}."
  (let ((indent-base (make-string (current-column) ?\s))
        (default-text "")
        (arg-string (al-environment-argument-string envname)))
    (cond
     ((stringp indent-increase) t)
     ((eq indent-increase t)
      (setq indent-increase al-indent-string))
     ((not indent-increase)
      (setq indent-increase "")))
    (when itemize-like
      (setq default-text "\\item "))
    (insert
     (format "\\begin{%s}%s\n%s%s%s\n%s\\end{%s}"
             envname arg-string
             indent-base indent-increase default-text
             indent-base envname))
    (forward-line -1)
    (end-of-line)))

(defun al-insert-environment (envname save-history)
  "Insert ENVNAME environment.

If SAVE-HISTORY is non-nil, save ENVNAME to
`al-environments-history-path'."
  (when save-history
    (al-add-data envname al-environments-history-path))
  (let ((indent-increase t)
        (itemize-like nil))
    (when (member envname '("document"))
      (setq indent-increase nil))
    (when (member envname al-itemize-like-environment-list)
      (setq indent-increase t)
      (setq itemize-like t))
    (al-insert-environment-func envname indent-increase itemize-like)))

(defun al-insert-environment-save (envname)
  "Insert ENVNAME environment and save it to history."
  (al-insert-environment envname t))

(defun al-insert-environment-notsave (envname)
  "Insert ENVNAME environment without saving it to history."
  (al-insert-environment envname nil))


(provide 'anything-latex-environment)

;;; anything-latex-environment.el ends here
