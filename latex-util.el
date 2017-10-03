(defvar latex-util-patterns-for-label
  '("\\\\begin{\\([a-zA-Z0-9*]*\\)}"
    "\\\\\\(section\\)"
    "\\\\\\(item\\)"))

(defun latex-util-search-pattern (pattern)
  (save-excursion
    (let ((command-pattern pattern)
          search-result-alist)
      (if (re-search-backward command-pattern nil t)
          (let ((ms (match-string 1)))
            (set-text-properties 0 (length ms) nil ms)
            (setq search-result-alist `((name . ,ms)
                                        (point . ,(point)))))
        (setq search-result-alist `((name . nil)
                                    (point . 0))))
      search-result-alist)))

(defun latex-util-choose-nearest (search-results)
  (unless search-results (error "search-results is nil"))
  (let ((nearest (car search-results)))
    (dolist (sr search-results)
      (when (> (cdr (assoc 'point sr))
               (cdr (assoc 'point nearest)))
        (setq nearest sr)))
    nearest))

(defun latex-util-search-previous-command ()
  (let (search-results
        nearest)
    (dolist (pattern latex-util-patterns-for-label)
      (add-to-list 'search-results (latex-util-search-pattern pattern)))
    (cdr (assoc 'name (latex-util-choose-nearest search-results)))))

(defun latex-util-insert-label ()
  (interactive)
  (let ((label-type (latex-util-search-previous-command)))
    (insert (format "\\label{%s:}" label-type))
    (backward-char 1)))

(provide 'latex-util)
