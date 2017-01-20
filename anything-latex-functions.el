(defun al-get-config (buffer)
  "search %ALCONFIG: variable = value"
  (let ((config-pattern "^ *%+ALCONFIG: *\\([a-zA-Z]+\\) *= *\\([a-zA-Z0-9]*\\)")
	(config-alist ())
	(var-name nil)
	(var-value nil))
    (with-current-buffer buffer
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward config-pattern nil t)
	  (setq var-name (match-string 1))
	  (setq var-value (match-string 2))
	  (add-to-list 'config-alist (cons (intern var-name) var-value)))
	config-alist))))

(defun al-set-config (buffer)
  "set config of the form '%ALCONFIG: variable = value'"
  (setq al-config-alist (al-get-config buffer)))

(defun al-search-label (buffer)
  "search \\label{hoge}"
  (interactive)
  (let ((res ()) (pattern "\\\\label{\\(.*?\\)}")) ; \\ref{\(.*?\)}
    (with-current-buffer buffer
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward pattern nil t)
	  (add-to-list 'res (match-string 1)))
	;; (message (number-to-string (length res)))
	(reverse res)))))

(defun al-find-label (buffer label)
  (let ((label-pattern (regexp-opt (list (format "\\label{%s}" label)))))
    (with-current-buffer buffer
      (save-excursion
	(goto-char (point-min))
	(re-search-forward label-pattern nil t)
	(re-search-backward label-pattern nil t)
	(point)))))

(defun al-find-bib-file (buffer)
  (interactive "sBuffer: ")
  (let ((pattern "^\\\\bibliography{\\(.*\\)}"))  ;"^[^%]*\\\\bibliography{\\(.*\\)}" slow
    (with-current-buffer buffer
      (save-excursion
	(goto-char (point-min))
	(if (not (re-search-forward pattern nil t))
	    (error "\\bibliography{...} not found")
	  (substring (shell-command-to-string
	  	      (format "kpsewhich %s.bib" (match-string 1)))
	  	     0 -1))))))

(defun al-search-theorem (buffer)
  (let ((theorem-pattern "\\\\newtheorem{\\([a-zA-Z]*\\)}\\(?:\[[a-zA-Z]*\]\\)?{\\([a-zA-Z]*\\)}")
	(theorem-list ()))
    (with-current-buffer buffer
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward theorem-pattern nil t)
	  (add-to-list 'theorem-list (match-string 1)))
	(reverse theorem-list)))))

(defun al-use-cleveref-p (buffer)
  (let ((cleveref-pattern "\\\\usepackage\\(?:\\[[a-zA-Z, ]*\\]\\)?{cleveref}")
	(use-cleveref nil))
    (with-current-buffer buffer
      (save-excursion
	(goto-char (point-min))
	(if (re-search-forward cleveref-pattern nil t)
	    (setq use-cleveref t)
	  (setq use-cleveref nil))
	use-cleveref))))

(defun al-1-arg-pattern (ctrl-seq &optional arg)
  (unless arg
    (setq arg "[a-zA-Z]+"))
  (format "\\\\%s{\\(%s\\)}" ctrl-seq arg))

(defun al-get-env (buffer pt)
  (let ((env-data-alist ())
	env-name
	begin-pattern
	end-pattern
	found
	begin-pt
	end-pt)
    (with-current-buffer buffer
      (save-excursion
	(goto-char pt)
	(while (not found)
	  (setq begin-pattern (al-1-arg-pattern "begin"))
	  (unless (re-search-backward begin-pattern nil t)
	    (setq found "failed")) 
	  (setq env-name (match-string 1))
	  (setq begin-pt (point))
	  (setq end-pattern (al-1-arg-pattern "end" env-name))
	  (unless (re-search-forward end-pattern nil t)
	    (setq found "failed"))
	  (setq end-pt (point))
	  (when (< begin-pt pt end-pt)
	    (setq found t)
	    (setq env-data-alist `((env-name . ,env-name)
				   (begin-pt . ,begin-pt)
				   (end-pt . ,end-pt))))
	  (goto-char begin-pt))
	(if (equal found "failed")
	    (setq env-data-alist nil))
	env-data-alist))))

(defun al-label-init (&optional buffer)
  (unless buffer
    (setq buffer anything-current-buffer))
  (setq al-use-cleveref (al-use-cleveref-p buffer))
  (setq al-theorem-list (al-search-theorem buffer)))

(defun al-bibkey-init (&optional buffer)
  (unless buffer
    (setq buffer anything-current-buffer))
  (setq al-bib-file (al-find-bib-file buffer)))

(defun al-init (buffer)
  (al-label-init buffer)
  (al-bibkey-init buffer)
  (al-set-config buffer)
  (let ((reftype (cdr (assq 'reftype al-config-alist))))
    (if reftype
	(setq al-reftype reftype)
      (if al-use-cleveref
	  (setq al-reftype "cref")
	(setq al-reftype "ref")))))

(defun al-bibkey-pattern (&optional bibkey)
  (if bibkey
      (setq bibkey (regexp-opt (list bibkey)))
    (setq bibkey ".*"))
  (format "^@[a-zA-Z]* *{\\(%s\\)," bibkey))

(defun al-find-bibkeys ()
  (let ((res ())
	)
    ;; (find-file-noselect bib-file t)
    (with-temp-buffer
      (insert-file-contents al-bib-file)
      (goto-char (point-min))
      (while (re-search-forward (al-bibkey-pattern) nil t)
	(add-to-list 'res (match-string 1))))
    res))

(defun al-insert-ctrl-seq (ctrl-seq key &optional option)
  "insert \\ctrl-seq{key} or \\ctrl-seq[option]{key}"
  (if option 				; if option is set (and non-nil)
      (insert "\\" ctrl-seq "[" option "]{" key "}")
    (insert "\\" ctrl-seq "{" key "}")))

(defun al-insert-option-to-previous-ctrl-seq (&optional option)
  (interactive)
  (re-search-backward "\\\\[a-zA-Z]+{" nil t)
  (re-search-forward "\\\\[a-zA-Z]*" nil t)
  (insert "[]")
  (forward-char -1)
  (when option
    (insert option)))

(defun al-check-option ()
  (when (equal ?\[ last-command-event)
    (setq this-command 'al-insert-option-to-previous-ctrl-seq))
  (remove-hook 'pre-command-hook 'al-check-option))

(defun al-insert-default-ref (label)
  "insert \\Xref{label} with X corresponding to config"
  (al-insert-ctrl-seq al-reftype label))

;; (defun al-insert-ref (label)
;;   "insert \\ref{label}"
;;   (interactive "sLabel: ")
;;   ;; (insert "\\ref{" label "}")
;;   (al-insert-ctrl-seq "ref" label))

(defun al-insert-cite (bibkey)
  "insert \\cite{label}"
  (interactive "sLabel: ")
  ;; (insert "\\cite{" bibkey "}")
  (al-insert-ctrl-seq "cite" bibkey)
  (add-hook 'pre-command-hook 'al-check-option))

(defun al-insert-theorem (theorem)
  ;; TODO: indent
  (insert (format "\\begin{%s}\n \n\\end{%s}" theorem theorem))
  (re-search-backward "\\\\end{" nil t)
  (backward-char))

;;; persistent-action
(defun al-show-persistent (string)
  (let ((al-help-buffer "*al-help*"))
    (get-buffer-create al-help-buffer)
    (switch-to-buffer al-help-buffer)
    (erase-buffer)
    (insert string)))

(defun al-show-persistent-label (label)
  (with-current-buffer anything-current-buffer
    (save-excursion
      (let (label-point
	    env-data-alist
	    env-name
	    show-start
	    show-end
	    show-string
	    (show-line 5)
	    )
	(setq label-point (al-find-label (current-buffer) label))
	(setq env-data-alist (al-get-env (current-buffer) label-point))
	(setq env-name (cdr (assq 'env-name env-data-alist)))
	(cond ((and env-name
		    (member env-name al-theorem-list))
	       (setq show-start (cdr (assq 'begin-pt env-data-alist)))
	       (setq show-end (cdr (assq 'end-pt env-data-alist)))
	       )
	      (t
	       (goto-char label-point)
	       (forward-line (- show-line))
	       (beginning-of-line)
	       (setq show-start (point))
	       (forward-line (+ 1 (* 2 show-line)))
	       (end-of-line)
	       (setq show-end (point))
	       ))
	(setq show-string (buffer-substring show-start show-end))
	(al-show-persistent show-string)))))

(defun al-show-persistent-bib (bibkey)
  (with-temp-buffer
    (let ((bib-start nil) (bib-end nil))
      (insert-file-contents al-bib-file)
      (goto-char (point-min))
      (re-search-forward (al-bibkey-pattern bibkey) nil t)
      (beginning-of-line)
      (setq bib-start (point))
      (re-search-forward "^}" nil t)
      (setq bib-end (point))
      (al-show-persistent (buffer-substring bib-start bib-end)))
    ))

(defun al-jump-label (label)
  (with-current-buffer
      (goto-char (al-find-label (current-buffer) label))))

(defun al-label-get-actions ()
  (let ((actions ()))
    (if al-use-cleveref
	(setq actions '(("Insert cref" . (lambda (label) (al-insert-ctrl-seq "cref" label)))
			("Insert ref" . (lambda (label) (al-insert-ctrl-seq "ref" label)))))
      (setq actions '(("Insert ref" . (lambda (label) (al-insert-ctrl-seq "ref" label))))))
    actions))

(provide 'anything-latex-functions)
