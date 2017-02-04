;;; This elisp file is independent of anything.el.
;;; Hence you can use this not only in anything.el but also other interface.

(require 'em-glob)

;;; variables
(defvar al-default-theorem-list
  '("definition" "theorem" "proposition" "lemma" "corollary" "example" "remark")
  "theorem list")

(defvar al-default-environment-list
  '("document" "itemize" "enumerate" "math" "equation" "eqnarray" "frame" "cases" "array" "proof" "abstract" "quote" "quotation"))

(defvar al-texmf-dirs
  (split-string
   (shell-command-to-string "kpsewhich -expand-path='$TEXMF' | perl -pe \"s/\\n//\"")
   ":" t)
  "list of texmf directories")

;;; TODO: remove full path
;; (defvar al-used-packages-file
;;   "~/Git/anything-latex/anything-latex-used-packages"
;;   "file containing a list of frequently used packages")
(defvar al-popular-files-path
  "~/Git/anything-latex/anything-latex-popular-files"
  "file containing a list of frequently used latex files")

;; (defvar al-package-history-file
;;   "~/.emacs.d/anything-latex-package-history"
;;   "file to save history of \\usepackage")
(defvar al-files-history-path
  "~/.emacs.d/anything-latex-files-history"
  "file to save history of latex files")

(defvar al-shell-command-list-files
  ;; "find $(kpsewhich -expand-path='$TEXMF' | sed -e \"s/:/ /g\")"
  (format "find %s \\( -name fonts -prune \\) -or \\( -xtype f -printf \"%%f\n\" \\)"
	  (apply 'concat (mapcar
			  #'(lambda (s) (concat s " "))
			  al-texmf-dirs)))
  "command to list files in texmf directories")

(defvar al-insert-ctrl-seq-alist
  '(("cls" . ((ctrl-seq . "documentclass")
	      (wait-option . t)))
    ("sty" . ((ctrl-seq . "usepackage")
	      (wait-option . t)))
    ("bib" . ((ctrl-seq . "bibliography")
	      (wait-option nil)))
    ("bst" . ((ctrl-seq . "bibliographystyle")
	      (wait-option nil)))
    ))

;;; functions
(defun al-save-data (data filename)
  (let ((str (format "%S" data)))
    (write-region str nil filename nil 'silent)))

(defun al-load-data (filename &optional noerror)
  (when (or (not noerror)
	    (file-exists-p filename))
    (with-temp-buffer
    (insert-file-contents filename)
    (read (current-buffer)))))

;;; TODO: current directory, beamer
(defun al-get-files-list ()
  (concat
   ;; from history
   (apply 'concat (mapcar
		   #'(lambda (s) (concat s "\n"))
		   (al-load-data al-files-history-path t)))
   ;; from popular files
   (with-temp-buffer
     (insert-file-contents al-popular-files-path)
     (buffer-substring-no-properties (point-min) (point-max)))
   "\n"
   ;; all files
   (shell-command-to-string al-shell-command-list-files)))

;;; functions

;;; exclude fonts??? (see completion of kpsewhich)
;; (defun al-list-files-in-texmf ()
;;   (let ((files-list ())
;; 	glob-res)
;;     (dolist (texmf al-texmf-dirs)
;;       (setq glob-res (eshell-extended-glob (concat texmf "/**/*")))
;;       (when (not (listp glob-res))
;; 	(setq glob-res ()))
;;       (setq files-list (append files-list glob-res)))
;;     files-list))

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
	    nil
	    ;; (error "\\bibliography{...} not found")
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
  (let ((cleveref-pattern "^[^%]*\\\\usepackage\\(?:\\[[a-zA-Z, ]*\\]\\)?{cleveref}")
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

(defun al-label-init (buffer)
  (setq al-use-cleveref (al-use-cleveref-p buffer))
  (setq al-user-theorem-list (al-search-theorem buffer))
  (setq al-theorem-list (append al-user-theorem-list al-default-theorem-list)))

(defun al-bibkey-init (buffer)
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
	(setq al-reftype "ref"))))
  ;; (setq al-texmf-files-list (al-list-files-in-texmf))
  )

(defun al-bibkey-pattern (&optional bibkey)
  (if bibkey
      (setq bibkey (regexp-opt (list bibkey)))
    (setq bibkey ".*"))
  (format "^@[a-zA-Z]* *{\\(%s\\)," bibkey))

(defun al-find-bibkeys ()
  (if (not al-bib-file)
      nil
    (let ((res ()))
      ;; (find-file-noselect bib-file t)
      (with-temp-buffer
	(insert-file-contents al-bib-file)
	(goto-char (point-min))
	(while (re-search-forward (al-bibkey-pattern) nil t)
	  (add-to-list 'res (match-string 1))))
      res))
  )

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

(defun al-wait-option ()
  (add-hook 'pre-command-hook 'al-check-option))

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

;; (defun al-insert-package (package)
;;   "insert \\usepackage{package}"
;;   (al-save-data (delete-dups (cons package (al-load-data al-package-history-file t)))
;; 		al-package-history-file)
;;   (al-insert-ctrl-seq "usepackage" (file-name-sans-extension package))
;;   (al-wait-option))

;;; TODO: \usepackage{amsmath,amsthm,amssymb}
(defun al-insert-file (filename)
  "insert something"
  (al-save-data (delete-dups (cons filename (al-load-data al-files-history-path t)))
		al-files-history-path)
  (let* ((basename (file-name-sans-extension filename))
	 (ext (file-name-extension filename))
	 (insert-option-alist (assoc-default ext al-insert-ctrl-seq-alist))
	 (ctrl-seq (assoc-default 'ctrl-seq insert-option-alist))
	 (wait-option (assoc-default 'wait-option insert-option-alist)))
    (al-insert-ctrl-seq ctrl-seq basename)
    (when wait-option
      (al-wait-option))))

;; (defun al-insert-package-path (package-path)
;;   (al-insert-package (file-name-sans-extension (file-name-nondirectory package-path))))

(defun al-insert-cite (bibkey)
  "insert \\cite{label}"
  (interactive "sLabel: ")
  ;; (insert "\\cite{" bibkey "}")
  (al-insert-ctrl-seq "cite" bibkey)
  (al-wait-option))

(defun al-insert-environment-func (envname &optional not-increase-indent)
  (let ((indent-base (make-string (current-column) ?\ ))
	(indent-increase "  "))
    (when not-increase-indent
      (setq indent-increase ""))
    (insert (format "\\begin{%s}\n%s%s\n%s\\end{%s}" envname indent-base indent-increase indent-base envname))
    (forward-line -1)
    (end-of-line)
    ;; (re-search-backward "\\\\end{" nil t)
    ;; (backward-char)
    ))

(defun al-insert-environment (envname)
  (let (not-increase-indent)
    (when (member envname '("document"))
      (setq not-increase-indent t))
    (al-insert-environment-func envname not-increase-indent)))

;;; persistent-action
(defun al-show-persistent (string)
  (let ((al-help-buffer "*al-help*"))
    (get-buffer-create al-help-buffer)
    (switch-to-buffer al-help-buffer)
    (erase-buffer)
    (insert string)))

(defun al-show-persistent-label-func (label buffer)
  (with-current-buffer buffer
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

;;; compile commands
;;; This is dependent on auctex, latexmk, auctex-latexmk
(defun al-execute-command (command)
  (cond ((equal command "latexmk")
	 (TeX-save-document (TeX-master-file))
	 (TeX-command "LatexMk" 'TeX-master-file nil))
	((equal command "latexmk clean")
	 (shell-command "latexmk -c")
	 (message "latexmk -c"))
	((equal command "latexmk clean all")
	 (shell-command "latexmk -C")
	 (message "latexmk -C"))
	((equal command "view")
	 (TeX-command "View" 'TeX-master-file nil))
	))

;;; compile
;; (defvar al-compile-command
;;   "platex $BASENAME.tex; platex $BASENAME.tex; dvipdfmx $BASENAME.dvi"
;;   "Command for compilation in anything-latex
;; The TeX source file can be written as '$BASENAME.tex'.
;; Similarly, the DVI file as '$BASENAME.dvi'")

;; (defun al-compile (basename)
;;   (interactive "sInput TeX file name: ")
;;   (lexical-let ((al-compile-command-replaced (replace-regexp-in-string "$BASENAME" basename al-compile-command t)))
;;     (deferred:$
;;       (deferred:next
;; 	(lambda () (message "start compilation")))
;;       (deferred:process "platex" (concat basename ".tex") ;; al-compile-command-replaced
;; 	)
;;       (deferred:nextc it (lambda (out) (message "finished compilation") (message out)))
;;       (deferred:error it (lambda (err (message err)))))))

(provide 'anything-latex-functions)
