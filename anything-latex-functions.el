;;; This elisp file is independent of anything.el.
;;; Hence you can use this not only in anything.el but also in another interface.

(require 'em-glob)
(require 'seq)

;;; variables
(defvar al-default-theorem-list
  '("definition" "theorem" "proposition" "lemma" "corollary" "example" "remark")
  "theorem list")

;; (defvar al-default-environment-list
;;   '("document" "itemize" "enumerate" "math" "equation*" "equation" "eqnarray*" "eqnarray" "frame" "cases" "cases*" "array" "proof" "abstract" "quote" "quotation" "minipage" "center" "flushright" "flushleft" "block" "exampleblock" "alertblock" "picture" "figure" "align" "align*" "lstlisting" "table" "tabular" "extable"))
;;                                         ;extable: temporary added

(defvar al-additional-theorem-list
  '()
  "theorem list added by a user")

(defvar al-additional-environment-list
  '()
  "environment list added by a user")

(defvar al-texmf-dirs
  (split-string
   (shell-command-to-string "kpsewhich -expand-path='$TEXMF' | LANG=C perl -pe \"s/\\n//\"")
   ":" t)
  "list of texmf directories")

(defvar al-indent-string
  "  "
  "string used to indent")

(defvar al-popular-files-path
  (locate-file "anything-latex-popular-files" load-path)
  "file containing a list of frequently used latex files")
(defvar al-popular-environments-path
  (locate-file "anything-latex-popular-environments" load-path)
  "file containing a list of frequently used latex environments")

;; (defvar al-package-history-file
;;   "~/.emacs.d/anything-latex-package-history"
;;   "file to save history of \\usepackage")
(defvar al-files-history-path
  "~/.emacs.d/anything-latex-files-history"
  "file to save history of latex files")

(defvar al-environments-history-path
  "~/.emacs.d/anything-latex-environments-history"
  "file to save history of latex files")

(defvar al-shell-command-list-files
  ;; "find $(kpsewhich -expand-path='$TEXMF' | sed -e \"s/:/ /g\")"
  (format "find %s \\( -name fonts -prune \\) -or \\( -xtype f -printf \"%%f\n\" \\)"
	  (apply 'concat (mapcar
			  #'(lambda (s) (concat s " "))
			  al-texmf-dirs)))
  "command to list files in texmf directories")

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

(defun al-add-data (data-elem filename)
  (al-save-data (delete-dups (cons data-elem (al-load-data filename t)))
                filename))

;;; TODO: current directory, beamer
(defun al-get-files-list-string ()
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

(defun al-get-environment-list ()
  (delete-dups
   (append
    ;; (al-load-data al-environments-history-path t)
    ;; (apply 'concat (mapcar
    ;;                 #'(lambda (s) (concat s "\n"))
    ;;                 al-default-environment-list))
    ;; al-default-environment-list
    (with-temp-buffer
      (insert-file-contents al-popular-environments-path)
      (split-string (buffer-string) "\n" t))
    )))


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
  (let (;; (theorem-pattern "\\\\newtheorem{\\([a-zA-Z]*\\)}\\(?:\[[a-zA-Z]*\]\\)?{\\([a-zA-Z]*\\)}")
	;; (theorem-pattern "\\\\newtheorem\\*?{\\([a-zA-Z]*\\)}\\(?:\[[a-zA-Z]*\]\\)?{\\([^}\n]*\\)}")
        (theorem-pattern (rx "\\newtheorem"
                             (opt "*")
                             "{"
                             (group (0+ (any alpha)))
                             "}"
                             (opt (seq "["
                                       (0+ alpha)
                                       "]"))
                             "{"
                             (group (0+ (not (any "}\n"))))
                             "}"))
	(theorem-list ()))
    (with-current-buffer buffer
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward theorem-pattern nil t)
	  (add-to-list 'theorem-list (match-string 1)))
	(reverse theorem-list)))))

(defun al-search-environment (buffer)
  (let ((environment-pattern "\\\\\\(re\\)?newenvironment{\\([a-zA-Z]*\\)}")
	(environment-list ()))
    (with-current-buffer buffer
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward environment-pattern nil t)
	  (add-to-list 'environment-list (match-string 2)))
	(reverse environment-list)))))

(defun al-use-cleveref-p (buffer)
  (let (;; (cleveref-pattern "^[^%\n]*\\\\usepackage\\(?:\\[[a-zA-Z, ]*\\]\\)?{cleveref}")
        ;; (cleveref-pattern (rx line-start
        ;;                       (0+ (not (any "%\n")))
        ;;                       "\\usepackage"
        ;;                       (opt (seq "["
        ;;                                 (0+ (any alpha ", "))
        ;;                                 "]"))
        ;;                       "{cleveref}"))
        (cleveref-pattern (rx line-start
                              (0+ (not (any "%\n")))
                              "\\usepackage"
                              (opt (seq "["
                                        (0+ (any alphanumeric ", "))
                                        "]"))
                              "{"
                              (0+ space)
                              (0+ (seq (1+ (any alphanumeric "-"))  ; alpha? alphanumeric?
                                       (0+ space)
                                       ","
                                       (0+ space)))
                              "cleveref"
                              (0+ space)
                              (0+ (seq ","
                                       (0+ space)
                                       (1+ (any alphanumeric "-"))
                                       (0+ space)))
                              "}")) ; \usepackage{amsthm,cleveref,tikz} でもok
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
  (setq al-theorem-list (append al-user-theorem-list al-additional-theorem-list al-default-theorem-list)))

(defun al-environment-init (buffer)
  (setq al-user-environment-list (al-search-environment buffer))
  (setq al-environment-list (delete-dups (append (al-load-data al-environments-history-path t)
                                                 al-user-environment-list al-additional-environment-list
                                                 (al-get-environment-list)))))

(defun al-bibkey-init (buffer)
  (setq al-bib-file (al-find-bib-file buffer)))

(defun al-init (buffer)
  (al-label-init buffer)
  (al-bibkey-init buffer)
  (al-set-config buffer)
  (al-environment-init buffer)
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

(defvar al-file-type-list--beamer
  (mapcar (lambda (themetype)
            (list :regexp (rx-to-string `(seq (eval (format "beamer%stheme" ,themetype))
                                              (group (1+ any)) ".sty" string-end)
                                        t)
                  :ctrl-seq (format "use%stheme" themetype)))
          '("" "color" "font" "inner" "outer")))

(defvar al-file-type-list
  (append al-file-type-list--beamer
          `((:regexp ,(rx (group (1+ any)) ".cls" string-end)
             :ctrl-seq "documentclass"
             :wait-option t)
            (:regexp ,(rx (group (1+ any)) ".sty" string-end)
             :ctrl-seq "usepackage"
             :wait-option t)
            (:regexp ,(rx (group (1+ any)) ".bib" string-end)
             :ctrl-seq "bibliography")
            (:regexp ,(rx (group (1+ any)) ".bst" string-end)
             :ctrl-seq "bibliographystyle")
            (:regexp ,(rx "tikzlibrary" (group (1+ any)) ".code.tex" string-end)
             :ctrl-seq "usetikzlibrary")
            (:regexp ,(rx "pgflibrary" (group (1+ any)) ".code.tex" string-end)
             :ctrl-seq "usetikzlibrary"))))

(defun al-get-file-info--for-file-type (filename file-type)
  (let ((regexp (plist-get file-type :regexp)))
    (when (string-match regexp filename)
      (list :name (match-string 1 filename)
            :ctrl-seq (plist-get file-type :ctrl-seq)
            :wait-option (plist-get file-type :wait-option)))))
(defun al-get-file-info (filename)
  (seq-some (lambda (file-type)
              (al-get-file-info--for-file-type filename file-type))
            al-file-type-list))

;;; TODO: \usepackage{amsmath,amsthm,amssymb}
(defun al-insert-file (filename)
  "insert something"
  ;; (al-save-data (delete-dups (cons filename (al-load-data al-files-history-path t)))
  ;;       	al-files-history-path)
  (al-add-data filename al-files-history-path)
  (let ((file-info (al-get-file-info filename)))
    (if file-info
        (let ((name (plist-get file-info :name))
              (ctrl-seq (plist-get file-info :ctrl-seq))
              (wait-option (plist-get file-info :wait-option)))
          (al-insert-ctrl-seq ctrl-seq name)
          (when wait-option
            (al-wait-option)))
      (message "Invalid filename: %s. Configure it in al-file-type-list" filename))))

;; (defun al-insert-package-path (package-path)
;;   (al-insert-package (file-name-sans-extension (file-name-nondirectory package-path))))

(defun al-insert-cite (bibkey)
  "insert \\cite{label}"
  (interactive "sLabel: ")
  ;; (insert "\\cite{" bibkey "}")
  (al-insert-ctrl-seq "cite" bibkey)
  (al-wait-option))

(defun al-insert-environment-func (envname &optional indent-increase itemize-like)
  (let ((indent-base (make-string (current-column) ?\ ))
	;; (indent-increase "  ")
	(default-text ""))
    ;; (when not-increase-indent
    ;;   (setq indent-increase ""))
    (cond ((stringp indent-increase) t)
	  ((eq indent-increase t) (setq indent-increase al-indent-string))
	  ((not indent-increase) (setq indent-increase "")))
    (when itemize-like
      (setq default-text "\\item "))
    (insert (format "\\begin{%s}\n%s%s%s\n%s\\end{%s}" envname indent-base indent-increase default-text indent-base envname))
    (forward-line -1)
    (end-of-line)
    ;; (re-search-backward "\\\\end{" nil t)
    ;; (backward-char)
    ))

(defun al-insert-environment (envname save-history)
  (when save-history
    (al-add-data envname al-environments-history-path))
  (let ((indent-increase t)
	(itemize-like nil))
    (when (member envname '("document"))
      (setq indent-increase nil))
    (when (member envname '("itemize" "enumerate"))
      (setq indent-increase t)
      (setq itemize-like t))
    (al-insert-environment-func envname indent-increase itemize-like)))

(defun al-insert-environment-save (envname)
  (al-insert-environment envname t))

(defun al-insert-environment-notsave (envname)
  (al-insert-environment envname nil))

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


(defvar al-after-latexmk-command
  nil
  "Command run after latexmk.
Evaluated in the following way:
(let ((al-master-file (TeX-master-file)))
  (call-process-shell-command (eval al-after-latexmk-command)
                              nil 0))
")


;;; compile commands
;;; This is dependent on auctex, latexmk, auctex-latexmk
(defun al-execute-command (command)
  (cond ((equal command "latexmk")
         (if (not (equal "tex" (file-name-extension (buffer-file-name))))
             (message (concat (propertize "Error:" 'face '(:foreground "orange"))
                              " NOT in .tex file"))
           (let ((al-master-file (TeX-master-file)))
                                        ; `TeX-master-file'を `TeX-command'の後に実行すると<none>になる
             (TeX-save-document (TeX-master-file))
             (TeX-command "LatexMk" 'TeX-master-file nil)
             ;; (call-process-shell-command (format "latexmk-dropbox -notypeset %s.tex &" al-master-file)
             ;;                             nil 0)
             (call-process-shell-command (eval al-after-latexmk-command)
                                         nil 0))))
	((equal command "latexmk clean")
	 (shell-command "latexmk -c")
	 (message "latexmk -c"))
	((equal command "latexmk clean all")
	 (shell-command "latexmk -C")
	 (message "latexmk -C"))
	((equal command "view")
	 ;; (TeX-command "View" 'TeX-master-file nil)
         ;; https://stackoverflow.com/questions/13901955/how-to-avoid-pop-up-of-async-shell-command-buffer-in-emacs
         (let* ((filename (concat (TeX-master-file) ".pdf"))
                (escaped-filename (shell-quote-argument filename)))
           (call-process-shell-command (concat "evince " escaped-filename) nil 0)))
        ((equal command "forward search (synctex)")
         (TeX-command "Fwdevince" 'TeX-master-file))))

(defun al-execute-command-1 (command)
  (cond ((equal command "latexmk/view")
         (al-execute-command "latexmk"))
        (t (al-execute-command command))))

(defun al-execute-command-2 (command)
  (cond ((equal command "latexmk/view")
         (al-execute-command "view"))
        (t (al-execute-command command))))

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
