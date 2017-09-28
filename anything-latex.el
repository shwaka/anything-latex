(require 'anything-latex-functions)
(require 'anything-config)

(defun al-show-persistent-label (label)
  (al-show-persistent-label-func label anything-current-buffer))

(defun anything-c-latex-get-labels ()
  (al-search-label anything-current-buffer))

(defun anything-c-latex-get-bibkeys ()
  (al-find-bibkeys))

(defun anything-c-latex-get-theorems ()
  al-theorem-list)

(defvar anything-c-source-latex-labels
  '((name . "Labels")
    (candidates . anything-c-latex-get-labels)
    (action ("Insert Default Ref" . al-insert-default-ref)
	    ("Jump" . al-jump-label)
	    ("Insert \\ref" . (lambda (label) (al-insert-ctrl-seq "ref" label)))
	    ("Insert \\cref" . (lambda (label) (al-insert-ctrl-seq "cref" label)))
	    ("Insert \\Cref" . (lambda (label) (al-insert-ctrl-seq "Cref" label))))
    (persistent-action . al-show-persistent-label)))

(defvar anything-c-source-latex-bibkeys
  '((name . "Bibkeys")
    (candidates . anything-c-latex-get-bibkeys)
    (action ("Insert Cite" . al-insert-cite))
    (persistent-action . al-show-persistent-bib)))

(defvar anything-c-source-latex-theorems
  '((name . "Theorems")
    (candidates . anything-c-latex-get-theorems)
    (action ("Insert" . al-insert-environment))))

(defvar anything-c-source-latex-environments
  '((name . "Environments")
    (candidates . al-environment-list)
    (action ("Insert" . al-insert-environment))))

;; (defvar anything-c-source-latex-files-candidates-list
;;   '((name . "LaTeX Files")
;;     (candidates . al-texmf-files-list)
;;     (action ("Message" . message))))

;;; TODO: not only \usepackage
;;; too many candidates to choose (history? remove path?)
(defvar anything-c-source-latex-files
  '((name . "LaTeX Files (candidates in buffer)")
    (init . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
			 ;; (shell-command al-shell-command-list-files t)
			 (insert (al-get-files-list-string))
			 )))
    (candidates-in-buffer)
    (action . al-insert-file)))

(defvar anything-c-source-latex-commands
  '((name . "LaTeX Commands")
    (candidates . ("latexmk" "view" "latexmk clean" "latexmk clean all"))
    (action ("execute" . al-execute-command))))

;;; anything command
(defun anything-for-latex ()
  (interactive)
  (al-init (current-buffer))
  (anything-other-buffer '(anything-c-source-latex-commands
			   ;; anything-c-source-latex-files
			   anything-c-source-latex-environments
			   anything-c-source-latex-theorems
			   anything-c-source-latex-bibkeys
			   anything-c-source-latex-labels
			   anything-c-source-latex-files
			   )
			 "*anything for latex*"))

(provide 'anything-latex)
