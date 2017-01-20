(require 'anything-latex-functions)

;;; anything
(require 'anything-config)

(defun anything-c-latex-get-labels ()
  (al-search-label anything-current-buffer))

(defun anything-c-latex-get-bibkeys ()
  (al-find-bibkeys))

(defun anything-c-latex-get-theorems ()
  al-theorem-list)

(defvar anything-c-source-latex-labels
  '((name . "Labels")
    ;; (init . al-label-init)
    (candidates . anything-c-latex-get-labels)
    (action ("Insert Default Ref" . al-insert-default-ref)
	    ("Jump" . al-jump-label)
	    ("Insert \\ref" . (lambda (label) (al-insert-ctrl-seq "ref" label)))
	    ("Insert \\cref" . (lambda (label) (al-insert-ctrl-seq "cref" label)))
	    ("Insert \\Cref" . (lambda (label) (al-insert-ctrl-seq "Cref" label)))
    	    )
    (persistent-action . al-show-persistent-label)
    ))

(defvar anything-c-source-latex-bibkeys
  '((name . "Bibkeys")
    ;; (init . al-bibkey-init)
    (candidates . anything-c-latex-get-bibkeys)
    (action ("Insert Cite" . al-insert-cite))
    (persistent-action . al-show-persistent-bib)))

(defvar anything-c-source-latex-theorems
  '((name . "Theorems")
    (candidates . anything-c-latex-get-theorems)
    (action ("Insert" . al-insert-theorem))))

;;; anything command
(defun anything-for-latex ()
  (interactive)
  (al-init (current-buffer))
  (anything-other-buffer '(anything-c-source-latex-theorems
			   anything-c-source-latex-bibkeys
			   anything-c-source-latex-labels
			   ;;anything-c-source-buffers
			   ;;anything-c-source-recentf
			   ;;anything-c-source-print-test
			   )
			 "*my anything*"))

(provide 'anything-latex)
