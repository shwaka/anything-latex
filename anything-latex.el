(require 'anything-latex-functions)
(require 'anything-config)

(defun al-show-persistent-label (label)
  (al-show-persistent-label-func label anything-current-buffer))

(defun anything-c-my-latex-get-labels ()
  (al-search-label anything-current-buffer))

(defun anything-c-my-latex-get-bibkeys ()
  (al-find-bibkeys))

(defun anything-c-my-latex-get-theorems ()
  al-theorem-list)

(defvar anything-c-source-my-latex-labels
  '((name . "Labels")
    (candidates . anything-c-my-latex-get-labels)
    (action ("Insert Default Ref" . al-insert-default-ref)
	    ("Jump" . al-jump-label)
	    ("Insert \\ref" . (lambda (label) (al-insert-ctrl-seq "ref" label)))
	    ("Insert \\cref" . (lambda (label) (al-insert-ctrl-seq "cref" label)))
	    ("Insert \\Cref" . (lambda (label) (al-insert-ctrl-seq "Cref" label)))
    	    )
    (persistent-action . al-show-persistent-label)
    ))

(defvar anything-c-source-my-latex-bibkeys
  '((name . "Bibkeys")
    (candidates . anything-c-my-latex-get-bibkeys)
    (action ("Insert Cite" . al-insert-cite))
    (persistent-action . al-show-persistent-bib)))

(defvar anything-c-source-my-latex-theorems
  '((name . "Theorems")
    (candidates . anything-c-my-latex-get-theorems)
    (action ("Insert" . al-insert-theorem))))

;;; anything command
(defun anything-for-latex ()
  (interactive)
  (al-init (current-buffer))
  (anything-other-buffer '(anything-c-source-my-latex-theorems
			   anything-c-source-my-latex-bibkeys
			   anything-c-source-my-latex-labels
			   ;;anything-c-source-buffers
			   ;;anything-c-source-recentf
			   ;;anything-c-source-print-test
			   )
			 "*anything for latex*"))

(provide 'anything-latex)
