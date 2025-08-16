;;; -*- lexical-binding: t; -*-
(require 'cl-lib)

;; 既存のヘルパ（例）
(defun al-subfiles--ensure-tex (name)
  (if (string-match-p "\\.tex\\'" name) name (concat name ".tex")))

(defun al-subfiles--ensure-key (name)
  "必ず \"./\" で始まり、.tex を付けたキー文字列に。"
  (let ((fname (al-subfiles--ensure-tex name)))
    (unless (string-prefix-p "./" fname)
      (setq fname (concat "./" fname)))
    fname))

(defun al-subfiles--resolve-abs (base-buf name)
  (let* ((fname (al-subfiles--ensure-tex name))
         (base-dir (with-current-buffer base-buf
                     (or (and buffer-file-name (file-name-directory buffer-file-name))
                         default-directory))))
    (expand-file-name fname base-dir)))

;;;; ─────────────────────────────────────────────────────────────
;;;; Temp buffer cache (abs path -> hidden, non-visiting buffer)
;;;; ─────────────────────────────────────────────────────────────

(defvar al-subfiles--temp-cache (make-hash-table :test 'equal)
  "絶対パス文字列 -> バッファ のキャッシュ。")

(defun al-subfiles--temp-buffer-on-kill ()
  "一時バッファが kill されたときにキャッシュから掃除する。"
  (when (bound-and-true-p al-subfiles--temp-buffer-abs)
    (remhash al-subfiles--temp-buffer-abs al-subfiles--temp-cache)))

(defun al-subfiles--temp-buffer-for (abs &optional refresh)
  "ABS ファイルの内容を持つ“隠し一時バッファ”を返す。REFRESH なら内容を読み直す。
同じ ABS については既存バッファを再利用する。"
  (let ((buf (gethash abs al-subfiles--temp-cache)))
    (cond
     ;; 既存をそのまま再利用
     ((and (buffer-live-p buf) (not refresh))
      buf)
     ;; 無い or refresh 要求 → 内容を読み直して（または再作成して）返す
     (t
      (let* ((bn (concat " " (file-name-nondirectory abs))) ; 先頭スペースで“隠し名”
             (reuse (and (buffer-live-p buf) buf))
             (new   (or reuse (generate-new-buffer (generate-new-buffer-name bn)))))
        (with-current-buffer new
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert-file-contents abs)) ; ファイル“訪問”はしない
          (setq buffer-file-name nil)
          (set-buffer-modified-p nil)
          ;; キャッシュ掃除用に ABS を覚え、kill 時に消す
          (setq-local al-subfiles--temp-buffer-abs abs)
          (add-hook 'kill-buffer-hook #'al-subfiles--temp-buffer-on-kill nil t))
        (puthash abs new al-subfiles--temp-cache)
        new)))))

(defun al-subfiles--visit-child-as-temp (base-buf name &optional refresh)
  "BASE-BUF を基準に NAME を解決し、隠し一時バッファを返す（再利用）。"
  (let ((abs (al-subfiles--resolve-abs base-buf name)))
    (when (file-exists-p abs)
      (al-subfiles--temp-buffer-for abs refresh))))

;;;; ─────────────────────────────────────────────────────────────
;;;; alist への蓄積側（キーは文字列 \"./foo.tex\"）
;;;; ─────────────────────────────────────────────────────────────

(defun al-subfiles--accumulate (alist name child-buf)
  (let ((k (al-subfiles--ensure-key name)))
    (if (assoc-string k alist t)
        (setcdr (assoc-string k alist t) child-buf)
      (push (cons k child-buf) alist))
    alist))

;;;; ─────────────────────────────────────────────────────────────
;;;; 例：コア探索（必要部分だけ抜粋）
;;;; ─────────────────────────────────────────────────────────────

(defconst al-subfiles--regex
  (rx
   (or
    (seq
     "\\" (or "subfile" "subfileinclude" "input" "include")
     (* space)
     "{" (group-n 1 (+ (not (any "}")))) "}")
    (seq
     "\\documentclass"
     (* space)
     "[" (group-n 1 (+ (not (any "]")))) "]"
     "{subfiles}"))))

(defun al-subfiles--scan-buffer-for-names (buf)
  (let (;; (regex "\\\\subfile{\\([^}]+\\)}")
        (names '()))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward al-subfiles--regex nil t)
            (push (match-string-no-properties 1) names)))))
    (nreverse names)))

(defun al-subfiles-collect-into-alist (start-buffer &optional result-alist)
  "START-BUFFER から \\subfile{...} を再帰探索。キーは文字列 \"./foo.tex\"、値は隠し一時バッファ。"
  (let* ((visited (make-hash-table :test 'equal)) ; 絶対パス -> t
         (queue (list start-buffer))
         (res (or result-alist nil)))
    (let ((abs0 (with-current-buffer start-buffer
                  (when buffer-file-name (expand-file-name buffer-file-name)))))
      (when abs0 (puthash abs0 t visited)))
    (while queue
      (let ((buf (pop queue)))
        (when (buffer-live-p buf)
          (dolist (name (al-subfiles--scan-buffer-for-names buf))
            (let ((abs (al-subfiles--resolve-abs buf name)))
              (unless (gethash abs visited)
                (puthash abs t visited)
                (when (file-exists-p abs)
                  (let ((child (al-subfiles--visit-child-as-temp buf name))) ; ← 再利用される
                    (when child
                      (setq res (al-subfiles--accumulate res name child))
                      (push child queue))))))))))
    res))

(provide 'anything-latex-subfiles)
