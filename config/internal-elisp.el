;;; package --- internal-elisp.el
;;; Commentary:
;;;  自作el(package化しない雑多な機能群)
;;; Code:

;;; ---------------------------------------------------------------------------
;;; grep this & grep find here / grepを & grep find 現在のbuffer/pathで実行
;;; ---------------------------------------------------------------------------
;; key-bind : grep-this, grep-find-this
(defun grep-this ()
  (interactive)
  (let ((word (read-from-minibuffer "grep this buffer : "))
	(file-name (buffer-file-name (current-buffer))))
    (if (not file-name)
	(message "unknown file")
      (grep (format "grep --color -nH -e \"%s\" %s" word file-name)))))

(defun grep-find-this ()
  (interactive)
  (let ((word (read-from-minibuffer "grep find . : ")))
    (grep-find
     (format "find . -type f -exec grep --color -nH -e %s {} +" word))))

;;; ---------------------------------------------------------------------------
;;; merge 2 lines / merge current line & next line
;;; ---------------------------------------------------------------------------
;; key-bind : merge2lines
(defun merge2lines ()
  (interactive)
  (save-excursion
    (delete-region
     (point)
     (progn (re-search-forward "^[ \t\n]*" nil t) (point))))
  (insert " "))

;;; ---------------------------------------------------------------------------
;;; kill the other buffers / 現在のbuffer以外のbufferをすべて閉じる
;;; ---------------------------------------------------------------------------
;; key-bind : kill-the-other-buffers
(defun kill-the-other-buffers ()
  (interactive)
  (dolist (buffer (buffer-list))
    (when (not (eq (current-buffer) buffer))
      (kill-buffer buffer))))

;;; ---------------------------------------------------------------------------
;;; TODO search / TODOコメント 管理
;;; ---------------------------------------------------------------------------
;; key-bind : goto-next-TODO
;; TODO : 未実装(書きかけ) listup-TODO
(defconst TODO-symbol "TODO")

(defun goto-next-TODO ()
  (interactive)
  (forward-line)
  (let ((result (search-forward TODO-symbol nil t)))
    (message result)
    (if (not result)
      (progn (beginning-of-buffer)
	     (search-forward TODO-symbol nil t)))))

;;; ---------------------------------------------------------------------------
;;; LWRE search & replace / 簡易正規表現検索
;;; ---------------------------------------------------------------------------
;; TODO : 未実装

;;; ---------------------------------------------------------------------------
;;; insert date / 日付の挿入
;;; ---------------------------------------------------------------------------
;; key-bind : insert-date key
(defun insert-date-normal ()
  (interactive)
  (insert (format-time-string "%Y/%m/%d %H:%M:%S")))

(defun insert-date-markdown ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M")))

;;; ---------------------------------------------------------------------------
;;; fixed-scrolls / カーソル位置固定移動
;;; ---------------------------------------------------------------------------
;; key-bind : scrolls keys
;; (M-p,M-nで)カーソル位置固定のまま、スクロール
(defun scroll-up-in-place (n)
  (interactive "p")
  (previous-line n)
  (scroll-down n))

(defun scroll-down-in-place (n)
  (interactive "p")
  (forward-line n)
  (scroll-up n))

;;; ---------------------------------------------------------------------------
;;; parenthesis control / 括弧操作
;;; ---------------------------------------------------------------------------
;; packageがあったはずだが自分のコードの方が使い慣れてしまったので、こっちを使う

;;; ---------------------------------------------------------------------------
;;; 括弧 挿入
(defun insert-squares ()
  (interactive)
  (insert "[]")
  (goto-char (- (point) 1)))

(defun insert-brackets ()
  (interactive)
  (insert "{}")
  (goto-char (- (point) 1)))

(defun insert-angle-brackets ()
  (interactive)
  (insert "<>")
  (goto-char (- (point) 1)))

(defun insert-parenthesis ()
  (interactive)
  (insert "()")
  (goto-char (- (point) 1)))

;;; ---------------------------------------------------------------------------
;;; 括弧 削除
;; 対応する括弧までを削除(後方only)
(defun delete-backward-corresp-paren ()
  (interactive)
  (delete-char (- (scan-lists (point) 1 0) (point))))

;; 対応する括弧までを削除
(defun kill-until-corresp-paren ()
  (interactive)
  (save-excursion
    (let* ((current-point (point))
           (current-char  (following-char))
           (last-char     (progn (backward-char) (following-char)))
           (vec           (cond ((= ?\) last-char) -1)
                                ((= ?\( current-char) 1)
                                (t nil))))
      (if vec
          (kill-region current-point (scan-lists current-point vec 0))))))

;; 対応する括弧を削除
(defun match-delete-parenthesis (beginp endp)
  (delete-char 1)
  (forward-char 1)
  (let ((counter 1))
    (while (> counter 0)
      (cond
       ((= beginp (following-char)) (setq counter (+ counter 1)))
       ((= endp   (following-char)) (setq counter (- counter 1)))
       ((= -1 (following-char)) (setq counter -1)))
      (forward-char 1))
    (if (not (= counter -1))
	(progn
	  (forward-char -1)
	  (delete-char 1)
	  ))))

(defun delete-parenthesis ()
  (interactive)
  (save-excursion
    (forward-char -1)
    (cond
     ((= ?\( (following-char)) (match-delete-parenthesis ?\( ?\)))
     ((= ?\{ (following-char)) (match-delete-parenthesis ?\{ ?\}))
     ((= ?\[ (following-char)) (match-delete-parenthesis ?\[ ?\])))))

(defun most-wide-match-lexical-insert-parenthesis (beginp endp)
  (insert "(")
  (let (((counter 1))
    (while (> counter 0)
      (cond
       ((= beginp (following-char)) (setq counter (+ counter 1)))
       ((= endp   (following-char)) (setq counter (- counter 1)))
       ((= -2 (following-char)) (setq counter -2)))
      (forward-char 1))
    (progn
      (forward-char -1)
      (insert ")")))))

(defun most-wide-lexical-insert-parenthesis ()
 (interactive)
 (save-excursion
   (cond
    ((= ?\( (following-char))
     (most-wide-match-lexical-insert-parenthesis ?\( ?\))))))

(defun most-narrow-match-lexical-insert-parenthesis (beginp endp)
  (insert "(")
  (let ((counter 0) (found-most-narrow-parenthesis nil))
    (while (and (or (not found-most-narrow-parenthesis)
		    (> counter 0))
		(not (= counter -2)))
      (cond
       ((= beginp (following-char))
	(progn
	  (setq counter (+ counter 1))
	  (setq found-most-narrow-parenthesis t)))
       ((= endp   (following-char))
	(setq counter (- counter 1)))
       ((= -2 (following-char))
	(setq counter -2))))
    (prognp
      (forward-char -1)
      (insert ")"))))

(defun most-narrow-lexical-insert-parenthesis ()
  (interactive)
  (save-excursion
    (cond
     ((= ?\( (following-char))
      (most-narrow-match-lexical-insert-parenthesis ?\( ?\))))))

;;; ---------------------------------------------------------------------------
;;; smart buffer move
;;; ---------------------------------------------------------------------------
;; key-bind : smart-buf-moves keys
;; *scratch*, *message* など ** 付きbufferを削除
(defun astarisked? (buf-name)
  (= 42 (car (string-to-list buf-name))))

(defun move-to-scratch ()
  (interactive)
  (let ((current-buffer-name (buffer-name)))
    (next-buffer)
    (while (and (not (string= "*scratch*" (buffer-name)))
                (not (string= current-buffer-name (buffer-name))))
      (next-buffer))))

(defun match-repl-pattern? (buffer-name)
  (or (string= "*haskell*" buffer-name)
      (string= "*cider-repl localhost*" buffer-name)
      (string= "*Racket REPL*" buffer-name)
      (string= "*scheme*" buffer-name)
      (string= "*ielm*" buffer-name)
      (string= "*inferior-lisp*" buffer-name)
      (string= "*Python*" buffer-name)
      (string= "*Python3*" buffer-name)
      (string= "*scratch*" buffer-name)
      (string= "*eshell*" buffer-name)))

(defun move-to-repl ()
  (interactive)
  (let ((current-buffer-name (buffer-name)))
    (next-buffer)
    (while (and (not (match-repl-pattern? (buffer-name)))
                (not (string= current-buffer-name (buffer-name))))
      (next-buffer))))

(defun next-buffer-with-skip* ()
  (interactive)
  (let ((current-buffer-name (buffer-name)))
    (next-buffer)
    (while (and (astarisked? (buffer-name))
                (not (string= current-buffer-name (buffer-name))))
      (next-buffer))))

(defun previous-buffer-with-skip* ()
  (interactive)
  (let ((current-buffer-name (buffer-name)))
    (previous-buffer)
    (while (and (astarisked? (buffer-name))
                (not (string= current-buffer-name (buffer-name))))
      (previous-buffer))))

;;; ---------------------------------------------------------------------------
;;; white-plus
;;; ---------------------------------------------------------------------------
;; bind-key : white plus keys

(defun insert-spaces (n)
  (interactive)
  (dotimes (i n) (insert " ")))

;;; ---------------------------------------------------------------------------
;;; intert-underscore
;;; ---------------------------------------------------------------------------
;; bind-key : insert underscore

(defun insert-underscore ()
  (interactive)
  (insert "_"))

;;; ---------------------------------------------------------------------------
;;; emoji-shufflin
;;; ---------------------------------------------------------------------------
;; TODO : ランダム絵文字

(defun emoji-shufflin ()
  (interactive)
  "🍣")

;;; ---------------------------------------------------------------------------
;;; make-buffer-small
;;; ---------------------------------------------------------------------------
;; bind-key : make-buffer-small
;; バッファの高さを調整

(defvar make-buffer-small-buffer-sizes
  (make-hash-table :test #'equal))

;; 設定値にはミニバッファとモードラインの高さ込
(puthash "leinrepl" 15 make-buffer-small-buffer-sizes)
(puthash "shell" 15 make-buffer-small-buffer-sizes)
(puthash "large" 30 make-buffer-small-buffer-sizes)
(puthash "small" 5 make-buffer-small-buffer-sizes)

(defun make-buffer-small ()
  (interactive)
  (let* ((type-name (read-from-minibuffer "your buffer type name : "))
	 (value-in-hash (gethash type-name make-buffer-small-buffer-sizes))
	 (target-size (if value-in-hash value-in-hash 10)))
    (when (< target-size (window-height))
      (shrink-window (- (window-height) target-size)))))

;;; ---------------------------------------------------------------------------
;;; provide
;;; ---------------------------------------------------------------------------
(provide 'internal-elisp)
;;; internal-elisp.el ends here
