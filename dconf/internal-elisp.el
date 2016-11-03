;;---------------------------------------------------------------------------
;;---------------------------------------------------------------------------
;;
;; 自作el(package化しない) / internal-elisp.el
;;
;;---------------------------------------------------------------------------
;;---------------------------------------------------------------------------

;;---------------------------------------------------------------------------
;; google with chrome / chromeを開いて検索
;;---------------------------------------------------------------------------
;; key-insert : google-with-chrome

(defvar windows-chrome
  "C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe")

(defvar linux-chrome
  "google-chrome")

(defun modify-blank (word)
  (let ((tokens (mapcar (lambda (x) (concat "+" x)) (split-string word))))
    (substring (apply 'concat tokens) 1)))

(defun google-with-chrome-open (word)
  (let ((new-word (modify-blank word)))
    (cond
     ((eq 'gnu/linux system-type)
      (let ((command (concat linux-chrome " google.com/#q=" new-word)))
        (message (shell-command-to-string command))))
     ((eq 'windows-nt system-type)
      (let ((command
             (concat "\"" windows-chrome "\"" " google.com/#q=" new-word)))
        (message (shell-command-to-string command))))
     'else
     (message "unknown os error: unable to search"))))

(defun search-input-with-google ()
  (interactive)
  (let ((word (read-from-minibuffer "search with google : ")))
    (google-with-chrome-open word)))

(defun search-region-with-google ()
  (interactive)
  (google-with-chrome-open (buffer-substring (mark) (point))))

;;---------------------------------------------------------------------------
;; insert date / 日付の挿入
;;---------------------------------------------------------------------------
;; key-bind : insert-time key

(defun insert-time ()
  (interactive)
  (insert (format-time-string "%Y/%m/%d %H:%M:%S")))

;;---------------------------------------------------------------------------
;; fixed-scrolls / カーソル位置固定移動
;;---------------------------------------------------------------------------
;; key-bind : scrolls keys
;; (M-p,M-nで)カーソル位置固定のまま、スクロール

(defun scroll-up-in-place (n)
  (interactive "p")
  (previous-line n)
  (scroll-down n))

(defun scroll-down-in-place (n)
  (interactive "p")
  (next-line n)
  (scroll-up n))

;;---------------------------------------------------------------------------
;; parenthesis control / 括弧操作
;;---------------------------------------------------------------------------
;; crudのうちcdのみ

;;---------------------------------------------------------------------------
;; 括弧 挿入

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

;;---------------------------------------------------------------------------
;; 括弧 削除

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

;;---------------------------------------------------------------------------
;; smart buffer move
;;---------------------------------------------------------------------------
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
      (string= "* Racket REPL *" buffer-name)
      (string= "*scheme*" buffer-name)
      (string= "*Python*" buffer-name)
      (string= "*scratch*" buffer-name)))

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

;;---------------------------------------------------------------------------
;; white-plus
;;---------------------------------------------------------------------------
;; bind-key : white plus keys
(defun insert-spaces (n)
  (interactive)
  (dotimes (i n) (insert " ")))

;;---------------------------------------------------------------------------
;; provide
;;---------------------------------------------------------------------------
(provide 'internal-elisp)
