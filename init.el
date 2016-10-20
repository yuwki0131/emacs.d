;; ---------------------------------------------------------------------------
;; パッケージマネージャ
(require 'package)

(add-to-list
 'package-archives
 '("melpa"          . "https://melpa.milkbox.net/packages/") t)

(package-initialize)

;; ---------------------------------------------------------------------------
;; 諸設定
(setq inhibit-startup-message t) ;; 起動時の画面は、いらない
(tool-bar-mode -1)               ;; ツールバーは、いらない
(menu-bar-mode -1)               ;; メニューバーは、いらない

;; beep音消す
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; backupfileつくらない
(setq make-backup-files nil)
(setq auto-save-default nil)

;; 折り返しを表示
(setq truncate-lines t)

;; 行番号を表示
(global-linum-mode t)

;; 行番号フォーマット
(setq linum-format " %4d ")

;; file名の補完で大文字小文字を区別しない
(setq completion-ignore-case t)

;; バッファ自動再読み込み
(global-auto-revert-mode 1)

;; カーソルタイプ
(setq default-cursor-type '(bar . 2))

;; エンコーディング
(set-language-environment "Japanese")
(setq default-buffer-file-coding-system 'utf-8)

;; フォント
(set-default-font "Ubuntu Mono-8")

;;---------------------------------------------------------------------------
;; 行末のwhitespaceを削除
(setq delete-trailing-whitespace-exclude-patterns
      (list "\\.md$" "\\.markdown$"))
(require 'cl)
(defun delete-trailing-whitespace-with-exclude-pattern ()
  (interactive)
  (cond
   ((equal
     nil
     (loop for pattern in delete-trailing-whitespace-exclude-patterns
           thereis (string-match pattern buffer-file-name)))
    (delete-trailing-whitespace))))
(add-hook 'before-save-hook
          'delete-trailing-whitespace-with-exclude-pattern)

;;---------------------------------------------------------------------------
;; ファイル末尾の改行を削除


;;---------------------------------------------------------------------------
;; ファイル末尾の改行を削除
;; http://www.emacswiki.org/emacs/DeletingWhitespace
(defun my-delete-trailing-blank-lines ()
  "Deletes all blank lines at the end of the file."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (delete-blank-lines))))

(add-hook 'before-save-hook 'my-delete-trailing-blank-lines)

;; ---------------------------------------------------------------------------
;; 普通のredo

(require 'redo+)
 (setq undo-no-redo t)
 (setq undo-limit 60000)
 (setq undo-strong-limit 90000)

;; ---------------------------------------------------------------------------
;; アスタリスク付バッファは飛ばす

(defun astarisked? (buf-name)
  (= 42 (car (string-to-list buf-name))))

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

(global-unset-key "\C-e")

(global-unset-key "\C-a")

(global-unset-key "\C-z") ;; (prev ctrl-zでwindowが最小化) しない

;;---------------------------------------------------------------------------
;; prefix なし
(global-set-key (kbd "C-S-k") 'backward-kill-line)

(global-set-key "\C-h"   'delete-backward-char)
(global-set-key "\M-h"   'backward-kill-word)

(global-set-key "\C-q"   'undo)
(global-set-key "\M-q"   'redo)

;; カーソル位置固定のままスクロール
(global-set-key "\M-p"   'scroll-up-in-place)
(global-set-key "\M-n"   'scroll-down-in-place)

;; 置換
(global-set-key "\C-z\C-r" 'replace-string)

;; コメントアウト
(global-set-key "\C-a\C-a" 'comment-dwim)

;; Shell
(global-set-key "\C-e\C-c" 'shell)

;; バッファ移動 (アスタリスク付バッファはスキップ)
(global-set-key "\C-e\C-b" 'previous-buffer-with-skip*)
(global-set-key "\C-e\C-f" 'next-buffer-with-skip*)

;; parenthesis advisary
(global-set-key [C-return]    'kill-until-corresp-paren)
(global-set-key "\C-l"        'insert-parenthesis)
(global-set-key (kbd "C-S-l") 'insert-angle-brackets)
(global-set-key "\M-l"        'insert-brackets)
(global-set-key (kbd "M-L")   'insert-squares)
