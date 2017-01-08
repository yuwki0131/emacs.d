;;;; ---------------------------------------------------------------------------
;;;; ---------------------------------------------------------------------------
;;;;
;;;; キーバインド設定 / key-binding.el
;;;;
;;;; ---------------------------------------------------------------------------
;;;; ---------------------------------------------------------------------------

;; prefix key (control)
;; q w E r t y U _ o p
;;  A s d f g h j k l
;;   Z X C v b n m _ .
;; upper cases : control-prefix
;; lower cases : not control-prefix
;; underscore  : not binding

;; 移動系 prefix
(global-unset-key "\C-e")

;; 編集系 prefix
(global-unset-key "\C-a")

;; 機能系 prefix
(global-unset-key "\C-z")

;; prefix key (meta)
;; q w e r t y u i o p
;;  a S d f G h j k l
;;   z x c v b n m _ .
;; upper cases : meta-prefix
;; lower cases : not meta-prefix
;; underscore  : not binding

(global-unset-key "\M-m")

(global-unset-key "\M-j")

;;; ---------------------------------------------------------------------------
;;; no prefix
;;; ---------------------------------------------------------------------------

(global-set-key (kbd "C-S-k") 'backward-kill-line)

;; delete action
(global-set-key "\C-h"     'delete-backward-char)
(global-set-key "\M-h"     'backward-kill-word)

;; move by paragraph
(global-set-key "\C-m"     'forward-paragraph)
(global-set-key "\M-m"     'backward-paragraph)

;; next / previous buffer with skip *
(global-set-key "\C-\M-f"  'next-buffer-with-skip*)
(global-set-key "\C-\M-p"  'previous-buffer-with-skip*)

;; undo & redo
(global-set-key "\C-q"     'undo)
(global-set-key "\M-q"     'redo)

;; カーソル位置固定のままスクロール
(global-set-key "\M-p"     'scroll-up-in-place)
(global-set-key "\M-n"     'scroll-down-in-place)

;; ace jump mode
(global-set-key "\M-a"     'ace-jump-mode)

;; vr/isearch側の正規表現置換
(global-set-key "\C-\M-s"  'vr/isearch-forward)
(global-set-key "\C-\M-r"  'vr/isearch-backward)

;; TODOコメント管理
(global-set-key (kbd "C-.")  'goto-next-TODO)

;; アンダースコア挿入
(global-set-key (kbd "C-:") 'insert-underscore)

;; シンボル移動　(highlight-symbol-mode)
(global-set-key (kbd "C-?") 'highlight-symbol-next)
(global-set-key (kbd "C-!") 'highlight-symbol-prev)

;; 括弧操作
(global-set-key [C-return]      'kill-until-corresp-paren)
(global-set-key "\C-l"          'insert-parenthesis)
(global-set-key (kbd "C-S-l")   'insert-angle-brackets)
(global-set-key "\M-l"          'insert-brackets)
(global-set-key (kbd "M-S-l")   'insert-squares)

;; 一時的なコマンド束縛用(テスト用/試用)
;(global-set-key "\M-j" 'temp-command)

;;; ---------------------------------------------------------------------------
;;; Z prefix (to work something)
;;; ---------------------------------------------------------------------------

;; キーバインドの表示
(global-set-key "\C-z\C-k" 'describe-bindings)

;; 手前の空白を削除 (delete until black key)
(global-set-key "\C-zp"    'toggle-truncate-lines)

;; 手前の空白を削除 (delete until black key)
(global-set-key "\C-z\C-d" 'delete-until-black)

;; replace string my shortcut
(global-set-key "\C-z\C-r" 'replace-string)

;; vr/isearch側の正規表現置換
(global-set-key "\C-\M-s"  'vr/isearch-forward)
(global-set-key "\C-\M-r"  'vr/isearch-backward)

;; change encoding
(global-set-key "\C-zf"    'set-file-name-coding-system)

;; grep this % grep find this
(global-set-key "\C-z\C-b" 'grep-this)
(global-set-key "\C-z\C-f" 'grep-find-this)

;; swoop
(global-set-key "\C-z\C-s" 'swoop)

;; make buffer small
(global-set-key "\C-zs"    'make-buffer-small)

;; sublime 風outline
(global-set-key "\C-z\C-o" 'nurumacs-map-toggle)

;; magit status
(global-set-key "\C-zm"    'magit-status)

;;; ---------------------------------------------------------------------------
;;; A prefix (to edit somewhat)
;;; ---------------------------------------------------------------------------

;; insert-white spaces
(defun white-plus (n)
  (if (= n 0)
      '()
     `((global-set-key
        ,(concat "\C-a" (number-to-string n))
        '(lambda () (interactive) (insert-spaces ,n)))
       . ,(white-plus (- n 1)))))

(defmacro white-plus-m ()
  `(progn . ,(white-plus 9)))

(white-plus-m)

;; insert-bar
(defvar inserting-comment-line
  (apply #'concat (mapcar #'(lambda (x) "-") (number-sequence 1 40))))

(defun insert--s ()
  (interactive)
  (insert inserting-comment-line))

;; comment out
(global-set-key "\C-a\C-a" 'comment-dwim)

;; upcase/downcase-word
(global-set-key "\C-a\C-u" 'upcase-word)
(global-set-key "\C-a\C-p" 'downcase-word)

;; kill current buffer
(global-set-key "\C-a\C-k" 'kill-this-buffer)

;; insert-date key
(global-set-key "\C-a\C-d" 'insert-date-normal)
(global-set-key "\C-a\M-d" 'insert-date-markdown)

;; insert commment line
(global-set-key "\C-a\C-m" 'insert--s)

;; multi-comment-out-in keys
(global-set-key "\C-a\C-a" 'comment-dwim)

;; merge 2 lines
(global-set-key "\C-a\C-f" 'merge2lines)

;;; ---------------------------------------------------------------------------
;;; E prefix (to move somewhere)
;;; ---------------------------------------------------------------------------

;; 正規表現検索
(global-set-key "\C-e\C-s" 'search-forward-regexp)
(global-set-key "\C-e\C-r" 'search-backward-regexp)

;; Visual Regexp
(global-set-key "\C-e\C-d" 'vr/query-replace)

;; デフォルトの行先頭後尾移動 Ctrl-e / Ctrl-a の再設定
(global-set-key "\C-e\C-a" 'move-beginning-of-line)
(global-set-key "\C-e\C-e" 'move-end-of-line)

;; quick shell
(global-set-key "\C-e\C-c" 'shell)
(global-set-key "\C-e\C-v" 'move-to-scratch)
(global-set-key "\C-e\C-w" 'move-to-repl)

;; top center bottom間移動
(global-set-key "\C-e\C-l" 'recenter-top-bottom)

;; バッファ移動 (アスタリスク付バッファはスキップ)
(global-set-key "\C-e\C-b" 'previous-buffer-with-skip*)
(global-set-key "\C-e\C-f" 'next-buffer-with-skip*)

;; シンボル移動　(highlight-symbol-mode)
(global-set-key "\C-e\C-n" 'highlight-symbol-next)
(global-set-key "\C-e\C-p" 'highlight-symbol-prev)

;;;---------------------------------------------------------------------------
;;; provide
;;;---------------------------------------------------------------------------
(provide 'key-binding)
