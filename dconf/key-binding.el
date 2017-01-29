;;; package --- key-binding.el
;;; Commentary:
;;;  キーバインド設定
;;; Code:

;;; ---------------------------------------------------------------------------
;;; global-safe-set-key : 安全なglobalsetkeyとエラーレポート
;;; ---------------------------------------------------------------------------

(defvar gsskey-report-text nil)

(defvar gsskey-second-load nil)

(defmacro bind-gss-key (keybind-str sym)
  `(cond
    ((fboundp ,sym)
     (global-set-key ,keybind-str ,sym))
    (gsskey-second-load
     nil)
    (t
     (setq gsskey-report-text
	   (concat gsskey-report-text
		   ";; - failed to bind  : " (symbol-name ,sym) "\n")))))

(defun report-gsskey ()
  (if (not gsskey-report-text)
      ";; all keybinds successfully defined"
    (concat ";; gsskey error : \n" gsskey-report-text)))

;;; ---------------------------------------------------------------------------
;;; unset
;;; ---------------------------------------------------------------------------
(defun force-default-keys-disable ()
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

  (global-unset-key "\M-j"))

;;; ---------------------------------------------------------------------------
;;; no prefix
;;; ---------------------------------------------------------------------------

(defun force-no-prefix-keys-enable ()
  (bind-gss-key (kbd "C-S-k") 'backward-kill-line)

  ;; delete action
  (bind-gss-key "\C-h"     'delete-backward-char)
  (bind-gss-key "\M-h"     'backward-kill-word)

  ;; move by paragraph
  (bind-gss-key "\C-m"     'forward-paragraph)
  (bind-gss-key "\M-m"     'backward-paragraph)

  ;; next / previous buffer with skip *
  (bind-gss-key "\C-\M-f"  'next-buffer-with-skip*)
  (bind-gss-key "\C-\M-p"  'previous-buffer-with-skip*)

  ;; undo & redo
  (bind-gss-key "\C-q"     'undo)
  (bind-gss-key "\M-q"     'redo)

  ;; カーソル位置固定のままスクロール
  (bind-gss-key "\M-p"     'scroll-up-in-place)
  (bind-gss-key "\M-n"     'scroll-down-in-place)

  ;; ace jump mode
  (bind-gss-key "\M-a"     'ace-jump-mode)

  ;; vr/isearch側の正規表現置換
  (bind-gss-key "\C-\M-s"  'vr/isearch-forward)
  (bind-gss-key "\C-\M-r"  'vr/isearch-backward)

  ;; TODOコメント管理
  (bind-gss-key (kbd "C-.")  'goto-next-TODO)

  ;; アンダースコア挿入
  (bind-gss-key (kbd "C-:") 'insert-underscore)

  ;; シンボル移動　(highlight-symbol-mode)
  (bind-gss-key (kbd "C-?") 'highlight-symbol-next)
  (bind-gss-key (kbd "C-!") 'highlight-symbol-prev)

  ;; 括弧操作
  (bind-gss-key [C-return]      'kill-until-corresp-paren)
  (bind-gss-key "\C-l"          'insert-parenthesis)
  (bind-gss-key (kbd "C-S-l")   'insert-angle-brackets)
  (bind-gss-key "\M-l"          'insert-brackets)
  (bind-gss-key (kbd "M-S-l")   'insert-squares)

  ;; 一時的なコマンド束縛用(テスト用/試用)
  ;(bind-gss-key "\M-j" 'temp-command)

  ;; snippet : スニペット挿入
  (bind-gss-key (kbd "M-RET") 'yas-insert-snippet))

;;; ---------------------------------------------------------------------------
;;; Z prefix (to work something)
;;; ---------------------------------------------------------------------------

(defun force-z-prefix-keys-enable ()
  ;; キーバインドの表示
  (bind-gss-key "\C-z\C-k" 'describe-bindings)

  ;; 手前の空白を削除 (delete until black key)
  (bind-gss-key "\C-zp"    'toggle-truncate-lines)

  ;; 手前の空白を削除 (delete until black key)
  (bind-gss-key "\C-z\C-d" 'delete-until-black)

  ;; replace string my shortcut
  (bind-gss-key "\C-z\C-r" 'replace-string)

  ;; vr/isearch側の正規表現置換
  (bind-gss-key "\C-\M-s"  'vr/isearch-forward)
  (bind-gss-key "\C-\M-r"  'vr/isearch-backward)

  ;; change encoding
  (bind-gss-key "\C-zf"    'set-file-name-coding-system)

  ;; grep this % grep find this
  (bind-gss-key "\C-z\C-b" 'grep-this)
  (bind-gss-key "\C-z\C-f" 'grep-find-this)

  ;; swoop
  (bind-gss-key "\C-z\C-s" 'swoop)

  ;; make buffer small
  (bind-gss-key "\C-zs"    'make-buffer-small)

  ;; sublime 風outline
  (bind-gss-key "\C-z\C-o" 'nurumacs-map-toggle)

  ;; magit status
  (bind-gss-key "\C-zm"    'magit-status)

  ;; rgrep grep
  (bind-gss-key "\C-zr"    'rgrep)

  ;; google this
  (bind-gss-key "\C-zr"    'google-this)

  ;; bm-toggle
  (bind-gss-key "\C-z\C-t" 'bm-toggle)
  (bind-gss-key "\C-zt"    'bm-show)
  (bind-gss-key "\C-z\M-t" 'bm-show-all))

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

;; insert-bar
(defvar inserting-comment-line
  (apply #'concat (mapcar #'(lambda (x) "-") (number-sequence 1 40))))

(defun insert--s ()
  (interactive)
  (insert inserting-comment-line))

(defun force-a-prefix-keys-enable ()

  ;; white space insertion
  (white-plus-m)

  ;; comment out
  (bind-gss-key "\C-a\C-a" 'comment-dwim)

  ;; upcase/downcase-word
  (bind-gss-key "\C-a\C-u" 'upcase-word)
  (bind-gss-key "\C-a\C-p" 'downcase-word)

  ;; kill current buffer
  (bind-gss-key "\C-a\C-k" 'kill-this-buffer)

  ;; insert-date key
  (bind-gss-key "\C-a\C-d" 'insert-date-normal)
  (bind-gss-key "\C-a\M-d" 'insert-date-markdown)

  ;; insert commment line
  (bind-gss-key "\C-a\C-m" 'insert--s)

  ;; multi-comment-out-in keys
  (bind-gss-key "\C-a\C-a" 'comment-dwim)

  ;; merge 2 lines
  (bind-gss-key "\C-a\C-f" 'merge2lines))

;;; ---------------------------------------------------------------------------
;;; E prefix (to move somewhere)
;;; ---------------------------------------------------------------------------

(defun force-e-prefix-keys-enable ()

  ;; goto-chg
  (bind-gss-key "\C-e\C-l" 'goto-last-change)

  ;; point undo
  (bind-gss-key "\C-e\C-j" 'point-undo)
  (bind-gss-key "\C-e\C-k" 'point-redo)

  ;; 正規表現検索
  (bind-gss-key "\C-e\C-s" 'search-forward-regexp)
  (bind-gss-key "\C-e\C-r" 'search-backward-regexp)

  ;; Visual Regexp
  (bind-gss-key "\C-e\C-d" 'vr/query-replace)

  ;; デフォルトの行先頭後尾移動 Ctrl-e / Ctrl-a の再設定
  (bind-gss-key "\C-e\C-a" 'move-beginning-of-line)
  (bind-gss-key "\C-e\C-e" 'move-end-of-line)

  ;; quick shell
  (bind-gss-key "\C-e\C-c" 'shell)
  (bind-gss-key "\C-e\C-v" 'move-to-scratch)
  (bind-gss-key "\C-e\C-w" 'move-to-repl)

  ;; top center bottom間移動
  (bind-gss-key "\C-e\C-l" 'recenter-top-bottom)

  ;; バッファ移動 (アスタリスク付バッファはスキップ)
  (bind-gss-key "\C-e\C-b" 'previous-buffer-with-skip*)
  (bind-gss-key "\C-e\C-f" 'next-buffer-with-skip*)

  ;; シンボル移動　(highlight-symbol-mode)
  (bind-gss-key "\C-e\C-n" 'highlight-symbol-next)
  (bind-gss-key "\C-e\C-p" 'highlight-symbol-prev))


(defun force-key-binding ()
  (force-default-keys-disable)
  (force-no-prefix-keys-enable)
  (force-z-prefix-keys-enable)
  (force-e-prefix-keys-enable)
  (force-a-prefix-keys-enable))

(define-minor-mode key-force-power-mode
  "key-force-power mode foces key binding em all."
  t ;; default enabled
  " KFP" ;; modeline comment
  '()
  (force-key-binding))

(force-key-binding)
;; unset gsskey
(setq gsskey-second-load t)

;;;---------------------------------------------------------------------------
;;; provide
;;;---------------------------------------------------------------------------
(provide 'key-binding)
;;; key-binding.el ends here
