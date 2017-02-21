;;; package --- key-binding.el
;;; Commentary:
;;;  キーバインド設定
;;; Code:

;;; ---------------------------------------------------------------------------
;;; global-safe-set-key : 安全なglobalsetkeyとエラーレポート、キーバインドレポート
;;; ---------------------------------------------------------------------------

;; キーバインド情報用(標準以外)
;; ((category binding-key emacs-function-name function-explaination))
(defvar gssk-keybind-report '())

(defvar gssk-current-category-state "")

(defvar gssk-current-function-name-state "")

(defun gssk-category
  (text)
  (setq gssk-current-category-state text))

(defun gssk-explain-function
  (text)
  (setq gssk-current-function-name-state text))

(defun gssk-category-function
  (category-text function-text)
  (setq gssk-current-category-state category-text)
  (setq gssk-current-function-name-state function-text))

(defvar gsskey-report-text nil)

(defun gssk-add-keybind-report
 (keybind-str sym)
 (add-to-list
  'gssk-keybind-report
  (list gssk-current-category-state
		keybind-str (symbol-name sym)
		gssk-current-function-name-state)))

(defmacro gssk-bind (keybind-str sym)
  `(cond
    ((fboundp ,sym)
	 (progn
	   (gssk-add-keybind-report ,keybind-str ,sym)
	   (global-set-key (kbd ,keybind-str) ,sym)))
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
(gssk-category "prefix key 解除")
(gssk-explain-function "prefix key 解除")
;; prefix key (control)
;; q w E r t y U _ o p
;;  A s d f g h j k l
;;   Z X C v b n m _ .
;; upper cases : control-prefix
;; lower cases : not control-prefix
;; underscore  : not binding

;; 標準のprefix
;; \C-x

;; 移動系 prefix
(gssk-add-keybind-report "C-e" 'unbind)
(global-unset-key "\C-e")

;; 編集系 prefix
(gssk-add-keybind-report "C-a" 'unbind)
(global-unset-key "\C-a")

;; 機能系 prefix
(gssk-add-keybind-report "C-z" 'unbind)
(global-unset-key "\C-z")

;; prefix key (meta)
;; q w e r t y u i o p
;;  a S d f G h j k l
;;   z x c v b n m _ .
;; upper cases : meta-prefix
;; lower cases : not meta-prefix
;; underscore  : not binding

(gssk-add-keybind-report "M-m" 'unbind)
(global-unset-key "\M-m")

(gssk-add-keybind-report "M-j" 'unbind)
(global-unset-key "\M-j")

;;; ---------------------------------------------------------------------------
;;; no prefix
;;; ---------------------------------------------------------------------------

(gssk-bind (kbd "C-S-k") 'backward-kill-line)

(gssk-category-function "編集" "Backspaceでの削除 (文字単位/単語単位)")
(gssk-bind "C-h"    'delete-backward-char)
(gssk-bind "M-h"    'backward-kill-word)

(gssk-category-function "移動" "パラグラフ単位の移動")
(gssk-bind "C-m"    'forward-paragraph)
(gssk-bind "M-m"    'backward-paragraph)

(gssk-category-function "移動" "バッファ移動 (アスタリスク付バッファはスキップ)")
(gssk-bind "C-M-f"  'next-buffer-with-skip*)
(gssk-bind "C-M-p"  'previous-buffer-with-skip*)

(gssk-category-function "機能" "undo & redo")
(gssk-bind "C-q"    'undo)
(gssk-bind "M-q"    'redo)

(gssk-category-function "移動" "スクロール(カーソル位置固定)")
(gssk-bind "M-p"    'scroll-up-in-place)
(gssk-bind "M-n"    'scroll-down-in-place)

(gssk-category-function "移動" "goto-line(１回)")
(gssk-bind "M-g"    'goto-line)

(gssk-category-function "移動" "ace jump mode")
(gssk-bind "M-a"    'ace-jump-mode)

(gssk-category-function "編集" "vr/isearch側の正規表現置換")
(gssk-bind "C-M-s"  'vr/isearch-forward)
(gssk-bind "C-M-r"  'vr/isearch-backward)

(gssk-category-function "移動" "TODOへ移動")
(gssk-bind "C-."    'goto-next-TODO)

(gssk-category-function "編集" "アンダースコア挿入")
(gssk-bind "C-:"    'insert-underscore)

(gssk-category-function "移動" "シンボル移動　(highlight-symbol-mode)")
(gssk-bind "C-?"    'highlight-symbol-next)
(gssk-bind "C-!"    'highlight-symbol-prev)

(gssk-category-function "編集" "括弧操作")
(gssk-bind "C-RET"  'kill-until-corresp-paren)
(gssk-bind "C-l"    'insert-parenthesis)
(gssk-bind "C-S-l"  'insert-angle-brackets)
(gssk-bind "M-l"    'insert-brackets)
(gssk-bind "M-S-l"  'insert-squares)

(gssk-category-function "機能" "別バッファへ移動")
(gssk-bind "M-o"     'other-window)

(gssk-category-function "機能" "一時的なコマンド束縛用(テスト用/試用)")
(gssk-bind "M-j"     'temp-command)

(gssk-category-function "編集" "snippet : スニペット挿入")
(gssk-bind "M-RET"  'yas-insert-snippet)

;;; ---------------------------------------------------------------------------
;;; Z prefix (to work something)
;;; ---------------------------------------------------------------------------
(gssk-category "機能")

(gssk-explain-function "キーバインド表示")
(gssk-bind "C-z C-k" 'describe-bindings)

(gssk-explain-function "enable/disable toggle-truncate-line")
(gssk-bind "C-z p"   'toggle-truncate-lines)

(gssk-explain-function "手前の空白を削除 (delete until black key)")
(gssk-bind "C-z C-d" 'delete-until-black)

(gssk-explain-function "ディレクトリ階層を表示 (neo tree)")
(gssk-bind "C-z C-n"   'neotree-toggle)

(gssk-explain-function "replace-string")
(gssk-bind "C-z C-r" 'replace-string)

(gssk-explain-function "vr/isearch側の正規表現置換")
(gssk-bind "C-M-s"   'vr/isearch-forward)
(gssk-bind "C-M-r"   'vr/isearch-backward)

(gssk-explain-function "change encoding")
(gssk-bind "C-z f"   'set-file-name-coding-system)

(gssk-explain-function "grep this & grep find this")
(gssk-bind "C-z C-b" 'grep-this)
(gssk-bind "C-z C-f" 'grep-find-this)

(gssk-explain-function "swoop")
(gssk-bind "C-z C-s" 'swoop)

(gssk-explain-function "バッファのウィンドウサイズを縮小")
(gssk-bind "C-z s"   'make-buffer-small)

(gssk-explain-function "sublime風のoutline")
(gssk-bind "C-z C-o" 'nurumacs-map-toggle)

(gssk-explain-function "magit (Emacs Git)")
(gssk-bind "C-z m"   'magit-status)

(gssk-explain-function "rgrep")
(gssk-bind "C-z r"   'rgrep)

(gssk-explain-function "google-this(Googleで検索)")
(gssk-bind "C-z r"   'google-this)

(gssk-explain-function "bm-toggle")
(gssk-bind "C-z C-t" 'bm-toggle)
(gssk-bind "C-z t"   'bm-show)
(gssk-bind "C-z M-t" 'bm-show-all)

;;; ---------------------------------------------------------------------------
;;; A prefix (to edit somewhat)
;;; ---------------------------------------------------------------------------
(gssk-category "編集")

(gssk-explain-function "insert-white spaces")
(defun white-plus (n)
  (if (= n 0)
      '()
     `((global-set-key
        ,(concat "\C-a" (number-to-string n))
        '(lambda () (interactive) (insert-spaces ,n)))
       . ,(white-plus (- n 1)))))

(defmacro white-plus-m ()
  `(progn . ,(white-plus 9)))

(gssk-explain-function "insert-bar")
(defvar inserting-comment-line
  (apply #'concat (mapcar #'(lambda (x) "-") (number-sequence 1 40))))

(defun insert--s ()
  (interactive)
  (insert inserting-comment-line))

(gssk-explain-function "white space insertion")
(white-plus-m)

(gssk-explain-function "コメント アウト/イン")
(gssk-bind "C-a C-a" 'comment-dwim)

(gssk-explain-function "upcase/downcase-word")
(gssk-bind "C-a C-u" 'upcase-word)
(gssk-bind "C-a C-p" 'downcase-word)

(gssk-explain-function "現在のバッファを消す")
(gssk-bind "C-a C-k" 'kill-this-buffer)

(gssk-explain-function "現在時刻挿入")
(gssk-bind "C-a C-d" 'insert-date-normal)
(gssk-bind "C-a M-d" 'insert-date-markdown)

(gssk-explain-function "insert comment line")
(gssk-bind "C-a C-m" 'insert--s)

(gssk-explain-function "merge 2 lines")
(gssk-bind "C-a C-f" 'merge2lines)

;;; ---------------------------------------------------------------------------
;;; E prefix (to move somewhere)
;;; ---------------------------------------------------------------------------
(gssk-category "移動")

(gssk-explain-function "最後の変更箇所へ移動")
(gssk-bind "C-e C-l" 'goto-last-change)

(gssk-explain-function "最後のカーソル位置へ移動")
(gssk-bind "C-e C-j" 'point-undo)
(gssk-bind "C-e C-k" 'point-redo)

(gssk-explain-function "正規表現検索")
(gssk-bind "C-e C-s" 'search-forward-regexp)
(gssk-bind "C-e C-r" 'search-backward-regexp)

(gssk-explain-function "Visual Regexp")
(gssk-bind "C-e C-d" 'vr/query-replace)

(gssk-explain-function "行頭/行末へ移動(unbindの再設定)")
(gssk-bind "C-e C-a" 'move-beginning-of-line)
(gssk-bind "C-e C-e" 'move-end-of-line)

(gssk-explain-function "shell/replへ移動")
(gssk-bind "C-e C-c" 'shell)
(gssk-bind "C-e C-v" 'move-to-scratch)
(gssk-bind "C-e C-w" 'move-to-repl)

(gssk-explain-function "top-center-bottom間移動")
(gssk-bind "C-e C-l" 'recenter-top-bottom)

(gssk-explain-function "バッファ移動 (アスタリスク付バッファはスキップ)")
(gssk-bind "C-e C-b" 'previous-buffer-with-skip*)
(gssk-bind "C-e C-f" 'next-buffer-with-skip*)

(gssk-explain-function "シンボル単位移動　(highlight-symbol-mode)")
(gssk-bind "C-e C-n" 'highlight-symbol-next)
(gssk-bind "C-e C-p" 'highlight-symbol-prev)

;;; ---------------------------------------------------------------------------
;;; gssk : binding report
;;; ---------------------------------------------------------------------------

(defconst grm-keybind-header
  "|カテゴリ|キーバインド|function|機能名|")

(defconst grm-keybind-table-line
  "| --------------- |:---------------| -------------------- |:-------|")

(defun gssk-setting-md "")

(defun generate-explanation-text ()
  (apply 'concat
		 (mapcar '(lambda (x) (concat "|" (car x)
									  "|" (car (cdr x))
									  "|" (car (cdr (cdr x)))
									  "|" (car (cdr (cdr (cdr x)))) "|\n"))
				 (reverse gssk-keybind-report))))

(defvar keybinding-md
  (concat
   grm-keybind-header
   "\n"
   grm-keybind-table-line
   "\n"
   (generate-explanation-text)))

;;; ---------------------------------------------------------------------------
;;; provide
;;; ---------------------------------------------------------------------------
(provide 'key-binding)
;;; key-binding.el ends here
