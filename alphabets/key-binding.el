;;; package --- key-binding.el
;;; Commentary:
;;;  キーバインド設定(helm以外)
;;;  global-safe-set-key from config-utils
;;; Code:
(require 'util-elisp)

;;; ---------------------------------------------------------------------------
;;; unset
;;; ---------------------------------------------------------------------------
(gssk-category "prefix")
(gssk-subcategory "解除")

(gssk-explain-function "prefix keyに使用")

;; 移動系 prefix
(gssk-add-keybind-report "C-e" 'unbind)
(global-unset-key "\C-e")

;; 編集系 prefix
(gssk-add-keybind-report "C-a" 'unbind)
(global-unset-key "\C-a")

;; 機能系 prefix
(gssk-add-keybind-report "C-z" 'unbind)
(global-unset-key "\C-z")

(gssk-explain-function "別用途のため解除")

(gssk-add-keybind-report "M-m" 'unbind)
(global-unset-key "\M-m")

(gssk-add-keybind-report "M-j" 'unbind)
(global-unset-key "\M-j")

;;; ---------------------------------------------------------------------------
;;; overwrite default keybind
;;; ---------------------------------------------------------------------------
(ignore-report
  (global-set-key (kbd "C-x C-f") 'counsel-find-file))

;;; ---------------------------------------------------------------------------
;;; no prefix
;;; ---------------------------------------------------------------------------
(defun temp-command ()
  (interactive)
  (message "this shortcut is for debug or some."))

(gssk-category-function "機能" "" "undo & redo")
(gssk-bind "C-q"    'undo)
(gssk-bind "M-q"    'redo)

(gssk-category-function "編集" "削除" "Backspaceで削除 (文字単位/単語単位)")
(gssk-bind "C-h"    'delete-backward-char)
(gssk-bind "M-h"    'backward-kill-word)

(gssk-category-function "編集" "削除" "セマンティクス削除")
(gssk-bind "M-i"    'change-inner)
(gssk-bind "M-o"    'change-outer)

(gssk-category-function "編集" "挿入" "アンダースコア挿入")
(gssk-bind "C-:"    'insert-underscore)

(gssk-category-function "編集" "挿入" "snippet: yaスニペット挿入")
(gssk-bind "M-RET"  'yas-insert-snippet)

(gssk-category-function "編集" "括弧" "括弧挿入")
(gssk-bind "C-l"    'insert-parenthesis)
(gssk-bind "C-S-l"  'insert-angle-brackets)
(gssk-bind "M-l"    'insert-brackets)
(gssk-bind "M-S-l"  'insert-squares)

(gssk-category-function "移動" "バッファ内" "マーク単位の移動")
(gssk-bind "M-m"    'counsel-mark-ring)

(gssk-category-function "移動" "バッファ内" "1行スクロール(カーソル位置固定)")
(gssk-bind "M-p"    'scroll-up-in-place)
(gssk-bind "M-n"    'scroll-down-in-place)

(gssk-category-function "移動" "バッファ内" "指定行へ移動(1回でgoto-line)")
(gssk-bind "M-g"    'goto-line)

(gssk-category-function "移動" "バッファ内" "ace jump mode")
(gssk-bind "M-a"    'ace-jump-mode)

(gssk-category-function "移動" "バッファ内" "次のTODOへ移動")
(gssk-bind "C-,"    'goto-next-TODO)

(gssk-category-function "移動" "バッファ内" "次のエラー(警告)へ移動")
(gssk-bind "C-."    'goto-next-locus)

(gssk-category-function "移動" "バッファ内" "シンボル単位移動")
(gssk-bind "C-?"    'highlight-symbol-next)
(gssk-bind "C-!"    'highlight-symbol-prev)

(gssk-category-function "移動" "バッファ間" "バッファ移動 (*付バッファはスキップ)")
(gssk-bind "C-M-f"  'next-buffer-with-skip*)
(gssk-bind "C-M-p"  'previous-buffer-with-skip*)

(gssk-category-function "その他" "その他" "文字の拡大/縮小")
(gssk-bind "C-S-+" 'text-scale-increase)
(gssk-bind "C-S--" 'text-scale-decrease)

(gssk-category-function "その他" "その他" "数値のインクリメント/デクリメント")
(gssk-bind "C-+"   'increment-number)
(gssk-bind "C--"   'decrement-number)

(gssk-category-function "その他" "その他" "一時的なコマンド束縛用(テスト用/試用)")
(gssk-bind "M-j"   'temp-command)

;;; ---------------------------------------------------------------------------
;;; Z prefix (to work something)
;;; ---------------------------------------------------------------------------
(gssk-category "機能")
(gssk-subcategory "")

(gssk-explain-function "enable/disable toggle-truncate-line")
(gssk-bind "C-z p"   'toggle-truncate-lines)

(gssk-explain-function "現在のバッファ以外のバッファを閉じる")
(gssk-bind "C-z C-k" 'kill-the-other-buffers)

(gssk-explain-function "エンコーディングを変更")
(gssk-bind "C-z f"   'set-file-name-coding-system)

(gssk-explain-function "インスタント・メモファイルを開く")
(gssk-bind "C-z C-z" 'zsnotes-open-today-note)

(gssk-explain-function "ジャンクファイルを作成、開く")
(gssk-bind "C-z C-j" 'open-junk-file)

(gssk-subcategory "置換")

(gssk-explain-function "文字列置換")
(gssk-bind "C-z C-r" 'replace-string)

(gssk-subcategory "検索")

(gssk-explain-function "grep this: バッファ内Grep")
(gssk-bind "C-z g"   'grep-this)

(gssk-explain-function "grep find this: ディレクトリ配下Grep")
(gssk-bind "C-z C-f" 'grep-find-this)

(gssk-explain-function "swoop: バッファ内を動的検索/移動")
(gssk-bind "C-z C-s" 'swoop)

(gssk-explain-function "codic: コーディング用辞書")
(gssk-bind "C-z C-c" 'codic)

(gssk-explain-function "define-word: 英英辞典")
(gssk-bind "C-z w"   'define-word)
(gssk-bind "C-z C-w" 'define-word-at-point)

(gssk-explain-function "rgrep: ディレクトリ内Grep")
(gssk-bind "C-z r"   'rgrep)

(gssk-explain-function "minimap: ソースコードのアウトライン表示")
(gssk-bind "C-z C-y" 'minimap-mode)

(gssk-explain-function "counsel-git-grep: Git-Grep")
(gssk-bind "C-z C-b" 'counsel-git-grep)

(gssk-explain-function "google-this(Googleで検索)")
(gssk-bind "C-z M-g" 'google-this)

(gssk-explain-function "現在のURLリンクを開く<br/>(goto-address-mode)")
(gssk-bind "C-z C-a" 'goto-address-at-point)

(gssk-explain-function "imenu-list(関数定義一覧表示)")
(gssk-bind "C-z i l" 'imenu-list-smart-toggle)

(gssk-explain-function "counsel-imenu(関数定義一覧検索)")
(gssk-bind "C-z i f" 'counsel-imenu)

(gssk-explain-function "ibuffer(バッファ一覧表示)")
(gssk-bind "C-z i b" 'ibuffer)

(gssk-subcategory "表示")

(gssk-explain-function "バッファのフレームサイズを縮小")
(gssk-bind "C-z s"   'make-buffer-small)

(gssk-explain-function "ディレクトリ階層を表示 (neo tree)")
(gssk-bind "C-z C-n" 'neotree-toggle)

(gssk-explain-function "magit (Emacs Git)")
(gssk-bind "C-z m"   'magit-status)

(gssk-explain-function "キーバインド表示(counsel)")
(gssk-bind "C-z C-k" 'counsel-descbinds)

(gssk-explain-function "コマンド表示(counsel)")
(gssk-bind "C-z k"   'counsel-apropos)

(gssk-subcategory "辞書")

(gssk-explain-function "現在の単語の意味を表示(要辞書設定)")
(gssk-bind "C-z C-d" 'search-dictionary-e2j-current-word)
(gssk-explain-function "英和辞典(要辞書設定)")
(gssk-bind "C-z d"   'search-dictionary-e2j)

(gssk-subcategory "実行")

(gssk-explain-function "現在のディレクトリのxxx.sh実行")
(gssk-bind "C-z e"   'execute-current-shell-script)

(gssk-subcategory "日記")

(gssk-explain-function "今日の日記を作成")
(gssk-bind "C-z l n" 'daily-notes-open-today-note)
(gssk-explain-function "既存の日記一覧を表示")
(gssk-bind "C-z l o" 'daily-notes-open-default-file)

(gssk-subcategory "ブログ")

(gssk-explain-function "新しいブログポストを作成")
(gssk-bind "C-z b n" 'quickblog-create-new-post)
(gssk-explain-function "既存のブログポスト一覧を表示")
(gssk-bind "C-z b o" 'quickblog-open-default-file)
(gssk-explain-function "Cryogenをローカルで実行")
(gssk-bind "C-z b r" 'quickblog-run-local-server)

;;; ---------------------------------------------------------------------------
;;; A prefix (to edit somewhat)
;;; ---------------------------------------------------------------------------
(gssk-category "編集")
(gssk-subcategory "")

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

(white-plus-m)

(gssk-explain-function "white space insertion")

(defun insert-turapoyo ()
  (interactive)
  (insert "(´･_･`)"))

(defun insert-current-file-name ()
  (interactive)
  (insert (buffer-file-name (current-buffer))))

(gssk-subcategory "")

(gssk-explain-function "comment out/in")
(gssk-bind "C-a C-a" 'comment-dwim)

(gssk-explain-function "upcase/downcase word")
(gssk-bind "C-a C-u" 'upcase-word)
(gssk-bind "C-a C-p" 'downcase-word)

(gssk-subcategory "削除")

(gssk-explain-function "括弧削除")
(gssk-bind "C-a C-c" 'kill-until-corresp-paren)

(gssk-explain-function "現在のバッファを削除")
(gssk-bind "C-a C-k" 'kill-this-buffer)

(gssk-explain-function "行のマージ(インデント用などの空白削除) <br/> (不要かも)")
(gssk-bind "C-a C-f" 'merge2lines)

(gssk-explain-function "周囲の空白を削除し、単一の空白にする")
(gssk-bind "C-a C-i" 'just-one-space)

(gssk-subcategory "挿入")

(gssk-explain-function "旧(C-q) 引用付き挿入(置換等で使用)")
(gssk-bind "C-a C-q" 'quoted-insert)

(gssk-explain-function "(´･_･`)を挿入")
(gssk-bind "C-a C-s" 'insert-turapoyo)

(gssk-explain-function "現在時刻挿入")
(gssk-bind "C-a C-d" 'insert-date-normal)
(gssk-bind "C-a M-d" 'insert-date-markdown)

(gssk-explain-function "現在のファイルパスを挿入")
(gssk-bind "C-a C-e" 'insert-current-file-name)

(gssk-explain-function "コメント用の線を挿入")
(gssk-bind "C-a C-m" 'insert--s)

(gssk-explain-function "killringから選択して挿入")
(gssk-bind "C-a C-y" 'counsel-yank-pop)

(gssk-subcategory "修正")

(gssk-explain-function "iedit-mode: 同一のシンボルを同時置換")
(gssk-bind "C-a i" 'iedit-mode)

(gssk-subcategory "その他")

(gssk-explain-function "矩形選択")
(gssk-bind "C-a C-r" 'rectangle-mark-mode)

;;; ---------------------------------------------------------------------------
;;; E prefix (to move somewhere)
;;; ---------------------------------------------------------------------------
(gssk-category "移動")

(gssk-subcategory "バッファ内")

(gssk-explain-function "最後の変更箇所へ")
(gssk-bind "C-e C-l" 'goto-last-change)

(gssk-explain-function "行頭/行末へ(unbindの再設定)")
(gssk-bind "C-e C-a" 'move-beginning-of-line)
(gssk-bind "C-e C-e" 'move-end-of-line)

(gssk-explain-function "top-center-bottom間")
(gssk-bind "C-e C-l" 'recenter-top-bottom)

(gssk-explain-function "imenu: 関数定義へ")
(gssk-bind "C-e C-l" 'imenu-list)

(gssk-explain-function "swiper: バッファ内を動的検索/移動")
(gssk-bind "C-e C-s" 'swiper)

(gssk-subcategory "バッファ間")

(gssk-explain-function "ace-window: Window間移動")
(gssk-bind "C-e C-z" 'ace-window)

(gssk-explain-function "shell/repl/grepxへ")
(gssk-bind "C-e C-c" 'shell)
(gssk-bind "C-e C-v" 'move-to-scratch)
(gssk-bind "C-e g"   'move-to-grep)
(gssk-bind "C-e C-w" 'move-to-repl)

(gssk-explain-function "次/前のバッファへ (*付バッファはスキップ)")
(gssk-bind "C-e C-b" 'previous-buffer-with-skip*)
(gssk-bind "C-e C-f" 'next-buffer-with-skip*)

(gssk-subcategory "検索")

(gssk-explain-function "正規表現検索 (通常)")
(gssk-bind "C-e s" 'search-forward-regexp)
(gssk-bind "C-e r" 'search-backward-regexp)

(gssk-explain-function "正規表現検索 (一覧表示)")
(gssk-bind "C-e C-o" 'occur)

(gssk-explain-function "正規表現置換 (Emacs標準)")
(gssk-bind "C-e C-r" 'replace-regexp)

(gssk-explain-function "関数の定義位置に移動(dumb-jump)")
(gssk-bind "C-e C-j" 'dumb-jump-go)
(gssk-bind "C-e C-k" 'dumb-jump-back)

(gssk-explain-function "次/前のシンボルの位置へ")
(gssk-bind "C-e C-n" 'highlight-symbol-next)
(gssk-bind "C-e C-p" 'highlight-symbol-prev)

(gssk-subcategory "ファイル")

(gssk-explain-function "最近開いたファイルを開く")
(gssk-bind "C-e o" 'recentf-open-files)

(gssk-explain-function "ファイルを開く(Gitベース)")
(gssk-bind "C-e f" 'counsel-git)

(gssk-explain-function "counsel-ag: ag search")
(gssk-bind "C-e C-y"   'counsel-ag)

(gssk-subcategory "Bookmark")
(gssk-explain-function "現在行をブックマーク、ハイライト表示")
(gssk-bind "C-e C-t" 'bm-toggle)
(gssk-bind "C-e C-i" 'bm-next)
(gssk-bind "C-e C-u" 'bm-previous)
(gssk-bind "C-e t"   'bm-show)
(gssk-bind "C-e M-t" 'bm-show-all)

;;; ---------------------------------------------------------------------------
;;; provide
;;; ---------------------------------------------------------------------------
(provide 'key-binding)
;;; key-binding.el ends here
