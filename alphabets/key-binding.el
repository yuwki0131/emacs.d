;;; package --- key-binding.el
;;; Commentary:
;;;  キーバインド設定(helm以外)
;;;  global-safe-set-key from config-utils
;;; Code:
(require 'util-elisp)

;;; ---------------------------------------------------------------------------
;;; unset
;;; ---------------------------------------------------------------------------
(gssk-category "なし")

(gssk-subcategory "Prefix")
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

(gssk-subcategory "別用途")
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

(gssk-category "機能")

(gssk-repeat-bind-ex-with-subcategory "表示"
 '(("C-S-+" text-scale-increase "文字の拡大/縮小")
   ("C-S--" text-scale-decrease "文字の拡大/縮小")))

(gssk-repeat-bind-ex-with-subcategory "その他"
 '(("M-j" temp-command "一時的なコマンド束縛用(テスト用/試用)")))

(gssk-category "編集")

(gssk-repeat-bind-ex-with-subcategory "履歴"
 '(("C-q" undo "undo")
   ("M-q" redo "redo")))

(gssk-repeat-bind-ex-with-subcategory "挿入"
 '(("C-:"   insert-underscore     "アンダースコア挿入")
   ("M-RET" yas-insert-snippet    "snippet: yaスニペット挿入")
   ("C-l"   insert-parenthesis    "括弧挿入()")
   ("C-S-l" insert-angle-brackets "括弧挿入<>")
   ("M-l"   insert-brackets       "括弧挿入{}")
   ("M-S-l" insert-squares        "括弧挿入[]")))

(gssk-repeat-bind-ex-with-subcategory "削除"
 '(("C-h" delete-backward-char "Backspaceで削除 (文字単位/単語単位)")
   ("M-h" backward-kill-word   "Backspaceで削除 (文字単位/単語単位)")
   ("M-i" change-inner         "セマンティクス削除")
   ("M-o" change-outer         "セマンティクス削除")))

(gssk-repeat-bind-ex-with-subcategory "数値"
 '(("C-+" increment-number "数値のインクリメント")
   ("C--" decrement-number "数値のデクリメント")))

(gssk-category "移動")

(gssk-repeat-bind-ex-with-subcategory  "バッファ内"
 '(("M-m" counsel-mark-ring     "マーク単位の移動")
   ("M-p" scroll-up-in-place    "1行スクロール(カーソル位置固定)")
   ("M-n" scroll-down-in-place  "1行スクロール(カーソル位置固定)")
   ("M-g" goto-line             "指定行へ移動(1回でgoto-line)")
   ("M-a" ace-jump-mode         "ace jump mode")
   ("C-," goto-next-TODO        "次のTODOへ移動")
   ("C-." goto-next-locus       "次のエラー(警告)へ移動")
   ("C-?" highlight-symbol-next "シンボル単位移動")
   ("C-!" highlight-symbol-prev "シンボル単位移動")))

(gssk-repeat-bind-ex-with-subcategory  "バッファ間"
 '(("C-M-f" next-buffer-with-skip* "バッファ移動 (*付バッファはスキップ)")
   ("C-M-p" previous-buffer-with-skip* "バッファ移動 (*付バッファはスキップ)")))

;;; ---------------------------------------------------------------------------
;;; other(without z a e) prefix
;;; ---------------------------------------------------------------------------
(gssk-category-function "移動" "バッファ間" "グローバルに検索しファイルを開く")
(gssk-bind "C-c C-f"   'counsel-locate)

;;; ---------------------------------------------------------------------------
;;; Z prefix (to work something)
;;; ---------------------------------------------------------------------------
(gssk-category "機能")

(gssk-repeat-bind-ex-with-subcategory "開く"
 '(("C-z C-k" kill-the-other-buffers      "現在のバッファ以外のバッファを閉じる")
   ("C-z C-e" set-file-name-coding-system "エンコーディングを変更")
   ("C-z C-z" zsnotes-open-today-note     "インスタント・メモファイルを開く")
   ("C-z C-j" open-junk-file              "ジャンクファイルを作成、開く")))

(gssk-repeat-bind-ex-with-subcategory "置換"
 '(("C-z C-r" replace-string "文字列置換")))

(gssk-repeat-bind-ex-with-subcategory "辞書"
 '(("C-z C-c" codic                   "codic: コーディング用辞書")
   ("C-z w"   define-word             "英英辞典で検索")
   ("C-z C-w" define-word-at-point    "現在位置の単語を英英辞典で検索")))

(gssk-repeat-bind-ex-with-subcategory "Web"
 '(("C-z M-g" google-this             "google-this(Googleで検索)")
   ("C-z C-a" goto-address-at-point   "現在のURLリンクを開く")))

(gssk-repeat-bind-ex-with-subcategory "表示"
 '(("C-z p"   toggle-truncate-lines   "enable/disable toggle-truncate-line")
   ("C-z i l" imenu-list-smart-toggle "imenu-list(関数定義一覧表示)")
   ("C-z i f" counsel-imenu           "counsel-imenu(関数定義一覧検索)")
   ("C-z i b" ibuffer                 "ibuffer(バッファ一覧表示)")
   ("C-z C-y" minimap-mode            "minimap: ソースコードのアウトライン表示")
   ("C-z s"   make-buffer-small       "バッファのフレームサイズを縮小")
   ("C-z C-n" neotree-toggle          "ディレクトリ階層を表示 (neo tree)")
   ("C-z m"   magit-status            "magit (Emacs Git)")
   ("C-z C-k" counsel-descbinds       "キーバインド表示(counsel)")
   ("C-z k"   counsel-apropos         "コマンド表示(counsel)")))

(gssk-repeat-bind-ex-with-subcategory "辞書"
 '(("C-z C-d" search-dictionary-e2j-current-word "現在の単語の意味を表示(要辞書設定)")
   ("C-z d"   search-dictionary-e2j              "英和辞典(要辞書設定)")))

(gssk-repeat-bind-ex-with-subcategory "実行"
 '(("C-z e" execute-current-shell-script "現在のディレクトリのxxx.sh実行")))

(gssk-repeat-bind-ex-with-subcategory "日記"
 '(("C-z l n" daily-notes-open-today-note   "今日の日記を作成")
   ("C-z l o" daily-notes-open-default-file "既存の日記一覧を表示")))

(gssk-repeat-bind-ex-with-subcategory "ブログ"
 '(("C-z b n" quickblog-create-new-post   "新しいブログポストを作成")
   ("C-z b o" quickblog-open-default-file "既存のブログポスト一覧を表示")
   ("C-z b r" quickblog-run-local-server  "Cryogenをローカルで実行")))

;;; ---------------------------------------------------------------------------
;;; A prefix (to edit somewhat)
;;; ---------------------------------------------------------------------------
(gssk-category "編集")

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

(defun insert-turapoyo ()
  (interactive)
  (insert "(´･_･`)"))

(defun insert-current-file-name ()
  (interactive)
  (insert (buffer-file-name (current-buffer))))

(gssk-repeat-bind-ex-with-subcategory "切替"
 '(("C-a C-a" comment-dwim  "コメントアウト切り替え")
   ("C-a C-u" upcase-word   "upcase word")
   ("C-a C-p" downcase-word "downcase word")))

(gssk-repeat-bind-ex-with-subcategory "削除"
 '(("C-a C-c" kill-until-corresp-paren "括弧削除")
   ("C-a C-k" kill-this-buffer         "現在のバッファを削除")
   ("C-a C-f" merge2lines              "行のマージ(インデント用などの空白削除)")
   ("C-a C-i" just-one-space           "周囲の空白を削除し、単一の空白にする")))

(gssk-repeat-bind-ex-with-subcategory "挿入"
 '(("C-a C-q" quoted-insert            "旧(C-q) 引用付き挿入(置換等で使用)")
   ("C-a C-s" insert-turapoyo          "(´･_･`)を挿入")
   ("C-a C-d" insert-date-normal       "現在時刻挿入(通常)")
   ("C-a M-d" insert-date-markdown     "現在時刻挿入(Markdown用)")
   ("C-a C-e" insert-current-file-name "現在のファイルパスを挿入")
   ("C-a C-m" insert--s                "コメント用の線を挿入")
   ("C-a C-y" counsel-yank-pop         "killringから選択して挿入")))

(gssk-repeat-bind-ex-with-subcategory "置換"
 '(("C-a i" iedit-mode "iedit-mode: 同一のシンボルを同時置換")))

(gssk-repeat-bind-ex-with-subcategory "その他"
 '(("C-a C-r" rectangle-mark-mode "矩形選択")))

;;; ---------------------------------------------------------------------------
;;; E prefix (to move somewhere)
;;; ---------------------------------------------------------------------------
(gssk-category "移動")

(gssk-repeat-bind-ex-with-subcategory "バッファ内"
 '(("C-e C-l" goto-last-change       "最後の変更箇所へ")
   ("C-e C-a" move-beginning-of-line "行頭へ(unbindの再設定)")
   ("C-e C-e" move-end-of-line       "行末へ(unbindの再設定)")
   ("C-e C-l" recenter-top-bottom    "top-center-bottom間")
   ("C-e C-l" imenu-list             "imenu: 関数定義へ")
   ("C-e C-s" swiper                 "swiper: バッファ内を動的検索/移動")
   ("C-e C-q" swoop                  "swoop: バッファ内を動的検索/移動")))

(gssk-repeat-bind-ex-with-subcategory "バッファ間"
 '(("C-e C-z" ace-window                 "ace-window: Window間移動")
   ("C-e C-c" shell                      "shellへ移動")
   ("C-e C-v" move-to-scratch            "scratchへ移動")
   ("C-e g"   move-to-grep               "grepへ移動")
   ("C-e C-w" move-to-repl               "replへ移動")
   ("C-e C-b" previous-buffer-with-skip* "前のバッファへ (*付バッファはスキップ)")
   ("C-e C-f" next-buffer-with-skip*     "次のバッファへ (*付バッファはスキップ)")))

(gssk-repeat-bind-ex-with-subcategory "検索"
 '(("C-e s"   search-forward-regexp     "正規表現検索 (通常/前方)")
   ("C-e r"   search-backward-regexp    "正規表現検索 (通常/後方)")
   ("C-e C-o" occur                     "正規表現検索 (一覧表示)")
   ("C-e C-r" anzu-query-replace-regexp "正規表現置換 (anzu)")
   ("C-e C-j" dumb-jump-go              "関数の定義位置に移動(dumb-jump)")
   ("C-e C-k" dumb-jump-back            "関数の定義位置に移動(dumb-jump)")
   ("C-e C-n" highlight-symbol-next     "次のシンボルの位置へ")
   ("C-e C-p" highlight-symbol-prev     "前のシンボルの位置へ")))

(gssk-repeat-bind-ex-with-subcategory "Grep"
 '(("C-e C-d C-r" rgrep            "rgrep: ディレクトリ内Grep")
   ("C-e C-d C-f" grep-find-this   "grep find this: ディレクトリ配下Grep")
   ("C-e C-d C-t" grep-this        "grep this: バッファ内Grep")
   ("C-e C-d C-c" counsel-git-grep "counsel-git-grep: Git-Grep")
   ("C-e C-y"     counsel-ag       "counsel-ag: ag search")))

(gssk-repeat-bind-ex-with-subcategory "ファイル"
 '(("C-e o" recentf-open-files "最近開いたファイルを開く")
   ("C-e f" counsel-git        "ファイルを開く(Gitベース)")))

(gssk-repeat-bind-ex-with-subcategory "Bookmark"
 '(("C-e C-t" bm-toggle   "現在行をブックマーク、ハイライト表示")
   ("C-e C-i" bm-next     "次のブックマークへ移動")
   ("C-e C-u" bm-previous "前のブックマークへ移動")
   ("C-e t"   bm-show     "ブックマークを表示")
   ("C-e M-t" bm-show-all "ブックマークを全て表示")))

;;; ---------------------------------------------------------------------------
;;; provide
;;; ---------------------------------------------------------------------------
(provide 'key-binding)
;;; key-binding.el ends here
