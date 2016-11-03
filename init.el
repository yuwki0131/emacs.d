;; ---------------------------------------------------------------------------
;; パッケージマネージャ (package.el & use-package)
;; ---------------------------------------------------------------------------
(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(package-initialize)

;; package-refresh-contentsは初回起動時1回のみ実行
;; 必要に応じて.emacs.dの以下のファイルを削除で、起動時にrefresh
;; ※起動毎の実行は重い
(defvar package-refresh-contents-lock
  "~/.emacs.d/.no-package-refresh-contents")

(when (not (file-exists-p package-refresh-contents-lock))
  (package-refresh-contents)
  (with-temp-buffer
    (insert (concat "package-refresh-contents\n last: " (current-time-string)))
    (write-file package-refresh-contents-lock)))

(package-install 'use-package)
(use-package magit
  :ensure t)

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

;; 折り返しを表示
(setq truncate-lines t)

;; file名の補完で大文字小文字を区別しない
(setq completion-ignore-case t)

;; バッファ自動再読み込み
(global-auto-revert-mode 1)

;; カーソルタイプ
(setq default-cursor-type '(bar . 2))

;; エンコーディング
(set-language-environment "Japanese")
(setq default-buffer-file-coding-system 'utf-8)

;; scratchの初期のメッセージ
(setq initial-scratch-message ";; hello world, emacs !!")

;; ---------------------------------------------------------------------------
;; local elisp files
;; ---------------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/dconf")

;; エディタの外観/サイズ調整
(require 'appearance-conf)

;; 標準機能(package-install不要)
(require 'standard-conf)

;; 非標準機能(要package-install)
(require 'nonstandard-conf)

;; 外部のpackage化されてない追加機能(package-install不要)
(require 'external-elisp)

;; 自作の追加機能(package-install不要)
(require 'internal-elisp)

;;---------------------------------------------------------------------------
;; prefix key <unset keys>
;; _ _ E _ _ _ U _ o _
;;  A _ _ _ _ _ _ _ _
;;   Z X C _ _ _ m

(global-unset-key "\C-e") ;; 移動系 prefix

(global-unset-key "\C-a") ;; 編集系 prefix

(global-unset-key "\C-z") ;; 機能系 prefix

(global-unset-key "\M-m") ;; (line topへ) いかない

(global-unset-key "\M-j") ;; (enter) しない

;;---------------------------------------------------------------------------
;; no prefix
(global-set-key (kbd "C-S-k") 'backward-kill-line)

;; delete action
(global-set-key "\C-h"   'delete-backward-char)
(global-set-key "\M-h"   'backward-kill-word)

;; move by paragraph
(global-set-key "\C-m" 'forward-paragraph)
(global-set-key "\M-m" 'backward-paragraph)

;; next / previous buffer with skip *
(global-set-key "\C-\M-f" 'next-buffer-with-skip*)
(global-set-key "\C-\M-p" 'previous-buffer-with-skip*)

;; undo & redo
(global-set-key "\C-q"   'undo)
(global-set-key "\M-q"   'redo)

;; カーソル位置固定のままスクロール
(global-set-key "\M-p"   'scroll-up-in-place)
(global-set-key "\M-n"   'scroll-down-in-place)

;;---------------------------------------------------------------------------
;; Z prefix (to work something)

;; キーバインドの表示
(global-set-key "\C-z\C-k" 'describe-bindings)

;; 手前の空白を削除 (delete until black key)
(global-set-key "\C-zp" 'toggle-truncate-lines)

;; 手前の空白を削除 (delete until black key)
(global-set-key "\C-z\C-d" 'delete-until-black)

;; 置換
(global-set-key "\C-z\C-r" 'replace-string)

;; vr/isearch側の正規表現置換
(global-set-key "\C-\M-s" 'vr/isearch-forward)
(global-set-key "\C-\M-r" 'vr/isearch-backward)

;; change encoding
(global-set-key "\C-zf"    'set-file-name-coding-system)

;;---------------------------------------------------------------------------
;; A prefix (to edit somewhat)
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

;; insert-time key
(global-set-key "\C-a\C-d" 'insert-time)

;; insert commment line
(global-set-key "\C-a\C-m" 'insert--s)

;; multi-comment-out-in keys
(global-set-key "\C-a\C-a" 'comment-dwim)

;;---------------------------------------------------------------------------
;; E prefix (to move somewhere)

;; 正規表現検索
(global-set-key "\C-e\C-s" 'search-forward-regexp)
(global-set-key "\C-e\C-r" 'search-backward-regexp)

;; デフォルトの行先頭後尾移動 Ctrl-e / Ctrl-a の再設定.
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

;; 括弧操作
(global-set-key [C-return]    'kill-until-corresp-paren)
(global-set-key "\C-l"        'insert-parenthesis)
(global-set-key (kbd "C-S-l") 'insert-angle-brackets)
(global-set-key "\M-l"        'insert-brackets)
(global-set-key (kbd "M-L")   'insert-squares)
