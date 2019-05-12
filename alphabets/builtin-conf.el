;;; package --- builtin-conf.el
;;; Commentary:
;;;  デフォルト機能の諸設定 / bizz-conf.el
;;; Code:

;; ツールバー表示設定
(tool-bar-mode -1)

;; メニューバー表示設定 (Linux Mint/Cinnamonだと綺麗な表示)
(menu-bar-mode -1)

;; スクロールバー表示設定
(scroll-bar-mode -1)

;; 画像ファイル表示
(auto-image-file-mode t)

;; 列数を表示
; (column-number-mode t)

;; バッファ自動再読み込み
(global-auto-revert-mode t)

;; 直前のウィンドウ構成に戻せるようにするための設定
(winner-mode 1)

(setq
 ;; scratchの初期のメッセージ消去
 initial-scratch-message nil

 ;; スタートメッセージを非表示
 inhibit-startup-message t

 ;; backupfile(*.~) つくらない
 make-backup-files nil

 ;; backupfile(*.# つくらない)
 auto-save-default nil

 ;; 編集中ファイルの自動backup
 backup-inhibited nil

 ;; 終了時に自動バックアップファイルを削除
 delete-auto-save-files nil

 ;; 編集中ファイルのバックアップ
 auto-save-list-file-name nil
 auto-save-list-file-prefix nil

 ;; ロックファイルの生成を抑止
 create-lockfiles nil

 ;; 複数保存しない
 kill-do-not-save-duplicates t

 ;; 折り返しを表示
 truncate-lines t

 ;; 折り返しを表示(ウインドウ分割時)
 truncate-partial-width-windows nil

 ;; file名の補完で大文字小文字を区別しない
 completion-ignore-case t

 ;; ファイル読込補完、大文字/小文字無視
 read-file-name-completion-ignore-case t

 ;; 最終行に1行挿入
 require-final-newline t

 ;; 再帰的ミニバッファ
 enable-recursive-minibuffers t

 ;; デフォルト起動時の画面非表示
 inhibit-startup-message t

 ;; 規則文字のdisable
 enable-kinsoku nil

 ;; elcとelで新しい方のファイルを読み込む(byte compile忘れ対策)
 load-prefer-newer t

 ;; 巨大なファイルを開く時に警告しない
 large-file-warning-threshold nil

 ;; Do not ask for confirm when killing processes
 confirm-kill-processes nil

 ;; スクロール時のカーソル位置を維持
 scroll-preserve-screen-position t

 ;; Make Tab complete if the line is indented
 tab-always-indent 'complete

 ;; kill ringについて
 kill-ring-max 200

 ;; killringでkillされる前にclipboardにコピーする
 save-interprogram-paste-before-kill t
 )

;; バッファの終端を表示(空行表示)
(setq-default indicate-empty-lines t)

;; インデントでタブを挿入しない
(setq-default indent-tabs-mode nil)

;; デフォルトインデント幅: 2
(setq standard-indent 2)

;; 再帰的ミニバッファの深さを表示
(minibuffer-depth-indicate-mode 1)

;;; 画像ファイル表示
(auto-image-file-mode t)

;; from yes-or-no to y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; disable
(recentf-mode nil)

;; ファイル保存時に空白削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; すべてのプログラムモードに対して
(add-hook
 'prog-mode-hook
 '(lambda ()
    (setq
     ;;タブ幅設定
     tab-width 2
     ;; 行番号を表示(標準)
     display-line-numbers-width 4)
    (setq display-line-numbers t)))

;; ダイアログボックス非表示
(defalias 'message-box 'message)
(setq use-dialog-box nil)

;; URL強調表示、URLをブラウザで表示
(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(add-hook 'text-mode-hook 'goto-address-mode)

;; 大文字・小文字を区別しないで検索
(setq-default case-fold-search nil)

;; ‘isearch-word’ is an obsolete variable (as of 25.1)対策
(defvar search-default-regexp-mode nil)

;;; ---------------------------------------------------------------------------
;;; デフォルトエンコーディング
;;; ---------------------------------------------------------------------------
(set-language-environment "Japanese")
(set-terminal-coding-system   'utf-8)
(prefer-coding-system         'utf-8)
(setq coding-system-for-read  'utf-8
      coding-system-for-write 'utf-8
      default-process-coding-system '(utf-8 . utf-8))
(setenv "LANG" "ja_JP.UTF-8")

;;; ---------------------------------------------------------------------------
;;; 別ディレクトリの同名バッファにディレクトリ名を付与する
;;; ---------------------------------------------------------------------------
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;; ---------------------------------------------------------------------------
;;; 特定のバッファではlinum-modeをoff
;;; ---------------------------------------------------------------------------
(defvar disable-nlinum-hooks
  '(shell-mode-hook
    eshell-mode-hook emacs-startup-hook grep-mode-hook
    swoop-mode-hook term-mode-hook imenu-list-major-mode-hook
    imenu-list-minor-mode-hook minimap-mode-hook
    ))

(while (not (null disable-nlinum-hooks))
  (add-hook (car disable-nlinum-hooks)
            (lambda () (setq display-line-numbers nil)))
  (setq disable-nlinum-hooks (cdr disable-nlinum-hooks)))

;;; ---------------------------------------------------------------------------
;;; provide
;;; ---------------------------------------------------------------------------
(provide 'builtin-conf)
;;; builtin-conf.el ends here
