;;; package --- bizz-conf.el
;;; Commentary:
;;;  デフォルト機能の諸設定 / bizz-conf.el
;;; Code:

;; デフォルト起動時の画面非表示
(setq inhibit-startup-message t)

;; ツールバー非表示
(tool-bar-mode -1)

;; メニューバー非表示
(menu-bar-mode -1)

;; スクロールバー非表示
(scroll-bar-mode -1)

;; beep音消す
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; backupfileつくらない
(setq make-backup-files nil)
(setq auto-save-default nil)

;; 折り返しを表示
(setq truncate-lines t)

;; 行番号を表示(標準) => 使用しない
;; (global-linum-mode t)

;; 列数を表示
(column-number-mode t)

;; file名の補完で大文字小文字を区別しない
(setq completion-ignore-case t)

;; バッファ自動再読み込み
(global-auto-revert-mode t)

;; バッファの終端を表示(空行表示)
(setq-default indicate-empty-lines t)

;; カーソルタイプ
(setq default-cursor-type '(bar . 3))

;; scratchの初期のメッセージ消去
(setq initial-scratch-message nil)

;; from yes-or-no to y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; 画像ファイル表示
(auto-image-file-mode t)

;; 最終行に1行挿入
(setq require-final-newline t)

;; ファイル保存時に空白削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; GC閾値 (what the fuck!)
;; (setq gc-cons-threshold (* 10 1024 1024))

;; GCが発生した場合にレポート
;; (setq garbage-collection-messages t)

;; ctrl+g時にbackgrace 最近emacsが何かと重いので(^_^;)
;; (setq debug-on-quit t)

;;; ---------------------------------------------------------------------------
;;; standard requires
;;; ---------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; 別ディレクトリの同名バッファにディレクトリ名を付与する
;;; ---------------------------------------------------------------------------
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;; ---------------------------------------------------------------------------
;;; provide
;;; ---------------------------------------------------------------------------
(provide 'bizz-conf)
;;; bizz-conf.el ends here
