;;---------------------------------------------------------------------------
;;---------------------------------------------------------------------------
;;
;; デフォルト機能の諸設定 / bizz-conf.el
;;
;;---------------------------------------------------------------------------
;;---------------------------------------------------------------------------

;; 起動時の画面は、いらない
(setq inhibit-startup-message t)

;; ツールバーは、いらない
(tool-bar-mode -1)

;; メニューバーは、いらない
(menu-bar-mode -1)

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

;; 列数を表示
(column-number-mode t)

;; 行番号フォーマット
(setq linum-format "%4d ")

;; 折り返しを表示
(setq truncate-lines t)

;; file名の補完で大文字小文字を区別しない
(setq completion-ignore-case t)

;; バッファ自動再読み込み
(global-auto-revert-mode 1)

;; カーソルタイプ
(setq default-cursor-type '(bar . 2))

;; エンコーディング
(prefer-coding-system 'utf-8-unix)

;; scratchの初期のメッセージ
(setq initial-scratch-message ";; hello world, emacs !!\n;; (´･_･`)\n")

;; yes-or-no to y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; 画像ファイル表示
(auto-image-file-mode t)

;; 最終行に1行挿入
(setq require-final-newline t)

;; ファイル保存時に空白削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ---------------------------------------------------------------------------
;; provide
;; ---------------------------------------------------------------------------
(provide 'bizz-conf)
