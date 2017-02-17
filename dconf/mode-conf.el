;;; package-conf.el --- packages
;;; Commentary:
;;;  各モードの設定
;;; Code:

;;; ---------------------------------------------------------------------------
;;; dired-mode config
;;; ---------------------------------------------------------------------------

(defun dired-mode-config ()
  ;; dired-mode時もバッファ切替をC-tでできるようにする
  (define-key dired-mode-map "\C-t" 'other-window)
  ;; 自在に名前変更するモード
  (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
  ;; dired のキー割り当て追加。zキーを押すと、
  ;; Macに関連付けられたアプリケーションでファイルを開けるようにする。
  (define-key dired-mode-map "z" 'dired-open-mac)
  ;; dired のキー割り当て追加。_キーを押すと、
  ;; Terminalでディレクトリまでcdして開けるようにする。
  (define-key dired-mode-map "_" 'dired-open-mac-terminal)
  ;; ファイル作成
  (define-key dired-mode-map "c" 'dired-create-file))

(add-hook 'dired-mode-hook dired-mode-config)
