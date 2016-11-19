;;---------------------------------------------------------------------------
;;---------------------------------------------------------------------------
;;
;; Emacsインストールパッケージ(package-install要) / package-conf.el
;;
;;---------------------------------------------------------------------------
;;---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; パッケージマネージャ (package.el & use-package)
;; ---------------------------------------------------------------------------
(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(package-initialize)

;; ---------------------------------------------------------------------------
;; 初回起動時設定(package-refresh-contents & package-install大量)
;; ---------------------------------------------------------------------------
;; package-refresh-contentsは初回起動時1回のみ実行
;; 必要に応じて.emacs.dの以下のファイルを削除で、起動時にrefresh
;; ※ 起動毎の実行は重い
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

;;---------------------------------------------------------------------------
;; emacs server
;;---------------------------------------------------------------------------
;; terminalで emacsclient <file name>
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;; ---------------------------------------------------------------------------
;; 普通のredo
;;---------------------------------------------------------------------------
(use-package redo+
  :config
  (setq undo-no-redo t)
  (setq undo-limit 60000)
  (setq undo-strong-limit 90000))

;;---------------------------------------------------------------------------
;; visual regexp steroids : 正規表現の拡張
;;---------------------------------------------------------------------------
(use-package visual-regexp-steroids
  :config
  (setq vr/engine 'java))

;;---------------------------------------------------------------------------
;; mouse disable : マウス禁止
;;---------------------------------------------------------------------------
(use-package disable-mouse
  :config
  (global-disable-mouse-mode))

;;---------------------------------------------------------------------------
;; smooth scroll : スクロール滑川
;;---------------------------------------------------------------------------
(use-package smooth-scroll
  :config
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (smooth-scroll-mode t))

;;---------------------------------------------------------------------------
;; auto complete : 自動補完
;;---------------------------------------------------------------------------
(use-package auto-complete
  :config
  (ac-config-default)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict"))

;;---------------------------------------------------------------------------
;; ace jump mode : 任意の場所に3ストロークで移動
;;---------------------------------------------------------------------------
(use-package ace-jump-mode)

;;---------------------------------------------------------------------------
;; anzu : モードラインの右側に検索中の単語数を表示
;;---------------------------------------------------------------------------
(use-package anzu
  :config
  (global-anzu-mode t))

;;---------------------------------------------------------------------------
;; auto highlight symbol : カーソル位置のシンボルの自動ハイライト
;;---------------------------------------------------------------------------
(use-package auto-highlight-symbol
  :config
  (global-auto-highlight-symbol-mode t))

;;---------------------------------------------------------------------------
;; auto highlight symbol : カーソル位置のシンボルの自動ハイライト
;;---------------------------------------------------------------------------
(use-package auto-highlight-symbol
  :config
  (global-auto-highlight-symbol-mode t))

;;---------------------------------------------------------------------------
;; google this : ググる
;;---------------------------------------------------------------------------
;; key-bind : google-this

(use-package google-this
  :config
  (google-this-mode t)
  (setq google-this-location-suffix "co.jp")
  (global-auto-highlight-symbol-mode t)
  (defun google-this-url ()
    "URL for google searches."
    (concat google-this-base-url google-this-location-suffix
	    "/search?q=%s&hl=ja&num=100&as_qdr=y5&lr=lang_ja")))

;;---------------------------------------------------------------------------
;; auto highlight symbol : カーソル位置のシンボルの自動ハイライト
;;---------------------------------------------------------------------------
(use-package auto-highlight-symbol
  :config
  (global-auto-highlight-symbol-mode t))

;;---------------------------------------------------------------------------
;; provide
;;---------------------------------------------------------------------------
(provide 'package-conf)
