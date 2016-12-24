;;; ---------------------------------------------------------------------------
;;; ---------------------------------------------------------------------------
;;;
;;; Emacsインストールパッケージ(package-install要) / package-conf.el
;;;
;;; ---------------------------------------------------------------------------
;;; ---------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; emacs server
;;; ---------------------------------------------------------------------------
;; terminalで emacsclient <file name>
(use-package auto-compile
  :config
  (setq load-prefer-newer t)
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;;; ---------------------------------------------------------------------------
;;; 普通のredo
;;; ---------------------------------------------------------------------------
(use-package redo+
  :config
  (setq undo-no-redo t)
  (setq undo-limit 60000)
  (setq undo-strong-limit 90000))

;;; ---------------------------------------------------------------------------
;;; visual regexp steroids : 正規表現の拡張
;;; ---------------------------------------------------------------------------
(use-package visual-regexp-steroids
  :config
  (setq vr/engine 'java))

;;; ---------------------------------------------------------------------------
;;; swoop : トークンレベル移動
;;; ---------------------------------------------------------------------------
(use-package swoop)

;;; ---------------------------------------------------------------------------
;;; mouse disable : マウス禁止
;;; ---------------------------------------------------------------------------
(use-package disable-mouse
  :config
  (global-disable-mouse-mode))

;;; ---------------------------------------------------------------------------
;;; smooth scroll : スクロール滑川
;;; ---------------------------------------------------------------------------
(use-package smooth-scroll
  :config
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (smooth-scroll-mode t))

;;; ---------------------------------------------------------------------------
;;; auto complete : 自動補完
;;; ---------------------------------------------------------------------------
(use-package auto-complete
  :config
  (ac-config-default)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict"))


;;; ---------------------------------------------------------------------------
;;; highlight indent guides : インデントのハイライト
;;; ---------------------------------------------------------------------------
(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character)
  (set-face-background 'highlight-indent-guides-even-face "white")
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

;;; ---------------------------------------------------------------------------
;;; ace jump mode : 任意の場所に3ストロークで移動
;;; ---------------------------------------------------------------------------
(use-package ace-jump-mode)

;;; ---------------------------------------------------------------------------
;;; magit : emacs git client
;;; ---------------------------------------------------------------------------
(use-package magit
  :ensure t)

;;; ---------------------------------------------------------------------------
;;; anzu : モードラインの右側に検索中の単語数を表示
;;; ---------------------------------------------------------------------------
(use-package anzu
  :config
  (global-anzu-mode t))

;;; ---------------------------------------------------------------------------
;;; auto highlight symbol : カーソル位置のシンボルの自動ハイライト
;;; ---------------------------------------------------------------------------
(use-package auto-highlight-symbol
  :config
  (global-auto-highlight-symbol-mode t))

;;; ---------------------------------------------------------------------------
;;; auto highlight symbol : カーソル位置のシンボルの自動ハイライト
;;; ---------------------------------------------------------------------------
(use-package auto-highlight-symbol
  :config
  (global-auto-highlight-symbol-mode t))

;;; ---------------------------------------------------------------------------
;;; google this : ググる
;;; ---------------------------------------------------------------------------
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

;;; ---------------------------------------------------------------------------
;;; auto highlight symbol : カーソル位置のシンボルの自動ハイライト
;;; ---------------------------------------------------------------------------
(use-package auto-highlight-symbol
  :config
  (global-auto-highlight-symbol-mode t))

;;; ---------------------------------------------------------------------------
;;; w3m : w3m in emacs
;;; ---------------------------------------------------------------------------
(use-package w3m-load
  :config
  (setq w3m-command "w3m"))

;;; ---------------------------------------------------------------------------
;;; auto async byte compile : emacsのバイトコンパイルの自動化
;;; ---------------------------------------------------------------------------
(use-package auto-async-byte-compile)

;;; ---------------------------------------------------------------------------
;;; goto chg : 最後に変更した箇所へカーソルを移動
;;; ---------------------------------------------------------------------------
;; TODO : キーバインド未設定
(use-package goto-chg)

;;; ---------------------------------------------------------------------------
;;; migemo : isearchをローマ字のままで日本語も検索可能に
;;; ---------------------------------------------------------------------------
;; required : sudo apt-get install cmigemo
(use-package migemo
  :config
  (when (executable-find "cmigemo")
    (setq migemo-options '("-q" "--emacs"))
    (setq migemo-user-dictionary nil)
    (setq migemo-regex-dictionary nil)
    (setq migemo-coding-system 'utf-8-unix)
    (load-library "migemo")
    (migemo-init)
    (setq migemo-command "cmigemo")
    (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")))

;;; ---------------------------------------------------------------------------
;;; twittering mode : ついった
;;; ---------------------------------------------------------------------------
(use-package twittering-mode
  :config
  (setq twittering-use-master-password t))

;;; ---------------------------------------------------------------------------
;;; 絵文字用フォントセット
;;; ---------------------------------------------------------------------------
;; source : http://qiita.com/tadsan/items/a67b28dd02bf819f3f4e
(use-package emoji-fontset
  :config
  (emoji-fontset-enable "Symbola"))

;;; ---------------------------------------------------------------------------
;;; にゃーん
;;; ---------------------------------------------------------------------------
;; original : https://www.youtube.com/watch?v=QH2-TGUlwu4
(use-package nyan-mode
  :config
  (nyan-mode)
  (nyan-start-animation)
  (scroll-bar-mode -1))

;;; ---------------------------------------------------------------------------
;;; provide
;;; ---------------------------------------------------------------------------
(provide 'package-conf)
