;;; ---------------------------------------------------------------------------
;;; ---------------------------------------------------------------------------
;;;
;;; Emacsインストールパッケージ(package-install要) / package-conf.el
;;;
;;; ---------------------------------------------------------------------------
;;; ---------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; failed-packages report : use-packageに失敗したパッケージのレポート
;;; ---------------------------------------------------------------------------

(defvar failed-packages '())

(defmacro use-package-with-report (&rest body)
  `(when (not (use-package . ,body))
     (add-to-list 'failed-packages ,(symbol-name (car body)))))

(defun interpose (ls obj)
  (if (null ls)
      nil
    `(,(car ls) ,obj . ,(interpose (cdr ls) obj))))

(defun report-failed-packages ()
  (if (not failed-packages)
      "all defined packages installed successfully"
    (concat
     "failed packages: \n"
     (apply 'concat (interpose failed-packages "\n")))))

(font-lock-add-keywords 'emacs-lisp-mode
  '(("\\(use-package-with-report\\)" . font-lock-keyword-face)))

;;; ---------------------------------------------------------------------------
;;; emacs server
;;; ---------------------------------------------------------------------------
;; terminalで emacsclient <file name>
;; (use-package-with-report auto-compile
;;   :config
;;   (setq load-prefer-newer t)
;;   (auto-compile-on-load-mode)
;;   (auto-compile-on-save-mode))

;;; ---------------------------------------------------------------------------
;;; 普通のredo
;;; ---------------------------------------------------------------------------
(use-package-with-report redo+
  :config
  (setq undo-no-redo t)
  (setq undo-limit 60000)
  (setq undo-strong-limit 90000))

;;; ---------------------------------------------------------------------------
;;; visual regexp steroids : 正規表現の拡張
;;; ---------------------------------------------------------------------------
;; (use-package-with-report visual-regexp-steroids
;;   :config
;;   (setq vr/engine 'java))

;;; ---------------------------------------------------------------------------
;;; highlight numbers : 数値のハイライト
;;; ---------------------------------------------------------------------------
(use-package-with-report highlight-numbers
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;;; ---------------------------------------------------------------------------
;;; highlight operators : 演算子のハイライト
;;; ---------------------------------------------------------------------------
(use-package-with-report highlight-operators
  :config
  (add-hook 'python-mode-hook 'highlight-operators-mode))

;;; ---------------------------------------------------------------------------
;;; highlight line plus : カーソル行ハイライト(拡張)
;;; ---------------------------------------------------------------------------
(use-package-with-report hl-line+
  :config
  (toggle-hl-line-when-idle)
  (setq hl-line-idle-interval 3))

;;; ---------------------------------------------------------------------------
;;; highlight current-buffer : 現在のバッファをハイライト
;;; ---------------------------------------------------------------------------
(use-package-with-report hiwin
  :config
  (hiwin-activate)
  (set-face-background 'hiwin-face "gray10"))

;;; ---------------------------------------------------------------------------
;;; highlight indent guides : インデントのハイライト
;;; ---------------------------------------------------------------------------
(use-package-with-report highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character)
  (set-face-background 'highlight-indent-guides-even-face "white")
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

;;; ---------------------------------------------------------------------------
;;; volatile highlights : 修正箇所のハイライト
;;; ---------------------------------------------------------------------------
(use-package-with-report volatile-highlights
  :config
  (volatile-highlights-mode t))

;;; ---------------------------------------------------------------------------
;;; nurumacs : sublime風アウトライン表示
;;; ---------------------------------------------------------------------------
(use-package-with-report nurumacs)

;;; ---------------------------------------------------------------------------
;;; helm : helm
;;; ---------------------------------------------------------------------------
(use-package-with-report helm
  :config
  (require 'helm-config)
  (helm-mode 1))

;;; ---------------------------------------------------------------------------
;;; swoop : トークンレベル移動(検索系)
;;; ---------------------------------------------------------------------------
(use-package-with-report swoop)

;;; ---------------------------------------------------------------------------
;;; mouse disable : マウス禁止
;;; ---------------------------------------------------------------------------
(use-package-with-report disable-mouse
  :config
  (global-disable-mouse-mode))

;;; ---------------------------------------------------------------------------
;;; smooth scroll : スクロール滑川
;;; ---------------------------------------------------------------------------
;; (use-package-with-report smooth-scroll
;;   :config
;;   (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;;   (smooth-scroll-mode t))

;;; ---------------------------------------------------------------------------
;;; auto complete : 自動補完
;;; ---------------------------------------------------------------------------
(use-package-with-report auto-complete
  :config
  (ac-config-default)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict"))

;;; ---------------------------------------------------------------------------
;;; ace jump mode : 任意の場所に3ストロークで移動
;;; ---------------------------------------------------------------------------
;; (use-package-with-report ace-jump-mode)

;;; ---------------------------------------------------------------------------
;;; anzu : モードラインの左側に検索中の単語数を表示
;;; ---------------------------------------------------------------------------
(use-package-with-report anzu
  :config
  (global-anzu-mode t))

;;; ---------------------------------------------------------------------------
;;; highlight symbol : カーソル位置のシンボルの自動ハイライト
;;; ---------------------------------------------------------------------------
(use-package-with-report highlight-symbol
  :config
  (add-hook 'prog-mode-hook 'highlight-symbol-mode))

;;; ---------------------------------------------------------------------------
;;; google this : ググる
;;; ---------------------------------------------------------------------------
;; key-bind : google-this

(use-package-with-report google-this
  :config
  (google-this-mode t)
  (setq google-this-location-suffix "co.jp")
  (global-auto-highlight-symbol-mode t)
  (defun google-this-url ()
    "URL for google searches."
    (concat google-this-base-url google-this-location-suffix
	    "/search?q=%s&hl=ja&num=100&as_qdr=y5&lr=lang_ja")))

;;; ---------------------------------------------------------------------------
;;; w3m : w3m in emacs
;;; ---------------------------------------------------------------------------
(use-package-with-report w3m-load
  :config
  (setq w3m-command "w3m"))

;;; ---------------------------------------------------------------------------
;;; auto async byte compile : emacsのバイトコンパイルの自動化
;;; ---------------------------------------------------------------------------
(use-package-with-report auto-async-byte-compile)

;;; ---------------------------------------------------------------------------
;;; goto chg : 最後に変更した箇所へカーソルを移動
;;; ---------------------------------------------------------------------------
;; TODO : キーバインド未設定
(use-package-with-report goto-chg)

;;; ---------------------------------------------------------------------------
;;; migemo : isearchをローマ字のままで日本語も検索可能に
;;; ---------------------------------------------------------------------------
;; required : sudo apt-get install cmigemo
(use-package-with-report migemo
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
;;; exec path from shell : シェルのpath設定引き継ぎ
;;; ---------------------------------------------------------------------------
(use-package-with-report exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;;; ---------------------------------------------------------------------------
;;; twittering mode : ついった
;;; ---------------------------------------------------------------------------
(use-package-with-report twittering-mode
  :config
  (setq twittering-use-master-password t))

;;; ---------------------------------------------------------------------------
;;; 絵文字用フォントセット
;;; ---------------------------------------------------------------------------
;; source : http://qiita.com/tadsan/items/a67b28dd02bf819f3f4e
(use-package-with-report emoji-fontset
  :config
  (emoji-fontset-enable "Symbola"))

;;; ---------------------------------------------------------------------------
;;; にゃーん
;;; ---------------------------------------------------------------------------
;; original : https://www.youtube.com/watch?v=QH2-TGUlwu4
(use-package-with-report nyan-mode
  :config
  (nyan-mode)
  (nyan-start-animation)
  (scroll-bar-mode -1))

;;; ---------------------------------------------------------------------------
;;; provide
;;; ---------------------------------------------------------------------------
(provide 'package-conf)
