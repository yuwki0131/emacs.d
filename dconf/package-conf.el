;;; package-conf.el --- packages
;;; Commentary:
;;;  Emacsインストールパッケージ(package-install要) / package-conf.el
;;;  package func : 機能
;;;  package edit : 編集
;;;  package app : 外観
;;;  package search : 検索
;;;  package config : 設定
;;;  package report : 報告・計測
;;; Code:

;;; ---------------------------------------------------------------------------
;;; failed-packages report : use-packageに失敗したパッケージのレポート
;;; ---------------------------------------------------------------------------
(defvar failed-packages '())

(defmacro use-package-with-report (&rest body)
  `(when (not (use-package . ,(append body '(:config 't))))
     (add-to-list 'failed-packages ,(symbol-name (car body)))))

(defun interpose (ls obj)
  (if (null ls)
      nil
    `(,(car ls) ,obj . ,(interpose (cdr ls) obj))))

(defun add-semicolon (line)
  (concat ";; " line))

(defun report-failed-packages ()
  (if (not failed-packages)
      ";; all defined packages have been installed successfully"
    (concat
     ";; failed packages: \n"
     (apply 'concat
	    (interpose (mapcar #'add-semicolon failed-packages) "\n")))))

(font-lock-add-keywords 'emacs-lisp-mode
  '(("\\(use-package-with-report\\)" . font-lock-keyword-face)))

;;; ---------------------------------------------------------------------------
;;; package-func : magit : emacs git client
;;; ---------------------------------------------------------------------------
(use-package magit
  :ensure t)

;;; ---------------------------------------------------------------------------
;;; package-func : auto compile : elファイル自動コンパイル
;;; ---------------------------------------------------------------------------
(use-package-with-report auto-compile
  :config
  (setq load-prefer-newer t)
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;;; ---------------------------------------------------------------------------
;;; package-func : auto complete : 自動補完
;;; ---------------------------------------------------------------------------
(use-package-with-report auto-complete
  :config
  (ac-config-default)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict"))

;;; ---------------------------------------------------------------------------
;;; package-func : google this : ググる
;;; ---------------------------------------------------------------------------
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
;;; package-func : w3m : w3m in emacs
;;; ---------------------------------------------------------------------------
(use-package-with-report w3m
  :config
  (setq w3m-command "w3m"))

;;; ---------------------------------------------------------------------------
;;; package-func : auto async byte compile : emacsのバイトコンパイルの自動化
;;; ---------------------------------------------------------------------------
(use-package-with-report auto-async-byte-compile)

;;; ---------------------------------------------------------------------------
;;; package-func : tiny menu : tiny menu
;;; ---------------------------------------------------------------------------
(use-package-with-report tiny-menu)

;;; ---------------------------------------------------------------------------
;;; package-func : open junk file : ジャンクファイル生成
;;; ---------------------------------------------------------------------------
(use-package-with-report open-junk-file
  :config
  (setq open-junk-file-format "~/.emacs.d/junk/%Y-%m-%d-%H%M%S."))

;;; ---------------------------------------------------------------------------
;;; package-func : twittering mode : ついった
;;; ---------------------------------------------------------------------------
(use-package-with-report twittering-mode
  :config
  (setq twittering-use-master-password t))

;;; ---------------------------------------------------------------------------
;;; package-move : goto chg : 最後に変更した箇所へカーソルを移動
;;; ---------------------------------------------------------------------------
(use-package-with-report goto-chg)

;;; ---------------------------------------------------------------------------
;;; package-move : point-undo : カーソル位置を戻す
;;; ---------------------------------------------------------------------------
(use-package-with-report point-undo)

;;; ---------------------------------------------------------------------------
;;; package-edit : redo+ : 普通のredo
;;; ---------------------------------------------------------------------------
(use-package-with-report redo+
  :config
  (setq undo-no-redo t)
  (setq undo-limit 60000)
  (setq undo-strong-limit 90000))

;;; ---------------------------------------------------------------------------
;;; package-edit : hungry-delete-mode : 空白の貪欲な削除
;;; ---------------------------------------------------------------------------
(use-package-with-report hungry-delete-mode
  :config
  (global-hungry-delete-mode t))

;;; ---------------------------------------------------------------------------
;;; package-app : bm : 現在行を永続的に記憶
;;; ---------------------------------------------------------------------------
(use-package-with-report bm)

;;; ---------------------------------------------------------------------------
;;; package-app : fancy-narrow : 現在の領域をnarrowingする
;;; ---------------------------------------------------------------------------
(use-package-with-report fancy-narrow
  :config
  (fancy-narrow-mode 1))

;;; ---------------------------------------------------------------------------
;;; package-app : にゃーん
;;; ---------------------------------------------------------------------------
;; original : https://www.youtube.com/watch?v=QH2-TGUlwu4
(use-package-with-report nyan-mode
  :config
  (nyan-mode)
  (nyan-start-animation))

;;; ---------------------------------------------------------------------------
;;; package-app : nlinum-mode : 軽量化された行番号表示
;;; ---------------------------------------------------------------------------
(use-package-with-report nlinum
  :config
  (global-nlinum-mode t)
  (setq nlinum-format " %4d "))

;;; ---------------------------------------------------------------------------
;;; package-app : highlight line plus : カーソル行ハイライト(拡張)
;;; ---------------------------------------------------------------------------
(use-package-with-report hl-line+
  :config
  (toggle-hl-line-when-idle)
  (setq hl-line-idle-interval 3))

;;; ---------------------------------------------------------------------------
;;; package-app : highlight current-buffer : 現在のバッファをハイライト
;;; ---------------------------------------------------------------------------
'(use-package-with-report hiwin
   :config
  (hiwin-activate)
  (set-face-background 'hiwin-face "gray10"))

;;; ---------------------------------------------------------------------------
;;; package-app : highlight indent guides : インデント表示
;;; ---------------------------------------------------------------------------
(use-package-with-report highlight-indent-guides
  :config
  (set-face-background 'highlight-indent-guides-even-face "gray20"))

;;; ---------------------------------------------------------------------------
;;; package-app : volatile highlights : 修正箇所のハイライト
;;; ---------------------------------------------------------------------------
(use-package-with-report volatile-highlights
  :config
  (volatile-highlights-mode t))

;;; ---------------------------------------------------------------------------
;;; package-app : highlight symbol : カーソル位置のシンボルの自動ハイライト
;;; ---------------------------------------------------------------------------
(use-package-with-report highlight-symbol
  :config
  (add-hook 'prog-mode-hook 'highlight-symbol-mode))

;;; ---------------------------------------------------------------------------
;;; package-app : rainbow delimiters : 括弧色つけ
;;; ---------------------------------------------------------------------------
(use-package-with-report rainbow-delimiters)

;;; ---------------------------------------------------------------------------
;;; package-app : anzu : モードラインの左側に検索中の単語数を表示
;;; ---------------------------------------------------------------------------
(use-package-with-report anzu
  :config
  (global-anzu-mode t))

;;; ---------------------------------------------------------------------------
;;; package-app : nurumacs : sublime風アウトライン表示
;;; ---------------------------------------------------------------------------
(use-package-with-report nurumacs)

;;; ---------------------------------------------------------------------------
;;; package-search : visual regexp steroids : 正規表現の拡張
;;; ---------------------------------------------------------------------------
(use-package-with-report visual-regexp-steroids
  :config
  (setq vr/engine 'java))

;;; ---------------------------------------------------------------------------
;;; package-search : helm : helm
;;; ---------------------------------------------------------------------------
(use-package-with-report helm
   :config
  (require 'helm-config)
  (helm-mode 1))

;;; ---------------------------------------------------------------------------
;;; package-search : swoop : トークンレベル移動(検索系)
;;; ---------------------------------------------------------------------------
(use-package-with-report swoop)

;;; ---------------------------------------------------------------------------
;;; package-search : ace jump mode : 任意の場所に3ストロークで移動
;;; ---------------------------------------------------------------------------
(use-package-with-report ace-jump-mode)

;;; ---------------------------------------------------------------------------
;;; package-search : migemo : isearchをローマ字のままで日本語も検索可能に
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
;;; package-config : mouse disable : マウス禁止
;;; ---------------------------------------------------------------------------
(use-package-with-report disable-mouse
  :config
  (global-disable-mouse-mode))

;;; ---------------------------------------------------------------------------
;;; package-config : exec path from shell : シェルのpath設定引き継ぎ
;;; ---------------------------------------------------------------------------
(use-package-with-report exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;;; ---------------------------------------------------------------------------
;;; package-config : 絵文字用フォントセット
;;; ---------------------------------------------------------------------------
;; source : http://qiita.com/tadsan/items/a67b28dd02bf819f3f4e
(use-package-with-report emoji-fontset
  :config
  (emoji-fontset-enable "Symbola"))

;;; ---------------------------------------------------------------------------
;;; package-report : esup : emacs起動時間の計測
;;; ---------------------------------------------------------------------------
(use-package-with-report esup)

;;; ---------------------------------------------------------------------------
;;; package-report : key freq mode : keyfreq
;;; ---------------------------------------------------------------------------
(use-package-with-report keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;;; ---------------------------------------------------------------------------
;;; provide
;;; ---------------------------------------------------------------------------
(provide 'package-conf)
;;; package-conf.el ends here
