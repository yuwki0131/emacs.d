;;; package --- emacs init.el
;;; Commentary:
;;;  Emacs init.el root
;;; Code:

;;; ---------------------------------------------------------------------------
;;; パッケージマネージャ (package.el & use-package)
;;; ---------------------------------------------------------------------------
(require 'package)

(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
		("melpa" . "http://melpa.milkbox.net/packages/")
        ("org"   . "http://orgmode.org/elpa/")))

(package-initialize)

;;; ---------------------------------------------------------------------------
;;; use package : emacs パッケージ管理
;;; ---------------------------------------------------------------------------
;; usepackageが存在しない場合は、インストール
(when (not (require 'use-package nil 'noerror))
  (package-install 'use-package))
;; 以降use-packageでインストール

;;; ---------------------------------------------------------------------------
;;; local elisp files
;;; ---------------------------------------------------------------------------

;; 設定ファイルのディレクトリ
(add-to-list 'load-path "~/.emacs.d/config")

;; utils (※use-package-with-report系より先頭に持ってくること)
(use-package config-utils)

;; 外部パッケージの設定
(use-package package-conf)

;; 雑多な設定
(use-package bizz-conf)

;; エディタの外観/サイズ調整
(use-package appearance-conf)

;; 言語共通設定(要package-install)
(use-package common-lang-conf)

;; 言語設定(要package-install)
(use-package language-conf)

;; 外部のpackage化されてない追加機能(package-install不要)
(use-package external-elisp)

;; 自作の追加機能(package-install不要)
(use-package internal-elisp)

;; キーバインド設定(package-install不要)
(use-package key-binding)

;;; ---------------------------------------------------------------------------
;;; reports
;;; ---------------------------------------------------------------------------
;;; configuration report
(report-configuration)

;;; generate readme
(spit readme-file-md (generate-readme-text))

;;; failed packages scinario
(generate-package-install-scinario)

;;; ---------------------------------------------------------------------------
;;; temp (playground)
;;; ---------------------------------------------------------------------------

(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-python-pyflakes-extra-arguments (quote ("--max-line-length=120" "--ignore=E128")))
 '(package-selected-packages
   (quote
    (wgrep zop-to-char volatile-highlights visual-regexp-steroids vimrc-mode use-package twittering-mode tiny-menu swoop sunshine smooth-scroll sml-mode smart-mode-line redo+ rainbow-delimiters racket-mode quickrun python-pep8 python-mode pylint pyflakes py-autopep8 powerline point-undo path-headerline-mode origami open-junk-file nyan-mode nurumacs nlinum neotree mozc-im migemo markdown-preview-mode mark-multiple magit lua-mode keyfreq jedi ipython initchart iedit hy-mode hungry-delete hlinum hl-line+ hiwin highlight-symbol highlight-sexp highlight-quoted highlight-operators highlight-numbers highlight-indent-guides helm-swoop helm-ag guide-key goto-chg google-translate google-this geiser free-keys flymake-python-pyflakes flymake-cursor flymake-checkers flycheck-pyflakes flycheck-mypy flycheck-haskell flycheck-gdc-dub flycheck-gdc flycheck-clojure fancy-narrow exec-path-from-shell esup emojify emoji-fontset emoji-display elpy echo-bell e2wm drag-stuff disable-mouse dic-lookup-w3m dashboard dash-functional col-highlight clj-refactor browse-kill-ring bm beacon auto-highlight-symbol auto-complete-clang-async auto-compile auto-async-byte-compile anzu ace-jump-mode ac-slime ac-python ac-cider))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(col-highlight ((t (:inherit hl-line)))))
