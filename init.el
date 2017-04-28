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
