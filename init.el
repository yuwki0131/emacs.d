;;; package --- emacs init.el
;;; Commentary:
;;;  Emacs init.el root
;;; Code:

;;; ---------------------------------------------------------------------------
;;; パッケージマネージャ (package.el & use-package)
;;; ---------------------------------------------------------------------------
(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("elpa-gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(package-initialize)

;;; ---------------------------------------------------------------------------
;;; use package : emacs パッケージ管理
;;; ---------------------------------------------------------------------------
;; usepackageが存在しない場合は、インストール
(when (not (require 'use-package nil 'noerror))
  (package-initialize)
  (package-refresh-contents)
  (package-install 'use-package)
  (setq use-package-verbose t))
;; 以降、use-packageでrequire

;;; ---------------------------------------------------------------------------
;;; set load path
;;; ---------------------------------------------------------------------------

;; 設定ファイルのディレクトリ
(add-to-list 'load-path "~/.emacs.d/config")

;; 外部設定ファイルのディレクトリ
(add-to-list 'load-path "~/.emacs.d/wrepo")
(add-to-list 'load-path "~/.emacs.d/gitrepo")

;;; ---------------------------------------------------------------------------
;;; local elisp files
;;; ---------------------------------------------------------------------------

;; utils (※use-package-with-report系より先頭に持ってくること)
(use-package util-elisp)
;; 以降、config下では、use-package-with-reportでrequire

;; 外部パッケージの設定(要package-install)
(use-package package-conf)

;; 編集系パッケージの設定(要package-install)
(use-package edit-package-conf)

;; 表示系パッケージの設定(要package-install)
(use-package display-package-conf)

;; 検索系パッケージの設定(要package-install)
(use-package search-package-conf)

;; 移動系パッケージの設定(要package-install)
(use-package jump-package-conf)

;; 雑多な設定
(use-package builtin-conf)

;; フォント設定
(use-package font-conf)

;; エディタの外観/サイズ調整
(use-package appearance-conf)

;; モードライン設定
(use-package modeline-conf)

;; 言語共通設定(要package-install)
(use-package common-lang-conf)

;; 言語設定(要package-install)
(use-package language-conf)

;; 外部のpackage化されてない追加機能(package-install不要)
(use-package outsider-elisp)

;; 雑多な追加機能(package-install不要)
(use-package tips-elisp)

;; キーバインド設定(package-install不要)
(use-package key-binding)

;;; ---------------------------------------------------------------------------
;;; exwm
;;; ---------------------------------------------------------------------------
;;; exwm

;; (require 'exwm)
;; (require 'exwm-config)
;; (exwm-config-default)
;; (setq exwm-workspace-show-all-buffers t)
;; (setq exwm-layout-show-all-buffers t)

;; (require 'exwm-randr)
;; (setq exwm-randr-workspace-output-plist '(0 "VGA1"))
;; (add-hook 'exwm-randr-screen-change-hook
;;           (lambda ()
;;             (start-process-shell-command
;;              "xrandr" nil "xrandr --output VGA1 --left-of LVDS1 --auto")))
;; (exwm-randr-enable)

;; on your magic
;; setxkbmap -option ctrl:swapcaps

;;; ---------------------------------------------------------------------------
;;; reports
;;; ---------------------------------------------------------------------------
;;; 設定レポート
(report-configuration)

;;; README.md生成 (標準ではOFF)
(when t
  (spit readme-file-md (generate-readme-text)))

;;; failed packages scenario
(generate-package-install-scenario)

;;; ignored errors
(report-ignore&report)

;;; ---------------------------------------------------------------------------
;;; custom set vaiables抑止
;;; ---------------------------------------------------------------------------
(setq custom-file (locate-user-emacs-file "custom.el"))

;;; ---------------------------------------------------------------------------
;;; temp (playground)
;;; ---------------------------------------------------------------------------
(provide 'init)
;;; init.el ends here
