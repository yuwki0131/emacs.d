;;; package --- emacs init.el
;;; Commentary:
;;;  Emacs init.el root
;;; Code:

;;; ---------------------------------------------------------------------------
;;; パッケージマネージャ (package.el & use-package)
;;; ---------------------------------------------------------------------------
(require 'package)

(setq package-archives
      '(("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("marmalade"    . "http://marmalade-repo.org/packages/")
        ("elpa-gnu"     . "http://elpa.gnu.org/packages/")
        ("org"          . "http://orgmode.org/elpa/")))

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
(add-to-list 'load-path "~/.emacs.d/alphabets")

;; 外部設定ファイルのディレクトリ
(add-to-list 'load-path "~/.emacs.d/wrepo")
(add-to-list 'load-path "~/.emacs.d/gitrepo")

;;; ---------------------------------------------------------------------------
;;; local elisp files
;;; ---------------------------------------------------------------------------

;; utils (※use-package-with-report系より先頭に持ってくること)
(require 'util-elisp)
;; 以降、alphabets下では、use-package-with-reportでrequire

;; 雑多な設定
(require 'builtin-conf)

;; 雑多な追加機能(package-install不要)
(require 'tips-elisp)

;; その他パッケージの設定(要package-install)
(require 'rest-packages)

;; 編集系パッケージの設定(要package-install)
(require 'edit-packages)

;; 表示(オブジェクト)系パッケージの設定(要package-install)
(require 'display-packages)

;; 表示(ハイライト)系パッケージの設定(要package-install)
(require 'highlight-packages)

;; 検索系パッケージの設定(要package-install)
(require 'search-packages)

;; 検索移動(grep)系パッケージの設定(要package-install)
(require 'grep-packages)

;; 移動系パッケージの設定(要package-install)
(require 'jump-packages)

;; 補完系パッケージの設定
(require 'complement-packages)

;; 言語設定(要package-install)
(require 'language-conf)

;; 外部のpackage化されてない追加機能(package-install不要)
(require 'outsider-elisp)

;; フォント設定
(require 'font-conf)

;; エディタの外観/サイズ調整
(require 'appearance-conf)

;; キーバインド設定(package-install不要)
(require 'key-binding)

;; emacs用のWindowsManager設定
;; (require 'wm-conf)

;;; ---------------------------------------------------------------------------
;;; reports
;;; ---------------------------------------------------------------------------
;;; 設定レポート
(report-configuration)

;;; README.md生成 (標準ではOFF)
(when t (spit readme-file-md (generate-readme-text)))

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
