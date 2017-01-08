;;;; ---------------------------------------------------------------------------
;;;; ---------------------------------------------------------------------------
;;;;
;;;; emacs init.el
;;;;
;;;; ---------------------------------------------------------------------------
;;;; ---------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; パッケージマネージャ (package.el & use-package)
;;; ---------------------------------------------------------------------------
(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("org" . "http://orgmode.org/elpa/")
	("melpa" . "http://melpa.milkbox.net/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)

;;; ---------------------------------------------------------------------------
;;; use package : emacs パッケージ管理
;;; ---------------------------------------------------------------------------
;; 以降use-packageでインストール
(require 'use-package)

;;; ---------------------------------------------------------------------------
;;; 初回起動時設定(package-refresh-contents & package-install大量)
;;; ---------------------------------------------------------------------------
;; package-refresh-contentsは初回起動時1回のみ実行
;; 必要に応じて.emacs.dの以下のファイルを削除で、起動時にrefresh
;; ※ 起動毎の実行は重い
(defvar package-refresh-contents-lock
  "~/.emacs.d/.no-package-refresh-contents")

;; refresh-contentのlockファイルが無い場合
(when (not (file-exists-p package-refresh-contents-lock))
  ;; refresh-contents実行、実行後lockファイル作成
  (package-refresh-contents)
  (with-temp-buffer
    (insert (concat "package-refresh-contents\n last: " (current-time-string)))
    (write-file package-refresh-contents-lock)))

;;; ---------------------------------------------------------------------------
;;; magit : emacs git client
;;; ---------------------------------------------------------------------------

(use-package magit
  :ensure t)

;;; ---------------------------------------------------------------------------
;;; local elisp files
;;; ---------------------------------------------------------------------------

;; 設定ファイルのディレクトリ
(add-to-list 'load-path "~/.emacs.d/dconf")

;; 外部パッケージの設定
(require 'package-conf)

;; 雑多な設定
(require 'bizz-conf)

;; エディタの外観/サイズ調整
(require 'appearance-conf)

;; 言語共通設定(要package-install)
(require 'common-lang-conf)

;; 言語設定(要package-install)
(require 'language-conf)

;; 外部のpackage化されてない追加機能(package-install不要)
(require 'external-elisp)

;; 自作の追加機能(package-install不要)
(require 'internal-elisp)

;; キーバインド設定(package-install不要)
(require 'key-binding)

;;; ---------------------------------------------------------------------------
;;; configuration report
;;; ---------------------------------------------------------------------------
(insert ";; hello world, emacs !!\n;; (´･_･`)\n")
(insert (report-failed-packages))

;;; ---------------------------------------------------------------------------
;;; temp (playground)
;;; ---------------------------------------------------------------------------
