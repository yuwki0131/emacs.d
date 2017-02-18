;;; package --- emacs init.el
;;; Commentary:
;;;  Emacs init.el root
;;; Code:

;;; ---------------------------------------------------------------------------
;;; パッケージマネージャ (package.el & use-package)
;;; ---------------------------------------------------------------------------
(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
		("org" . "http://orgmode.org/elpa/")
		("melpa" . "http://melpa.milkbox.net/packages/")))
;; ("marmalade" . "http://marmalade-repo.org/packages/")

(package-initialize)

;;; ---------------------------------------------------------------------------
;;; use package : emacs パッケージ管理
;;; ---------------------------------------------------------------------------
;; usepackageが存在しない場合は、インストール
(when (not (require 'use-package nil 'noerror))
  (package-install 'use-package))
;; 以降use-packageでインストール

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
;;; ---------------------------------------------------------------------------
;;; local elisp files(dconf下ファイル)の分割方針

;;; - package : 外部パッケージ(elpaからパッケージ要取得)の設定項目
;;; - bizz : emacsデフォルト(elpaからパッケージの取得が不要)の設定項目
;;; - appearance : bizzに引続き、emacsデフォルトの外見設定
;;; - common-lang : 言語(*1)共通設定 or 複数言語に共通する設定(要elpaの設定)
;;; - language : 特定の言語(*1)設定、1言語限定の設定

;;; - external-eslip : 外部からコピペしてきたコードなど
;;; - internal-eslip : 自作したコード

;;; - key-binding : キーバインドは一括してここにまとめる

;;; *1 : ここで言語は、プログラミング言語、DSLなどの形式言語のみ
;;;      自然言語については、追々考えていきたい

;;; ---------------------------------------------------------------------------
;;; 問題点など
;;; - 横断的関心事 (外部パッケージ(package)とappearanceで設定項目が分離するなど)
;;; - 上に同じく、キーバインド定義がuse-package側でできないなどの問題がある
;;;   しかし、Ctrl-Z, Ctrl+A, Ctrl+E系のバインドの記述は一箇所にまとめたい等

;;; ---------------------------------------------------------------------------
;;; local elisp files
;;; ---------------------------------------------------------------------------

;; 設定ファイルのディレクトリ
(add-to-list 'load-path "~/.emacs.d/config")

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
;;; configuration report
;;; ---------------------------------------------------------------------------
(insert
 (concat ";; " (emoji-shufflin) "\"hello world, emacs !!\"\n"
	 ";; ('･_･`)\n\n"
	 ";; load-config reports\n"
	 (report-failed-packages)
	 (report-gsskey)))

;;; ---------------------------------------------------------------------------
;;; generate readme
;;; ---------------------------------------------------------------------------
(defconst readme-file-md "~/.emacs.d/README.md")

(defun spit (file-name text)
  (ignore-errors
	(if (file-exists-p file-name)
		(delete-file file-name))
	(find-file file-name)
	(insert text)
	(save-buffer)
	(kill-buffer)
    t))

(defvar readme-text
  (concat
   ;; header
   "# 自分用 ~/.emacs.d\n\n発展途上 & 未確認 (´・_・`)\n\n"
   ;; explain keybinds
   "\n## キーバインド\n\n"
   "デフォルト以外のキーバインド設定\n\n"
   keybinding-md))

(spit readme-file-md readme-text)
;;; ---------------------------------------------------------------------------
;;; temp (playground)
;;; ---------------------------------------------------------------------------

(provide 'init)
;;; init.el ends here
