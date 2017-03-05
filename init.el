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
		("melpa" . "http://melpa.milkbox.net/packages/")
		("marmalade" . "http://marmalade-repo.org/packages/")))

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

;; utils
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
;;; configuration report
;;; ---------------------------------------------------------------------------
(insert
 (concat
  ";; 🍣\"hello world, emacs !!\"\n"
  ";; ('･_･`)\n\n"
  ";; load-config reports\n"
  (report-failed-packages)
  (report-gsskey)))

;;; ---------------------------------------------------------------------------
;;; generate readme
;;; ---------------------------------------------------------------------------
(defvar config-composition-md
  "~/.emacs.d/configディレクトリ以下

|el file|設定|
|:-------------|:------------------------------------------------------|
| package-cnof | 外部パッケージ(elpaからパッケージ要取得)の設定項目 |
| bizz-cnof | emacsデフォルト(elpaからパッケージの取得が不要)の設定項目 |
| appearance-cnof | bizzに引続き、emacsデフォルトの外見設定 |
| common-lang-cnof | 言語共通設定 or 複数言語に共通する設定(要elpaの設定) |
| language-cnof | 特定の言語設定、1言語ごとの設定 |
| external-eslip | 外部から持ち込んだコードなど |
| internal-eslip | 自作したコード |
| key-binding | キーバインドは一括してここにまとめる |

")

(defconst readme-file-md "~/.emacs.d/README.md")

(defvar readme-text
  (concat
   ;; header
   "# 自分用 ~/.emacs.d\n\n発展途上 & 未確認 (´・_・`)\n\n"
   "以下イメージ\n\n"
   "![画面](img/image.png)\n\n"
   ;; read-me text
   "修正して使う.
設定など.
```
$ git clone https://github.com/yuwki0131/emacs.d
$ mv emacs.d ~/.emacs.d
```
※要use-package
"
   ;; config composition
   "\n## elispファイル構成\n\n"
   config-composition-md
   ;; explain keybinds
   "\n## キーバインド\n\n"
   "デフォルト以外のglobal-set-key設定\n\n"
   keybinding-md))

(spit readme-file-md readme-text)
;;; ---------------------------------------------------------------------------
;;; temp (playground)
;;; ---------------------------------------------------------------------------

(provide 'init)
;;; init.el ends here
