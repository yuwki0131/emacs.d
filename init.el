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
 (concat ";; 🍣\"hello world, emacs !!\"\n"
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-python-pyflakes-extra-arguments (quote ("--max-line-length=120" "--ignore=E128")))
 '(package-selected-packages
   (quote
    (path-headerline-mode beacon browse-kill-ring+ mozc-im zop-to-char volatile-highlights visual-regexp-steroids vimrc-mode use-package twittering-mode tiny-menu swoop smooth-scroll sml-mode smart-mode-line redo+ rainbow-delimiters racket-mode quickrun python-pep8 python-mode pylint pyflakes py-autopep8 point-undo open-junk-file nyan-mode nurumacs nlinum neotree migemo markdown-preview-mode magit lua-mode keyfreq jedi ipython initchart hy-mode hungry-delete hlinum hl-line+ hiwin highlight-symbol highlight-sexp highlight-quoted highlight-operators highlight-numbers highlight-indent-guides helm-swoop helm-ag guide-key goto-chg google-translate google-this geiser free-keys flymake-python-pyflakes flymake-cursor flymake-checkers flycheck-pyflakes flycheck-mypy flycheck-haskell flycheck-gdc-dub flycheck-gdc flycheck-clojure fancy-narrow exec-path-from-shell esup emoji-fontset emoji-display elpy e2wm drag-stuff disable-mouse dic-lookup-w3m dash-functional col-highlight clj-refactor bm auto-highlight-symbol auto-complete-clang-async auto-compile auto-async-byte-compile anzu ace-jump-mode ac-slime ac-python))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(col-highlight ((t (:inherit hl-line)))))
