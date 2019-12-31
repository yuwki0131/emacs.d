;;; rest-packages.el --- packages
;;; Commentary:
;;;  Emacsインストールパッケージ(package-install要) / rest-packages.el
;;;  package func : 機能
;;;  package config : 設定
;;;  package report : 報告・計測
;;; Code:
(require 'package)
(require 'use-package)
(require 'util-elisp)

;;; ---------------------------------------------------------------------------
;;; magit : emacs git client
;;; ---------------------------------------------------------------------------
(use-package-with-report magit)

;;; ---------------------------------------------------------------------------
;;; multi-term : terminal emulator
;;; ---------------------------------------------------------------------------
(use-package-with-report multi-term)

;;; ---------------------------------------------------------------------------
;;; emacs client/server
;;; ---------------------------------------------------------------------------
;;; memo:
;;;   emacsclinet/ecで起動する。(* ecは自分のbashrcの設定から)
;;;   C-x #で終了をターミナル側に通知する。
(use-package-with-report server
  :config
  (unless (server-running-p)
    (server-start)))

;;; ---------------------------------------------------------------------------
;;; restclient
;;; ---------------------------------------------------------------------------
(use-package-with-report restclient)

;;; ---------------------------------------------------------------------------
;;; auto sudoedit : 権限のないファイルをsudo経由で開きなおし
;;; ---------------------------------------------------------------------------
(use-package auto-sudoedit
  :ensure t
  :config
  (auto-sudoedit-mode 1))

;;; ---------------------------------------------------------------------------
;;; open junk file : ジャンクファイル生成
;;; ---------------------------------------------------------------------------
(use-package-with-report open-junk-file
  :config
  (setq open-junk-file-format "~/.emacs.d/junk/%Y-%m-%d-%H%M%S."))

;;; ---------------------------------------------------------------------------
;;; markdown preview mode : markdown preview
;;; ---------------------------------------------------------------------------
;; required sudo apt install node-marked
(use-package-with-report markdown-preview-mode
  :config
  (setq markdown-command "/usr/bin/marked"))

;;; ---------------------------------------------------------------------------
;;; which key : キーバインドサポート
;;; ---------------------------------------------------------------------------
(use-package-with-report which-key
  :config
  (which-key-setup-minibuffer)
  (which-key-mode))

;;; ---------------------------------------------------------------------------
;;; exec path from shell : シェルのpath設定引き継ぎ
;;; ---------------------------------------------------------------------------
;;; memo:
;;;    Windowsでは使わないこと
(use-package-with-report exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;;; ---------------------------------------------------------------------------
;;; esup : emacs起動時間の計測
;;; ---------------------------------------------------------------------------
(use-package-with-report esup)

;;; ---------------------------------------------------------------------------
;;; key freq mode : keyfreq
;;; ---------------------------------------------------------------------------
(use-package-with-report keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;;; --------------------------------------------------------------------------------
;;; provide
;;; --------------------------------------------------------------------------------
(provide 'rest-packages)
;;; rest-packages.el ends here
