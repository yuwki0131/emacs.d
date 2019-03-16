;;; package-conf.el --- packages
;;; Commentary:
;;;  Emacsインストールパッケージ(package-install要) / package-conf.el
;;;  package func : 機能
;;;  package config : 設定
;;;  package report : 報告・計測
;;; Code:
(require 'package)
(require 'use-package)
(require 'util-elisp)

;;; ---------------------------------------------------------------------------
;;; package-func : magit : emacs git client
;;; ---------------------------------------------------------------------------
(use-package-with-report magit)

;;; ---------------------------------------------------------------------------
;;; package-func : multi-term : terminal emulator
;;; ---------------------------------------------------------------------------
(use-package-with-report multi-term)

;;; ---------------------------------------------------------------------------
;;; package-func : server : emacs client/server
;;; ---------------------------------------------------------------------------
;;; memo:
;;;   emacsclinet/ecで起動する。(* ecは自分のbashrcの設定から)
;;;   C-x #で終了をターミナル側に通知する。
(use-package-with-report server
  :config
  (unless (server-running-p)
    (server-start)))

;;; ---------------------------------------------------------------------------
;;; package-func : restclient
;;; ---------------------------------------------------------------------------
(use-package-with-report restclient)

;;; ---------------------------------------------------------------------------
;;; package-func : open junk file : ジャンクファイル生成
;;; ---------------------------------------------------------------------------
(use-package-with-report open-junk-file
  :config
  (setq open-junk-file-format "~/.emacs.d/junk/%Y-%m-%d-%H%M%S."))

;;; ---------------------------------------------------------------------------
;;; package-func : markdown preview mode : markdown preview
;;; ---------------------------------------------------------------------------
;; required sudo apt install node-marked
(use-package-with-report markdown-preview-mode
  :config
  (setq markdown-command "/usr/bin/marked"))

;;; ---------------------------------------------------------------------------
;;; package-func : codic : j2e/e2j dictionary
;;; ---------------------------------------------------------------------------
(use-package-with-report codic)

;;; ---------------------------------------------------------------------------
;;; package-func : codic : e2e dictionary
;;; ---------------------------------------------------------------------------
(use-package-with-report define-word)

;;; ---------------------------------------------------------------------------
;;; package-func : which key : キーバインドサポート
;;; ---------------------------------------------------------------------------
(use-package-with-report which-key
  :config
  (which-key-setup-minibuffer)
  (which-key-mode))

;;; ---------------------------------------------------------------------------
;;; package-config : exec path from shell : シェルのpath設定引き継ぎ
;;; ---------------------------------------------------------------------------
;;; memo:
;;;    Windowsでは使わないこと
(use-package-with-report exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

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

;;; --------------------------------------------------------------------------------
;;; provide
;;; --------------------------------------------------------------------------------
(provide 'package-conf)
;;; package-conf.el ends here
