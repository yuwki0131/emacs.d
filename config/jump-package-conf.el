;;; jump-package-conf.el --- packages
;;; Commentary:
;;;  Emacsインストールパッケージ(package-install要) / jump-package-conf.el
;;;  package jump : 移動
;;; Code:
(require 'package)
(require 'use-package)
(require 'util-elisp)

;;; ---------------------------------------------------------------------------
;;; dumb-jump : 言語によらず定義にジャンプ
;;; ---------------------------------------------------------------------------
(use-package-with-report dumb-jump
  :config
  (setq dumb-jump-mode t))

;;; ---------------------------------------------------------------------------
;;; package-move : ace window : smartなwindow移動
;;; ---------------------------------------------------------------------------
(use-package-with-report ace-window)

;;; ---------------------------------------------------------------------------
;;; ace jump mode : 任意の場所に3ストロークで移動
;;; ---------------------------------------------------------------------------
(use-package-with-report ace-jump-mode
  :config
  (setq aw-minibuffer-flag t))

;;; ---------------------------------------------------------------------------
;;; subword mode : Camel notationのシンボル移動時の単位を変更
;;; ---------------------------------------------------------------------------
;;; (before) |ITransientAssociative| -> (after) |I|Transient|Associative|
(use-package-with-report subword
  :config
 (global-subword-mode +1))

;;; ---------------------------------------------------------------------------
;;; package-move : goto chg : 最後に変更した箇所へカーソルを移動
;;; ---------------------------------------------------------------------------
(use-package-with-report goto-chg)

;;; --------------------------------------------------------------------------------
;;; provide
;;; --------------------------------------------------------------------------------
(provide 'jump-package-conf)
;;; jump-package-conf.el ends here
