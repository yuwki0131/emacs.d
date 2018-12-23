;;; Commentary:
;;;  Emacsインストールパッケージ(package-install要) / search-package-conf.el
;;;  package search : 検索
;;; Code:
(require 'package)
(require 'use-package)
(require 'util-elisp)

;;; ---------------------------------------------------------------------------
;;; visual regexp steroids : 正規表現の拡張
;;; ---------------------------------------------------------------------------
(use-package-with-report visual-regexp-steroids
  :config
  (setq vr/engine 'java))

;;; ---------------------------------------------------------------------------
;;; helm : helm
;;; ---------------------------------------------------------------------------
(use-package-with-report helm
   :diminish helm-mode
   :config
  (require 'helm-config)
  (helm-mode 1))

;;; ---------------------------------------------------------------------------
;;; zop-to-char : M-zの可視化
;;; ---------------------------------------------------------------------------
(use-package-with-report zop-to-char)

;;; ---------------------------------------------------------------------------
;;; swiper : 絞り込みfuzzy検索
;;; ---------------------------------------------------------------------------
(use-package-with-report swiper)

;;; ---------------------------------------------------------------------------
;;; dumb-jump : 絞り込みfuzzy検索
;;; ---------------------------------------------------------------------------
(use-package-with-report dumb-jump
  :config
  (setq dumb-jump-mode t))

;;; ---------------------------------------------------------------------------
;;; swoop : トークンレベル移動(検索系)
;;; ---------------------------------------------------------------------------
(use-package-with-report swoop
  :config
  (setq swoop-minibuffer-input-dilay 0.4))

;;; ---------------------------------------------------------------------------
;;; ace jump mode : 任意の場所に3ストロークで移動
;;; ---------------------------------------------------------------------------
(use-package-with-report ace-jump-mode)

;;; ---------------------------------------------------------------------------
;;; subword mode : Camel notationのシンボル移動時の単位を変更
;;; ---------------------------------------------------------------------------
;;; (before) |ITransientAssociative| -> (after) |I|Transient|Associative|
(use-package-with-report subword
  :config
 (global-subword-mode +1))

;;; ---------------------------------------------------------------------------
;;; migemo : isearchをローマ字のままで日本語も検索可能に
;;; ---------------------------------------------------------------------------
;; required : sudo apt-get install cmigemo
(use-package-with-report migemo
  :config
  (when (executable-find "cmigemo")
    (setq migemo-options '("-q" "--emacs"))
    (setq migemo-user-dictionary nil)
    (setq migemo-regex-dictionary nil)
    (setq migemo-coding-system 'utf-8-unix)
    (load-library "migemo")
    (migemo-init)
    (setq migemo-command "cmigemo")
    (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")))

;;; --------------------------------------------------------------------------------
;;; provide
;;; --------------------------------------------------------------------------------
(provide 'search-package-conf)
;;; search-package-conf.el ends here
