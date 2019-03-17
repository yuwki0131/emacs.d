;;; search-package-conf.el --- packages
;;; Commentary:
;;;  Emacsインストールパッケージ(package-install要) / search-package-conf.el
;;;  package search : 検索
;;; Code:
(require 'package)
(require 'use-package)
(require 'util-elisp)

;;; ---------------------------------------------------------------------------
;;; swiper : 絞り込みfuzzy検索
;;; ---------------------------------------------------------------------------
(use-package-with-report swiper)

;;; ---------------------------------------------------------------------------
;;; swoop : トークンレベル移動(検索系)
;;; ---------------------------------------------------------------------------
(use-package-with-report swoop
  :config
  (setq swoop-minibuffer-input-dilay 0.4)
  (setq swoop-window-split-current-window: nil)
  (setq swoop-font-size-change: t)
  (setq swoop-font-size: 0.9))

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

;;; ---------------------------------------------------------------------------
;;; package-func : google this : ググる
;;; ---------------------------------------------------------------------------
(use-package-with-report google-this
  :config
  (google-this-mode t)
  (setq google-this-location-suffix "co.jp")
  (defun google-this-url ()
    "URL for google searches."
    (concat google-this-base-url google-this-location-suffix
	    "/search?q=%s&hl=ja&num=100&as_qdr=y5&lr=lang_ja")))

;;; ---------------------------------------------------------------------------
;;; package-func : codic : j2e/e2j dictionary
;;; ---------------------------------------------------------------------------
(use-package-with-report codic)

;;; ---------------------------------------------------------------------------
;;; package-func : codic : e2e dictionary
;;; ---------------------------------------------------------------------------
(use-package-with-report define-word)

;;; --------------------------------------------------------------------------------
;;; provide
;;; --------------------------------------------------------------------------------
(provide 'search-package-conf)
;;; search-package-conf.el ends here
