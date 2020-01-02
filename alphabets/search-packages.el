;;; search-packages.el --- packages
;;; Commentary:
;;;  Emacsインストールパッケージ(package-install要) / search-packages.el
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
;;; migemo : isearchをローマ字のままで日本語も検索可能に
;;; ---------------------------------------------------------------------------
;; required : sudo apt-get install cmigemo
(use-package-with-report migemo
  :config
  (when (executable-find "cmigemo")
    (setq
     migemo-options '("-q" "--emacs")
     migemo-user-dictionary nil
     migemo-regex-dictionary nil
     migemo-coding-system 'utf-8-unix
     migemo-command "cmigemo"
     migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
    (load-library "migemo")
    (migemo-init)))

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
(provide 'search-packages)
;;; search-packages.el ends here
