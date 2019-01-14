;;; search-package-conf.el --- packages
;;; Commentary:
;;;  Emacsインストールパッケージ(package-install要) / search-package-conf.el
;;;  package search : 検索
;;; Code:
(require 'package)
(require 'use-package)
(require 'util-elisp)

;;; ---------------------------------------------------------------------------
;;; ivy: 補完
;;; ---------------------------------------------------------------------------
(use-package-with-report ivy
  :config
  ;; ivy設定
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 30)
  (setq ivy-extra-directories nil)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-plus))))

;;; ---------------------------------------------------------------------------
;;; counsel: 補完
;;; ---------------------------------------------------------------------------
(use-package-with-report counsel
  :config
  (defvar counsel-find-file-ignore-regexp
    (regexp-opt '("./" "../"))))

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

;;; --------------------------------------------------------------------------------
;;; provide
;;; --------------------------------------------------------------------------------
(provide 'search-package-conf)
;;; search-package-conf.el ends here
