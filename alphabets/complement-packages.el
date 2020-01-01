;;; complement-packages.el --- packages
;;; Commentary:
;;;  Emacsインストールパッケージ(package-install要) / complement-packages.el
;;;  補完とGrep系のパッケージ
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
  (setq
   ivy-use-virtual-buffers t
   enable-recursive-minibuffers t
   ivy-height 20
   ivy-extra-directories nil
   ivy-re-builders-alist '((t . ivy--regex-plus))
   ivy-count-format "(%d/%d) "
   ))

;; (setq ivy-display-style nil)

;;; ---------------------------------------------------------------------------
;;; ivy-rich: 補完(ivyをさらにrichに) enhance M-x
;;; ---------------------------------------------------------------------------
(use-package-with-report ivy-rich
  :config
  (ivy-rich-mode 1)
  ;;; update ivy function
  (setq ivy-format-function #'ivy-format-function-line))

;;; ---------------------------------------------------------------------------
;;; all-the-icons-ivy: ivy with icons
;;; ---------------------------------------------------------------------------
'(use-package-with-report all-the-icons-ivy
  :config
  (all-the-icons-ivy-setup))

;;; ---------------------------------------------------------------------------
;;; ivy-xref; enhance xref
;;; ---------------------------------------------------------------------------
(use-package-with-report ivy-xref
  :config
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;;; ---------------------------------------------------------------------------
;;; ivy-explorer; enhance find-file
;;; ---------------------------------------------------------------------------
(use-package-with-report ivy-explorer
  :config
  (ivy-explorer-mode 1)
  (counsel-mode 1))

;;; ---------------------------------------------------------------------------
;;; amx: enhance M-x
;;; ---------------------------------------------------------------------------
(use-package-with-report amx)

;;; ---------------------------------------------------------------------------
;;; counsel: counsel for M-x
;;; ---------------------------------------------------------------------------
(use-package-with-report counsel
  :config
  (defvar counsel-find-file-ignore-regexp
    (regexp-opt '("./" "../")))
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (setq
   counsel-preselect-current-file t
   counsel-yank-pop-preselect-last t
   ))

;;; ---------------------------------------------------------------------------
;;; package-func : company : 自動補完
;;; ---------------------------------------------------------------------------
(use-package-with-report company
  :config
  (global-company-mode 1)
  (setq
   ;; デフォルトは0.5
   company-idle-delay 0
   ;; デフォルトは0.5
   company-tooltip-idle-delay 0
   ;; デフォルトは4
   company-minimum-prefix-length 2
   ;; loop
   company-selection-wrap-around t
   ;; 補完候補の最大表示数
   company-tooltip-limit 20
   ;; 候補に番号を付与
   company-show-numbers t
   ))

(setq company-selection-wrap-around t)

(use-package-with-report company-quickhelp
  :config
  ;; help tip
  (company-quickhelp-mode +1))

;;; --------------------------------------------------------------------------------
;;; provide
;;; --------------------------------------------------------------------------------
(provide 'complement-packages)
;;; complement-packages.el ends here
