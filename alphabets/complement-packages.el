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
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 30)
  (setq ivy-extra-directories nil)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-plus))))

;;; ---------------------------------------------------------------------------
;;; ivy-rich: 補完(ivyをさらにrichに)
;;; ---------------------------------------------------------------------------
(use-package-with-report ivy-rich
  :config
  (ivy-rich-mode 1)
  ;;; update ivy function
  (setq ivy-format-function #'ivy-format-function-line))

;;; ---------------------------------------------------------------------------
;;; counsel: 補完
;;; ---------------------------------------------------------------------------
(use-package-with-report counsel
  :config
  (defvar counsel-find-file-ignore-regexp
    (regexp-opt '("./" "../")))
  (global-set-key (kbd "M-x") 'counsel-M-x))

;;; ---------------------------------------------------------------------------
;;; ivy-rich: 補完(ivyをさらにrichに)
;;; ---------------------------------------------------------------------------
(use-package-with-report all-the-icons-ivy
  :config
  (all-the-icons-ivy-setup))

;;; ---------------------------------------------------------------------------
;;; package-func : company : 自動補完
;;; ---------------------------------------------------------------------------
(use-package-with-report company
  :config
  (global-company-mode 1)
  ;; デフォルトは0.5
  (setq company-idle-delay 0)
  ;; デフォルトは4
  (setq company-minimum-prefix-length 2)
  ;; loop
  (setq company-selection-wrap-around t))

(use-package-with-report company-quickhelp
  :config
  ;; help tip
  (company-quickhelp-mode +1))

;;; --------------------------------------------------------------------------------
;;; provide
;;; --------------------------------------------------------------------------------
(provide 'complement-packages)
;;; complement-packages.el ends here
