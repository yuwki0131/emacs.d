;;; grep-package-conf.el --- packages
;;; Commentary:
;;;  Emacsインストールパッケージ(package-install要) / grep-package-conf.el
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
;;; package-func : auto complete : 自動補完
;;; ---------------------------------------------------------------------------
;; (use-package-with-report auto-complete
;;   :config
;;   (ac-config-default)
;;   (setq ac-auto-start 1)
;;   (setq ac-candidate-max 40)
;;   (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;;   (global-auto-complete-mode t))

(use-package-with-report company
  :config
  (global-company-mode 1)
  ;; デフォルトは0.5
  (setq company-idle-delay 0)
  ;; デフォルトは4
  (setq company-minimum-prefix-length 2)
  ;; help tip
  (company-quickhelp-mode +1)
  ;; loop
  (setq company-selection-wrap-around t))

;;; ---------------------------------------------------------------------------
;;; package-func : git complete : git-grep自動補完
;;; ---------------------------------------------------------------------------
(wconst-pakcage 'git-complete
  "https://raw.githubusercontent.com/zk-phi/git-complete/master/git-complete.el"
  nil)

;;; ---------------------------------------------------------------------------
;;; edit grepped text : grep済みのテキスト編集、反映
;;; ---------------------------------------------------------------------------
(use-package-with-report wgrep
  :config
  ;; "e"でwgrepモード有効
  (setf wgrep-enable-key "e")
  ;; wgrep終了時にバッファを保存
  (setq wgrep-auto-save-buffer t))

;;; ---------------------------------------------------------------------------
;;; ag
;;; ---------------------------------------------------------------------------
(use-package-with-report ag)
(use-package-with-report wgrep-ag)

;;; --------------------------------------------------------------------------------
;;; provide
;;; --------------------------------------------------------------------------------
(provide 'grep-package-conf)
;;; grep-package-conf.el ends here
