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
;;; counsel: 補完
;;; ---------------------------------------------------------------------------
(use-package-with-report counsel
  :config
  (defvar counsel-find-file-ignore-regexp
    (regexp-opt '("./" "../"))))

;;; ---------------------------------------------------------------------------
;;; package-func : auto complete : 自動補完
;;; ---------------------------------------------------------------------------
(use-package-with-report auto-complete
  :config
  (ac-config-default)
  (setq ac-auto-start 1)
  (setq ac-candidate-max 40)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict"))

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

;;; --------------------------------------------------------------------------------
;;; provide
;;; --------------------------------------------------------------------------------
(provide 'grep-package-conf)
;;; grep-package-conf.el ends here
