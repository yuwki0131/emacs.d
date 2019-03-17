;;; grep-packages.el --- packages
;;; Commentary:
;;;  Emacsインストールパッケージ(package-install要) / grep-packages.el
;;;  Grep系のパッケージ
;;; Code:
(require 'package)
(require 'use-package)
(require 'util-elisp)

;;; ---------------------------------------------------------------------------
;;; git complete : git-grep自動補完
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
(provide 'grep-packages)
;;; grep-packages.el ends here
