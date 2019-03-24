;;; edit-packages.el --- packages
;;; Commentary:
;;;  Emacsインストールパッケージ(package-install要) / edit-packages.el
;;;  package edit : 編集
;;; Code:
(require 'package)
(require 'use-package)
(require 'util-elisp)

;;; ---------------------------------------------------------------------------
;;; redo+ : 普通のredo
;;; ---------------------------------------------------------------------------
(git-package
 (redo+ "https://github.com/emacsmirror/redo-plus.git" "redo-plus")
 (progn
   (setq undo-no-redo t)
   (setq undo-limit 60000)
   (setq undo-strong-limit 90000)))

;;; ---------------------------------------------------------------------------
;;; drug-stuff : 単語単位で移動
;;; ---------------------------------------------------------------------------
;;; memo:
;;;   <M-up>, <M-down>, <M-right>, <M-left>で行単位、単語単位で移動
(use-package-with-report drag-stuff
  :config
  (drag-stuff-define-keys)
  (drag-stuff-global-mode t))

;;; ---------------------------------------------------------------------------
;;; multiple-cursors-mode : multiple-cursors-mode
;;; ---------------------------------------------------------------------------
(use-package-with-report multiple-cursors)

;;; ---------------------------------------------------------------------------
;;; iedit-mode
;;; ---------------------------------------------------------------------------
(use-package-with-report iedit)

;;; ---------------------------------------------------------------------------
;;; change-inner
;;; ---------------------------------------------------------------------------
(use-package-with-report change-inner)

;;; --------------------------------------------------------------------------------
;;; provide
;;; --------------------------------------------------------------------------------
(provide 'edit-packages)
;;; edit-packages.el ends here
