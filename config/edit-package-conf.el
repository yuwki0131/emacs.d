;;; edit-package-conf.el --- packages
;;; Commentary:
;;;  Emacsインストールパッケージ(package-install要) / edit-package-conf.el
;;;  package edit : 編集
;;; Code:
(require 'package)
(require 'use-package)
(require 'util-elisp)

;;; ---------------------------------------------------------------------------
;;; redo+ : 普通のredo
;;; ---------------------------------------------------------------------------
(use-package-with-report redo+
  :config
  (setq undo-no-redo t)
  (setq undo-limit 60000)
  (setq undo-strong-limit 90000))

;;; ---------------------------------------------------------------------------
;;; smart newline : 改行
;;; ---------------------------------------------------------------------------
(use-package-with-report smart-newline
  :config
  (smart-newline-mode 1))

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
;;; hungry-delete-mode : 空白の貪欲な削除
;;; ---------------------------------------------------------------------------
(use-package-with-report hungry-delete
  :disabled t
  :config
  (global-hungry-delete-mode))

;;; ---------------------------------------------------------------------------
;;; drug-stuff : 単語単位で移動
;;; ---------------------------------------------------------------------------
(use-package-with-report drag-stuff
  :config
  (setq drag-stuff-modifier '(meta shift))
  (drag-stuff-global-mode t))

;;; ---------------------------------------------------------------------------
;;; multiple-cursors-mode : multiple-cursors-mode
;;; ---------------------------------------------------------------------------
(use-package-with-report multiple-cursors)

;;; ---------------------------------------------------------------------------
;;; iedit-mode
;;; ---------------------------------------------------------------------------
(use-package-with-report iedit)

;;; --------------------------------------------------------------------------------
;;; provide
;;; --------------------------------------------------------------------------------
(provide 'edit-package-conf)
;;; edit-package-conf.el ends here
