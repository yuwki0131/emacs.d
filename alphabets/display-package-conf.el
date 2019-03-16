;;; display-package-conf.el --- packages
;;; Commentary:
;;;  Emacsインストールパッケージ(package-install要) / display-package-conf.el
;;;  package display : 表示
;;; Code:
(require 'package)
(require 'use-package)
(require 'util-elisp)

;;; ---------------------------------------------------------------------------
;;; all-the-icons : iconの設定
;;; ---------------------------------------------------------------------------
(use-package-with-report all-the-icons)
(use-package-with-report all-the-icons-dired)

;;; ---------------------------------------------------------------------------
;;; imenu list : 関数(クラス)定義一覧を横に表示
;;; ---------------------------------------------------------------------------
(use-package-with-report imenu-list)

;;; ---------------------------------------------------------------------------
;;; minimap : ソースコードマップを横に表示
;;; ---------------------------------------------------------------------------
(use-package-with-report minimap
  :config
  ;; minimapを表示させる位置
  (setq minimap-window-location 'right)
  ;; 表示を更新する時間
  (setq minimap-update-delay 0.2)
  ;; 幅の長さ
  (setq minimap-minimum-width 25)
  )

;;; ---------------------------------------------------------------------------
;;; nlinum-hl-mode : 軽量化された行番号表示
;;; ---------------------------------------------------------------------------
;; 標準は重いので使用しない
(use-package-with-report nlinum
  :config
  (global-nlinum-mode t)
  (setq nlinum-format " %4d "))

;;; ---------------------------------------------------------------------------
;;; modern emacs modeline: doom-modeline
;;; ---------------------------------------------------------------------------
(use-package-with-report doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 22))

;;; ---------------------------------------------------------------------------
;;; anzu : モードラインの左側に検索中の単語数を表示
;;; ---------------------------------------------------------------------------
(use-package-with-report anzu
  :diminish anzu-mode
  :config
  (global-anzu-mode t))

;;; ---------------------------------------------------------------------------
;;; path header line mode : path header line mode
;;; ---------------------------------------------------------------------------
(use-package-with-report path-headerline-mode
  :config
  (path-headerline-mode +1))

;;; --------------------------------------------------------------------------------
;;; provide
;;; --------------------------------------------------------------------------------
(provide 'display-package-conf)
;;; display-package-conf.el ends here
