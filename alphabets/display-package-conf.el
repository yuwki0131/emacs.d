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
;;; imenu list : 関数(クラス)定義一覧を横に表示
;;; ---------------------------------------------------------------------------
(use-package-with-report git-gutter+
  :config
  (global-git-gutter+-mode 1))

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
;;; modern emacs modeline: doom-modeline
;;; ---------------------------------------------------------------------------
(use-package-with-report doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 22)
  (doom-modeline-def-modeline 'my-simple-line
    '(bar matches buffer-info remote-host)
    '(misc-info buffer-encoding major-mode process vcs checker))
  (defun setup-custom-doom-modeline ()
    (doom-modeline-set-modeline 'my-simple-line 'default))
  (add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline))

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

;;; ---------------------------------------------------------------------------
;;; package-func : neotree : ディレクトリ表示
;;; ---------------------------------------------------------------------------
(use-package-with-report neotree
  :config
  (defvar neo-persist-show t)
  (setq neo-show-hidden-files t)
  (setq neo-keymap-style 'concise)
  (setq neo-smart-open t)
  (setq neo-theme 'ascii)
  (setq neo-window-fixed-size nil)
  (setq neo-window-width 30)
  (add-hook 'neotree-mode-hook
            '(lambda ()
               (nlinum-mode -1)
               (setq mode-line-format nil)
               (force-mode-line-update)
               ))
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;;; --------------------------------------------------------------------------------
;;; provide
;;; --------------------------------------------------------------------------------
(provide 'display-package-conf)
;;; display-package-conf.el ends here
