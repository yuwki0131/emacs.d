;;; display-package-conf.el --- packages
;;; Commentary:
;;;  Emacsインストールパッケージ(package-install要) / display-package-conf.el
;;;  package display : 表示
;;; Code:
(require 'package)
(require 'use-package)
(require 'util-elisp)

;;; ---------------------------------------------------------------------------
;;; bm : 現在行を永続的に記憶
;;; ---------------------------------------------------------------------------
(use-package-with-report bm)

;;; ---------------------------------------------------------------------------
;;; fancy-narrow : 現在の領域をnarrowingする
;;; ---------------------------------------------------------------------------
;;; memo:
;;;   使わないかつ警告が出るので一旦、disable
(use-package-with-report fancy-narrow
  :disabled t
  :config
  (fancy-narrow-mode 1))

;;; ---------------------------------------------------------------------------
;;; にゃーん
;;; ---------------------------------------------------------------------------
;; original : https://www.youtube.com/watch?v=QH2-TGUlwu4
(use-package-with-report nyan-mode
  :config
  (nyan-mode)
  (nyan-start-animation))

;;; ---------------------------------------------------------------------------
;;; nlinum-hl-mode : 軽量化された行番号表示
;;; ---------------------------------------------------------------------------
;; 標準は重いので使用しない
(use-package-with-report nlinum
  :config
  (global-nlinum-mode t)
  (setq nlinum-format " %4d "))

;;; ---------------------------------------------------------------------------
;;; highlight line plus : カーソル行ハイライト(拡張)
;;; ---------------------------------------------------------------------------
;; 標準は重いので使用しない。以下を使用
(use-package-with-report hl-line+
  :config
  (toggle-hl-line-when-idle)
  (setq hl-line-idle-interval 3))

;;; ---------------------------------------------------------------------------
;;; column highlight line plus : カーソル桁ハイライト
;;; ---------------------------------------------------------------------------
(use-package-with-report col-highlight
  :disabled t
  :config
  (toggle-highlight-column-when-idle 1)
  (col-highlight-set-interval 3)
  (column-highlight-mode nil))

;;; ---------------------------------------------------------------------------
;;; highlight current-buffer : 現在のバッファをハイライト
;;; ---------------------------------------------------------------------------
(use-package-with-report hiwin
  :disabled t
  :config
  (hiwin-activate)
  (set-face-background 'hiwin-face "#D0D0D0"))

;;; ---------------------------------------------------------------------------
;;; hl-todo-mode : TODOをハイライト
;;; ---------------------------------------------------------------------------
(use-package-with-report hl-todo
  :init
  (eval-when-compile (require 'hl-todo))
  (setq hl-todo-keyword-faces '(("TODO" . "#cc9393")))
  :config
  ;;; global-hl-todo-modeで有効にするメジャーモード(derived-mode)
  (setq hl-todo-activate-in-modes '(prog-mode))
  (global-hl-todo-mode 1))

;;; ---------------------------------------------------------------------------
;;; highlight indententation-mode : インデント表示
;;; ---------------------------------------------------------------------------
(use-package-with-report highlight-indentation
  :config
  (set-face-background 'highlight-indentation-face "#e0e0e0")
  (add-hook 'prog-mode-hook 'highlight-indentation-mode))

;;; ---------------------------------------------------------------------------
;;; volatile highlights : 修正箇所のハイライト
;;; ---------------------------------------------------------------------------
(use-package-with-report volatile-highlights
  :config
  (volatile-highlights-mode t))

;;; ---------------------------------------------------------------------------
;;; beacon : bufferを移動時にハイライト
;;; ---------------------------------------------------------------------------
(use-package-with-report beacon
  :config
  (beacon-mode 1))

;;; ---------------------------------------------------------------------------
;;; highlight symbol : カーソル位置のシンボルの自動ハイライト
;;; ---------------------------------------------------------------------------
(use-package-with-report highlight-symbol
  :config
  (add-hook 'prog-mode-hook 'highlight-symbol-mode))

;;; ---------------------------------------------------------------------------
;;; highlight block : 現在のブロックをハイライト
;;; ---------------------------------------------------------------------------
(use-package-with-report highlight-blocks
  :disabled
  :config
  (add-hook 'prog-mode-hook '(lambda () (highlight-blocks-mode 1))))

;;; ---------------------------------------------------------------------------
;;; rainbow delimiters : 対応括弧ハイライト
;;; ---------------------------------------------------------------------------
(use-package-with-report rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  ;; deriving from
  ;; https://yoo2080.wordpress.com/2013/12/21/small-rainbow-delimiters-tutorial
  (require 'cl-lib)
  (require 'color)
  (let ((index 1))
    (cl-loop
     for index from 1 to rainbow-delimiters-max-face-count
     do
     (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
       (cl-callf color-saturate-name (face-foreground face) 30)))))

;;; ---------------------------------------------------------------------------
;;; anzu : モードラインの左側に検索中の単語数を表示
;;; ---------------------------------------------------------------------------
(use-package-with-report anzu
  :diminish anzu-mode
  :config
  (global-anzu-mode t))

;;; ---------------------------------------------------------------------------
;;; prompt-text : ミニバッファの左側にカレントディレクトリを表示
;;; ---------------------------------------------------------------------------
(use-package-with-report prompt-text
  :disabled t
  :config
  (prompt-text-mode 1))

;;; ---------------------------------------------------------------------------
;;; path header line mode : path header line mode
;;; ---------------------------------------------------------------------------
(use-package-with-report path-headerline-mode
  :config
  (path-headerline-mode +1))

;;; ---------------------------------------------------------------------------
;;; sublimity : sublime風アウトライン表示
;;; ---------------------------------------------------------------------------
(use-package-with-report sublimity
  :config
  (require 'sublimity-scroll)
  ;; これも基本的に不要
  ;; (require 'sublimity-map)
  ;; 表示領域が狭くなるため基本的に不要
  ;; (require 'sublimity-attractive)
  (add-hook 'prog-mode-hook '(lambda () (sublimity-mode 1))))

;;; ---------------------------------------------------------------------------
;;; golden-ratio : 黄金比で表示
;;; ---------------------------------------------------------------------------
;;; memo:
;;;   使わなくなったので
(use-package-with-report golden-ratio
  :disabled t
  :config
  (golden-ratio-mode 1))

;;; --------------------------------------------------------------------------------
;;; provide
;;; --------------------------------------------------------------------------------
(provide 'display-package-conf)
;;; package-conf.el ends here
