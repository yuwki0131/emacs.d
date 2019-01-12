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
;;; all-the-icons : 現在行を永続的に記憶
;;; ---------------------------------------------------------------------------
(use-package-with-report all-the-icons)

;;; ---------------------------------------------------------------------------
;;; にゃーん
;;; ---------------------------------------------------------------------------
;; original : https://www.youtube.com/watch?v=QH2-TGUlwu4
(use-package-with-report nyan-mode
  :config
  (nyan-mode)
  (nyan-start-animation))

;;; ---------------------------------------------------------------------------
;;; parrot
;;; ---------------------------------------------------------------------------
;; original : https://cultofthepartyparrot.com/
(use-package-with-report parrot)

;;; ---------------------------------------------------------------------------
;;; nlinum-hl-mode : 軽量化された行番号表示
;;; ---------------------------------------------------------------------------
;; 標準は重いので使用しない
(use-package-with-report nlinum
  :config
  (global-nlinum-mode t)
  (setq nlinum-format " %4d  "))

;;; ---------------------------------------------------------------------------
;;; highlight line plus : カーソル行ハイライト(拡張)
;;; ---------------------------------------------------------------------------
;; 標準は重いので使用しない。以下を使用
(git-package
 (hl-line+ "https://github.com/emacsmirror/hl-line-plus.git" "hl-line-plus")
 (progn
   (toggle-hl-line-when-idle)
   (setq hl-line-idle-interval 3)))

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
