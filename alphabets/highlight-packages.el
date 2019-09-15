;;; highlight-packages.el --- packages
;;; Commentary:
;;;  Emacsインストールパッケージ(package-install要) / highlight-packages.el
;;;  package highlight : 表示
;;; Code:
(require 'package)
(require 'use-package)
(require 'util-elisp)

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
;;; highlight-indent-guides : インデント表示
;;; ---------------------------------------------------------------------------
(use-package-with-report highlight-indent-guides
  :config
  (setq
   highlight-indent-guides-method 'character
   highlight-indent-guides-responsive 'stack
   highlight-indent-guides-auto-odd-face-perc 15
   highlight-indent-guides-auto-even-face-perc 15
   highlight-indent-guides-auto-character-face-perc 30)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

;;; ---------------------------------------------------------------------------
;;; volatile highlights : 修正箇所のハイライト
;;; ---------------------------------------------------------------------------
(use-package-with-report volatile-highlights
  :config
  (volatile-highlights-mode t))

;;; ---------------------------------------------------------------------------
;;; volatile highlights : escape sequenceのハイライト
;;; ---------------------------------------------------------------------------
(use-package-with-report highlight-escape-sequences)

;;; ---------------------------------------------------------------------------
;;; highlight defined : 定義済みemacs-lisp symbolのハイライト
;;; ---------------------------------------------------------------------------
(use-package-with-report highlight-defined
  :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode))

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
;;; highlight numbers : 数値のハイライト
;;; ---------------------------------------------------------------------------
(use-package-with-report highlight-numbers
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;;; ---------------------------------------------------------------------------
;;; highlight operators : 演算子のハイライト
;;; ---------------------------------------------------------------------------
(use-package-with-report highlight-operators
  :config
  (add-hook 'python-mode-hook 'highlight-operators-mode)
  (add-hook 'lua-mode-hook 'highlight-operators-mode))

;;; --------------------------------------------------------------------------------
;;; provide
;;; --------------------------------------------------------------------------------
(provide 'highlight-packages)
;;; highlight-packages.el ends here
