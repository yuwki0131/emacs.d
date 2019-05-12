;;; z-packages.el --- packages
;;; Commentary:
;;;  Emacsインストールパッケージ(package-install要) / complement-packages.el
;;;  Windows向け軽量版パッケージリスト
;;; alternatives for
;;; - rest-packages
;;; - edit-packages
;;; - search-packages
;;; - highlight-packages
;;; - jump-packages

;;; Code:
(require 'package)
(require 'use-package)
(require 'util-elisp)

;;; ---------------------------------------------------------------------------
;;; magit : emacs git client
;;; ---------------------------------------------------------------------------
(use-package-with-report magit)

;;; ---------------------------------------------------------------------------
;;; restclient
;;; ---------------------------------------------------------------------------
(use-package-with-report restclient)

;;; ---------------------------------------------------------------------------
;;; redo+ : 普通のredo
;;; ---------------------------------------------------------------------------
(git-package
 (redo+ "https://github.com/emacsmirror/redo-plus.git" "redo-plus")
 (progn
   (setq
    undo-no-redo t
    undo-limit 60000
    undo-strong-limit 90000)))

;;; ---------------------------------------------------------------------------
;;; swiper : 絞り込みfuzzy検索
;;; ---------------------------------------------------------------------------
(use-package-with-report swiper)

;;; ---------------------------------------------------------------------------
;;; swoop : トークンレベル移動(検索系)
;;; ---------------------------------------------------------------------------
(use-package-with-report swoop
  :config
  (setq
   swoop-minibuffer-input-dilay 0.4
   swoop-window-split-current-window: nil
   swoop-font-size-change: t
   swoop-font-size: 0.9
   ))

;;; ---------------------------------------------------------------------------
;;; package-func : codic : j2e/e2j dictionary
;;; ---------------------------------------------------------------------------
(use-package-with-report codic)

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


;;; --------------------------------------------------------------------------------
;;; provide
;;; --------------------------------------------------------------------------------
(provide 'z-packages)
;;; complement-packages.el ends here
