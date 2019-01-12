;;; ---------------------------------------------------------------------------
;;; fancy-narrow : 現在の領域をnarrowingする
;;; ---------------------------------------------------------------------------
;;; memo:
;;;   使わないかつ警告が出るので一旦、disable
;; (use-package-with-report fancy-narrow
;;   :disabled t
;;   :config
;;   (fancy-narrow-mode 1))

;;; ---------------------------------------------------------------------------
;;; column highlight line plus : カーソル桁ハイライト
;;; ---------------------------------------------------------------------------
;; (use-package-with-report col-highlight
;;   :disabled t
;;   :config
;;   (toggle-highlight-column-when-idle 1)
;;   (col-highlight-set-interval 3)
;;   (column-highlight-mode nil))

;;; ---------------------------------------------------------------------------
;;; highlight current-buffer : 現在のバッファをハイライト
;;; ---------------------------------------------------------------------------
;; (use-package-with-report hiwin
;;   :disabled t
;;   :config
;;   (hiwin-activate)
;;   (set-face-background 'hiwin-face "#D0D0D0"))

;;; ---------------------------------------------------------------------------
;;; highlight block : 現在のブロックをハイライト
;;; ---------------------------------------------------------------------------
;; (use-package-with-report highlight-blocks
;;   :disabled
;;   :config
;;   (add-hook 'prog-mode-hook '(lambda () (highlight-blocks-mode 1))))

;;; ---------------------------------------------------------------------------
;;; prompt-text : ミニバッファの左側にカレントディレクトリを表示
;;; ---------------------------------------------------------------------------
;; (use-package-with-report prompt-text
;;   :disabled t
;;   :config
;;   (prompt-text-mode 1))

;;; ---------------------------------------------------------------------------
;;; sublimity : sublime風アウトライン表示
;;; ---------------------------------------------------------------------------
;;; memo:
;;;   結局すべて使わなくなってしまった
;; (use-package-with-report sublimity
;;   :disabled t
;;   :config
;;   (require 'sublimity-scroll)
;;   ;; これも基本的に不要
;;   ;; (require 'sublimity-map)
;;   ;; 表示領域が狭くなるため基本的に不要
;;   ;; (require 'sublimity-attractive)
;;   (add-hook 'prog-mode-hook '(lambda () (sublimity-mode 1))))

;;; ---------------------------------------------------------------------------
;;; golden-ratio : 黄金比で表示
;;; ---------------------------------------------------------------------------
;;; memo:
;;;   使わなくなったので
;; (use-package-with-report golden-ratio
;;   :disabled t
;;   :config
;;   (golden-ratio-mode 1))

;;; ---------------------------------------------------------------------------
;;; smart newline : 改行
;;; ---------------------------------------------------------------------------
;;; memo:
;;;   使わなくなったので
;; (use-package-with-report smart-newline
;;   :disabled t
;;   :config
;;   (smart-newline-mode 1))

;;; ---------------------------------------------------------------------------
;;; hungry-delete-mode : 空白の貪欲な削除
;;; ---------------------------------------------------------------------------
;; (use-package-with-report hungry-delete
;;   :disabled t
;;   :config
;;   (global-hungry-delete-mode))


;;; ---------------------------------------------------------------------------
;;; visual regexp steroids : 正規表現の拡張
;;; ---------------------------------------------------------------------------
;;; memo:
;;;   使っていないので
;; (use-package-with-report visual-regexp-steroids
;;   :config
;;   (setq vr/engine 'java))

;;; ---------------------------------------------------------------------------
;;; helm : helm
;;; ---------------------------------------------------------------------------
;;; memo:
;;;   使っていないので
;; (use-package-with-report helm
;;    :diminish helm-mode
;;    :config
;;   (require 'helm-config)
;;   (helm-mode 1))

;;; ---------------------------------------------------------------------------
;;; zop-to-char : M-zの可視化
;;; ---------------------------------------------------------------------------
;;; memo:
;;;   使っていないので
;; (use-package-with-report zop-to-char)

;;; ---------------------------------------------------------------------------
;;; package-func : w3m : w3m in emacs
;;; ---------------------------------------------------------------------------
;; ;高速化のため不使用
;; (use-package-with-report w3m
;;   :config
;;   (setq w3m-command "w3m"))

;;; ---------------------------------------------------------------------------
;;; package-func : google translate : google翻訳
;;; ---------------------------------------------------------------------------
;; (use-package-with-report google-translate
;;   ;;TODO: Fix config
;; )

;;; ---------------------------------------------------------------------------
;;; package-func : tiny menu : tiny menu
;;; ---------------------------------------------------------------------------
;; (use-package-with-report tiny-menu)
