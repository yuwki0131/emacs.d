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

;;; ---------------------------------------------------------------------------
;;; package-func : auto compile : elファイル自動コンパイル
;;; ---------------------------------------------------------------------------
;;; memo:
;;;   現在不要なので一旦、使用を保留
;; (use-package-with-report auto-compile
;;   :disabled t
;;   :config
;;   (auto-compile-on-load-mode)
;;   (auto-compile-on-save-mode))

;;; ---------------------------------------------------------------------------
;;; package-func : auto async byte compile : emacsのバイトコンパイルの自動化
;;; ---------------------------------------------------------------------------
;; (use-package-with-report auto-async-byte-compile
;;   :disabled t
;;   :config
;;   ;(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
;;   )

;;; ---------------------------------------------------------------------------
;;; package-func : hide comnt : hide comment
;;; ---------------------------------------------------------------------------
;; (use-package-with-report hide-comnt)

;;; ---------------------------------------------------------------------------
;;; package-move : popwin : ヘルプ/補完バッファをポップアップで表示
;;; ---------------------------------------------------------------------------
;; (use-package-with-report popwin
;;   :config
;;   (popwin-mode 1))

;;; ---------------------------------------------------------------------------
;;; package-move : shell-pop : popup(lightweight) shell
;;; ---------------------------------------------------------------------------
;; (use-package-with-report shell-pop
;;   :config
;;   (setq shell-pop-shell-type '("shell" "*shell*" (lambda () (shell)))))

;;; --------------------------------------------------------------------------------
;;; package-config : configuration
;;; --------------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; package-config : mouse disable : マウス禁止
;;; ---------------------------------------------------------------------------
;; (use-package-with-report disable-mouse
;;   :diminish disable-mouse-mode
;;   :config
;;   (global-disable-mouse-mode))

;;; ---------------------------------------------------------------------------
;;; package-report : aozora-view
;;; ---------------------------------------------------------------------------
;; (use-package-with-report aozora-view)

;;; ---------------------------------------------------------------------------
;;; package-func : simon : system monitor
;;; ---------------------------------------------------------------------------
;; (use-package-with-report symon
;;   :config
;;   (setq symon-delay 30)
;;   (symon-mode))

;;; ---------------------------------------------------------------------------
;;; にゃーん
;;; ---------------------------------------------------------------------------
;; original : https://www.youtube.com/watch?v=QH2-TGUlwu4
;; (use-package-with-report nyan-mode
;;   :config
;;   (nyan-mode)
;;   (nyan-start-animation))

;;; ---------------------------------------------------------------------------
;;; parrot
;;; ---------------------------------------------------------------------------
;; original : https://cultofthepartyparrot.com/
;; (use-package-with-report parrot)

;;; ---------------------------------------------------------------------------
;;; origami : ford the inner of parenthesis
;;; ---------------------------------------------------------------------------
;;; memo:
;;;   使わないので
;; (use-package-with-report origami)

;;; ---------------------------------------------------------------------------
;;; yasnippet : スニペット
;;; ---------------------------------------------------------------------------
;; see : https://github.com/AndreaCrotti/yasnippet-snippets
;; (use-package-with-report yasnippet
;;   :diminish yas-minor-mode
;;   :config
;;   (setq yas-snippet-dirs
;;  '("~/.emacs.d/mysnippets"
;;    "~/.emacs.d/yasnippets"))
;;   (yas-global-mode 1))

;; memo :
;; 新規スニペット作成バッファを用意する
;; (define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; 既存スニペットを閲覧・編集する
;; (define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)

;;; ---------------------------------------------------------------------------
;;; package-func : twittering mode : ついった
;;; ---------------------------------------------------------------------------
;; (use-package-with-report twittering-mode
;;   :config
;;   (setq twittering-use-master-password t)
;;   ;; 更新頻度(sec)
;;   (setq twittering-timer-interval 30)
;;   ;; 単位時間あたりのツイート取得数
;;   (setq twittering-number-of-tweets-on-retrieval 50)
;;   ;; アイコン表示
;;   (setq twittering-icon-mode t)
;;   ;; 表示形式
;;   (setq twittering-status-format "%i @%s %S %p: \n %T
;; ──────────────────────────────────────────────────────────────────────────"))

;;; ---------------------------------------------------------------------------
;;; package-func : browse kill ring : kill ring
;;; ---------------------------------------------------------------------------
;; (use-package-with-report browse-kill-ring)
