;;; appearance-conf.el --- packages
;;; Commentary:
;;;  外観 / appearance-conf.el
;;; Code:

;;; ---------------------------------------------------------------------------
;;; ハイライト系設定(標準)
;;; ---------------------------------------------------------------------------
;; M-x list-faces-display で編集対象の色を表示
;; 括弧の対応関係を色で示す
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; 選択範囲に色をつける
(setq transient-mark-mode t)
(setq font-lock-maximum-decoration t)

;;; ---------------------------------------------------------------------------
;;; 色設定
;;; ---------------------------------------------------------------------------
;; cyan系
(defvar color/darkcyan "#0997B6")
(defvar color/lightcyan "lightcyan")

;; red系
(defvar color/red "red")
(defvar color/darkred "darkred")

;; magenta系
(defvar color/deeppink "#E5266A")
(defvar color/purpledark "#9B8B9B")
(defvar color/lightpink "lightpink")

;; orange系
(defvar color/orange "#FF4C00")

;; lime green系
(defvar color/limegreen "#32cd32")

;; gray系
(defvar color/active "#101010")
(defvar color/inactive "#393939")
(defvar color/bggray "#FFFFFF")
(defvar color/fggray "#101010")
(defvar color/comment "#777777")

;; 背景/前景の設定
(set-background-color color/bggray)
(set-foreground-color color/fggray)

;; 各構文要素の色/タイプ(normal/italic/bold)の設定
(defun set-face-fore-with-cbi (attr-symbol color-name bold-param italic-param)
  (set-face-foreground attr-symbol color-name)
  (set-face-bold-p     attr-symbol bold-param)
  (set-face-italic-p   attr-symbol italic-param))

;; 各構文要素の色(foreground, background)/タイプ(normal/italic/bold)の設定
(defun set-face-fore-with-bfcbi (attr-symbol color-fg color-bg bold italic)
  (set-face-foreground attr-symbol color-fg)
  (set-face-background attr-symbol color-bg)
  (set-face-bold-p     attr-symbol bold)
  (set-face-italic-p   attr-symbol italic))

;; coloring program
(set-face-fore-with-cbi 'font-lock-comment-face       color/comment    nil t)
(set-face-fore-with-cbi 'font-lock-doc-face           color/deeppink   nil t)
(set-face-fore-with-cbi 'font-lock-string-face        color/limegreen  t   nil)
(set-face-fore-with-cbi 'font-lock-keyword-face       color/darkcyan   t   nil)
(set-face-fore-with-cbi 'font-lock-builtin-face       color/darkcyan   nil nil)
(set-face-fore-with-cbi 'font-lock-function-name-face color/orange     t   nil)
(set-face-fore-with-cbi 'font-lock-variable-name-face color/orange     t   nil)
(set-face-fore-with-cbi 'font-lock-type-face          color/darkcyan   t   nil)
(set-face-fore-with-cbi 'font-lock-constant-face      color/deeppink   t   nil)
(set-face-fore-with-cbi 'font-lock-warning-face       color/deeppink   nil t)

;; coloring program (extension)
;; (set-face-fore-with-cbi 'highlight-numbers-number     color/deeppink   nil nil)
;; (set-face-fore-with-cbi 'highlight-operators-face     color/limegreen  nil nil)

;; coloring property
(ignore-errors
  (set-face-fore-with-cbi 'info-header-xref             color/darkcyan   nil t)
  (set-face-fore-with-cbi 'info-xref                    color/darkcyan   nil t)
  (set-face-fore-with-cbi 'link                         color/deeppink   nil t)
  (set-face-fore-with-cbi 'escape-glyph                 color/darkcyan   nil nil)
  (set-face-fore-with-cbi 'minibuffer-prompt            color/deeppink   t   nil))

;; coloring ac
(ignore-errors
  (set-face-fore-with-bfcbi 'ac-selection-face  color/deeppink "white"   t   nil)
  (set-face-fore-with-bfcbi 'ac-candidate-face  "white" color/darkcyan   t   nil))

;; カーソルの色
(set-cursor-color color/deeppink)

;; カーソル行ハイライト
(defface hlline-face
  `((((class color) (background dark))  (:background ,color/inactive))
    (((class color) (background light)) (:background ,color/lightcyan))
    (t ())) "*Face used by hl-line.")

(setq hl-line-face 'hlline-face)

;; カーソル桁ハイライト
(ignore-errors
  (custom-set-faces '(col-highlight ((t (:inherit hl-line))))))

;; 選択範囲
(set-face-foreground 'region "gray80")
(set-face-background 'region "gray20")

;; 行番号(line-num)の色の設定
(ignore-errors
  (set-face-attribute
   'linum nil
   :foreground "white" :background color/inactive
   :weight 'bold))

;; モードラインの設定(active)
(ignore-errors
  (set-face-attribute 'mode-line nil
   :foreground color/lightcyan :background color/inactive
   :inverse-video nil
   :weight 'extra-light
   :height 110
   :font default-font-family
   :box '(:line-width 1 :color "black" :style nil)))

;; モードラインの設定(inactive)
(ignore-errors
  (set-face-attribute 'mode-line-inactive nil
   :foreground color/lightcyan :background color/inactive
   :inverse-video nil
   :weight 'extra-light
   :height 110
   :font default-font-family
   :box '(:line-width 1 :color "gray30" :style nil)))

;; モードラインのフォーマットの設定
(ignore-errors
  (defvar fly-notice
    '(:eval (cond
             (buffer-read-only
              (propertize "□ 🚧"
                          'face '(:foreground "#FF4C00" :weight 'bold)
                          'help-echo "buffer is read-only !!!"))
             ((buffer-modified-p)
              (propertize "💾 ✗"
                          'face '(:foreground "deeppink")
                          'help-echo "buffer modified."))
             ((not (buffer-modified-p))
              (propertize "□ ✓"
                          'face '(:foreground "#0997B6")
                          'help-echo "no buffer modified.")))))

  ;; tanakh-chain (´･_･`) ( ´･_･) (  ´･_)
  (defvar tanakh-chain-list
    (list
     "(´･_･`)"
     "( ´･_･)"
     "(  ´･_)"
     "(   ´･)"
     "(    ´)"
     "(      )"
     "(`     )"
     "(･`    )"
     "(_･`   )"
     "(･_･`  )"))

  (defun insert-tanakh-chain ()
    (let ((i (% (cadr (current-time)) (length tanakh-chain-list))))
      (propertize (nth i tanakh-chain-list))))

  ;; エラーで落ちるとデフォルトに戻るので注意
  (defvar mode-line-format-customized
    (list "%e "
          ;; mode-line-front-space
          ;; * エンコーディングシステム
          mode-line-mule-info
           ;; mode-line-client
          ;; * カレントバッファの修正通知 (fly-noticeがあるのでoff)
          ;; mode-line-modified
          ;; * 修正/readonly通知
          "  "  fly-notice "  "
          ;; mode-line-auto-compile
          ;; mode-line-remote
          ;; (:eval (mode-line-frame-control))
          ;; * バッファ名
          mode-line-buffer-identification
          "  "
          mode-line-position
          "  "
          '(:eval (insert-tanakh-chain))
          ;; * VC + modes
          ;; '(:eval (vc-mode vc-mode))
          ;; mode-line-modes
          ;; mode-line-misc-info
          ;; mode-line-end-spaces
          ))

  ;; カスタマイズ済みのフォーマットの設定
  (setq-default mode-line-format mode-line-format-customized)

  (setq global-mode-string
        '((t jabber-activity-mode-string)
          "" display-time-string appt-mode-string)))

(ignore-errors
  (custom-set-variables '(hl-sexp-background-color color/lightpink)))

;;;---------------------------------------------------------------------------
;;; ウィンドウ幅などの設定
;;;---------------------------------------------------------------------------
;; カーソルタイプ
(setq default-cursor-type '(bar . 2))

;; タイトルバー
(setq frame-title-format "Emacs: %b")

;; モードライン(時間非表示)
(setq display-time-day-and-date -1)

;; 行番号フォーマット
;; (setq linum-format " %4d")

;; 画面サイズ初期化
(setq initial-frame-alist
      '((top . 20) (left . 0) (width . 128) (height . 75)
        (alpha . (100 100))))

;;;---------------------------------------------------------------------------
;;; provide
;;;---------------------------------------------------------------------------
(provide 'appearance-conf)
