;;; appearance-conf.el --- packages
;;; Commentary:
;;;  外観 / appearance-conf.el
;;; Code:

;;; ---------------------------------------------------------------------------
;;; フォント設定
;;; ---------------------------------------------------------------------------

(defvar default-font-family "Ubuntu Mono")
(defvar default-font-family-jp "Takaoゴシック")
;; "MeiryoKe_Gothic"

(set-face-attribute 'default nil :family default-font-family :height 100)
(set-fontset-font
 'nil 'japanese-jisx0208 (font-spec :family default-font-family-jp :height 90))
(setq-default line-spacing 2)
;;; ---------------------------------------------------------------------------
;;; 色設定(標準)
;;; ---------------------------------------------------------------------------
;; M-x list-faces-display で編集対象の色を表示
;; 括弧の対応関係を色で示す
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; 選択範囲に色をつける
(setq transient-mark-mode t)
(setq font-lock-maximum-decoration t)

;; cyan系
(defvar color/darkcyan "#0997B6")
(defvar color/lightcyan "lightcyan")

;; red系
(defvar color/red "red")
(defvar color/darkred "darkred")

;; magenta系
(defvar color/deeppink "deeppink")
(defvar color/purpledark "#4B3B4B")

;; lime green系
(defvar color/limegreen "#AFCA95")

;; gray系
(defvar color/active "#101010")
(defvar color/inactive "#393939")
(defvar color/bggray "#101010")
(defvar color/fggray "#E0E0E0")
(defvar color/comment "#999999")

;; 背景/前景の設定
(set-background-color color/bggray)
(set-foreground-color color/fggray)

;; 各構文要素の色/タイプ(normal/italic/bold)の設定
(defun set-face-fore-with-cbi (attr-symbol color-name bold-param italic-param)
  (set-face-foreground attr-symbol color-name)
  (set-face-bold-p     attr-symbol bold-param)
  (set-face-italic-p   attr-symbol italic-param))

;; 各構文要素の色(foreground, background)/タイプ(normal/italic/bold)の設定
(defun set-face-fore-with-bfcbi
    (attr-symbol color-name-fg color-name-bg bold-param italic-param)
  (set-face-foreground attr-symbol color-name-fg)
  (set-face-background attr-symbol color-name-bg)
  (set-face-bold-p     attr-symbol bold-param)
  (set-face-italic-p   attr-symbol italic-param))

;; coloring program
(set-face-fore-with-cbi 'font-lock-comment-face       color/comment    nil t)
(set-face-fore-with-cbi 'font-lock-doc-face           color/deeppink   nil t)
(set-face-fore-with-cbi 'font-lock-string-face        color/limegreen  nil nil)
(set-face-fore-with-cbi 'font-lock-keyword-face       color/darkcyan   t   nil)
(set-face-fore-with-cbi 'font-lock-builtin-face       color/darkcyan   nil nil)
(set-face-fore-with-cbi 'font-lock-function-name-face color/limegreen  nil nil)
(set-face-fore-with-cbi 'font-lock-variable-name-face color/limegreen  nil nil)
(set-face-fore-with-cbi 'font-lock-type-face          color/darkcyan   t   nil)
(set-face-fore-with-cbi 'font-lock-constant-face      color/deeppink   t   nil)
(set-face-fore-with-cbi 'font-lock-warning-face       color/deeppink   nil t)

;; coloring program (extension)
;; (set-face-fore-with-cbi 'highlight-numbers-number     color/deeppink   nil nil)
;; (set-face-fore-with-cbi 'highlight-operators-face     color/limegreen  nil nil)

;; coloring property
(set-face-fore-with-cbi 'info-header-xref             color/darkcyan   nil t)
(set-face-fore-with-cbi 'info-xref                    color/darkcyan   nil t)
(set-face-fore-with-cbi 'link                         color/deeppink   nil t)
(set-face-fore-with-cbi 'escape-glyph                 color/darkcyan   nil nil)
(set-face-fore-with-cbi 'minibuffer-prompt            color/deeppink   t   nil)

;; coloring ac
(set-face-fore-with-bfcbi 'ac-selection-face  color/deeppink "white"   t   nil)
(set-face-fore-with-bfcbi 'ac-candidate-face  "white" color/darkcyan   t   nil)

;; coloring helm
;;(set-face-fore-with-bfcbi 'helm-selection  color/deeppink color/inactive t nil)

;; 対応する括弧に色をつける
;; (set-face-background 'show-paren-match-face "white")
'(set-face-foreground 'show-paren-match-face color/deeppink)

;; カーソルの色
(set-cursor-color color/deeppink)

;; カーソル行ハイライト
(defface hlline-face
  `((((class color) (background dark))  (:background ,color/inactive))
    (((class color) (background light)) (:background ,color/lightcyan))
    (t ())) "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)
;; (setq hl-line-face 'underline)

;; カーソル桁ハイライト
'(ignore-errors
   (custom-set-faces '(col-highlight ((t (:inherit hl-line))))))

;; 選択範囲
(set-face-foreground 'region color/fggray)
(set-face-background 'region "black")

;; 行番号(line-num)の色の設定
(set-face-attribute 'linum nil
    :foreground "white" :background color/inactive
    :weight 'bold)

;; モードラインの色の設定(active)
(ignore-errors
  (set-face-attribute 'mode-line nil
    :foreground color/lightcyan :background color/inactive
    :inverse-video nil
    :weight 'bold
    :height 100
    :font default-font-family
    :box '(:line-width 1 :color "black" :style nil)))

;; モードラインの色の設定(inactive)
(ignore-errors
  (set-face-attribute 'mode-line-inactive nil
              :foreground color/lightcyan :background color/inactive
              :inverse-video nil
              :weight 'extra-light
              :height 100
              :font default-font-family
              :box '(:line-width 1 :color "gray30" :style nil)))

;;;---------------------------------------------------------------------------
;;; 色設定(標準以外)
;;;---------------------------------------------------------------------------

;;;---------------------------------------------------------------------------
;;; ウィンドウ幅などの設定
;;;---------------------------------------------------------------------------
;; カーソルタイプ
(setq default-cursor-type '(bar . 2))

;; タイトルバー
(setq frame-title-format "emacs : %b")

;; モードライン(時間非表示)
(setq display-time-day-and-date -1)

;; 行番号フォーマット
(setq linum-format " %4d")

;; 画面サイズ初期化
(setq initial-frame-alist
      '((top . 20) (left . 0) (width . 128) (height . 75)
        (alpha . (95 85))))

;;;---------------------------------------------------------------------------
;;; provide
;;;---------------------------------------------------------------------------
(provide 'appearance-conf)
