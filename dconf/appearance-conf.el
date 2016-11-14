;;---------------------------------------------------------------------------
;;---------------------------------------------------------------------------
;;
;; 外観 / appearance-conf.el
;;
;;---------------------------------------------------------------------------
;;---------------------------------------------------------------------------

;;---------------------------------------------------------------------------
;; フォント設定
;;---------------------------------------------------------------------------
;; Windows系の場合
(when (eq system-type 'windows-nt)
  (set-face-attribute
   'default nil :family "Ubuntu Mono" :height 90)
  (set-fontset-font
   'nil 'japanese-jisx0208 (font-spec :family "MeiryoKe_Gothic" :size 10)))

;; Linux系の場合
(when (eq system-type 'gnu/linux)
  (set-face-attribute
   'default nil :family "Ubuntu Mono" :height 100))

;;---------------------------------------------------------------------------
;; 色設定
;;---------------------------------------------------------------------------
;; 括弧の対応関係を色で示す
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; 選択範囲に色をつける
(setq transient-mark-mode t)
(setq font-lock-maximum-decoration t)

;; cyan系色
(defvar color/darkcyan "#0997B6")
(defvar color/lightcyan "lightcyan")

;; 背景/前景の設定
(set-background-color "#111111")
(set-foreground-color "#DDDDDD")

;; 各構文要素の色/タイプ(normal/italic/bold)の設定
(defun set-face-fore-with-cbi
    (attr-symbol color-name bold-param italic-param)
  (set-face-foreground attr-symbol color-name)
  (set-face-bold-p     attr-symbol bold-param)
  (set-face-italic-p   attr-symbol italic-param))

(set-face-fore-with-cbi 'font-lock-comment-face       "gray40"       nil t)
(set-face-fore-with-cbi 'font-lock-string-face        "deeppink"     t   nil)
(set-face-fore-with-cbi 'font-lock-keyword-face       color/darkcyan t   nil)
(set-face-fore-with-cbi 'font-lock-builtin-face       color/darkcyan nil nil)
(set-face-fore-with-cbi 'font-lock-function-name-face "#AFCA95"      t   nil)
(set-face-fore-with-cbi 'font-lock-variable-name-face "#AFCA95"      t   nil)
(set-face-fore-with-cbi 'font-lock-type-face          color/darkcyan t   nil)
(set-face-fore-with-cbi 'font-lock-constant-face      "deeppink"     t   nil)
(set-face-fore-with-cbi 'font-lock-warning-face       "deeppink"     nil t)

;; 対応する括弧に色をつける
(set-face-background 'show-paren-match-face "white")
(set-face-foreground 'show-paren-match-face "black")

;; カーソルの色
(set-cursor-color "red")

;; カーソル行ハイライト
(defface hlline-face
  '((((class color) (background dark))  (:background "#441200"))
    (((class color) (background light)) (:background "lightcyan"))
    (t ())) "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)
(global-hl-line-mode)

;; 選択範囲
(set-face-foreground 'region "black")
(set-face-background 'region "darkred")

;; 行番号(line-num)の色の設定
(set-face-attribute 'linum nil
    :foreground "white" :background color/darkcyan
    :weight 'bold)

;; モードラインの色の設定(active)
(set-face-attribute 'mode-line nil
    :foreground "lightcyan" :background "black"
    :inverse-video nil
    :weight 'bold
    :height 100
    :box '(:line-width 1 :color "black" :style nil))

;; モードラインの色の設定(inactive)
(set-face-attribute 'mode-line-inactive nil
    :foreground "lightcyan" :background color/darkcyan
    :inverse-video nil
    :weight 'extra-light
    :height 100
    :box '(:line-width 1 :color "gray30" :style nil))

;;---------------------------------------------------------------------------
;; ウィンドウ幅などの設定
;;---------------------------------------------------------------------------
;; カーソルタイプ
(setq default-cursor-type '(bar . 2))

;; タイトルバー
(setq frame-title-format "%b")

;; モードライン
(display-time)
(setq display-time-day-and-date t)
(setq display-time-string-forms
      '((format "time : %s/%s(%s) [%s:%s]" month day dayname 24-hours minutes)))

;; 行番号フォーマット
(setq linum-format " %4d ")

;; 画面サイズ初期化
(setq initial-frame-alist
      '((top . 0) (left . 0) (width . 128) (height . 75) (alpha . (95 70))))

;;---------------------------------------------------------------------------
;; provide
;;---------------------------------------------------------------------------
(provide 'appearance-conf)
