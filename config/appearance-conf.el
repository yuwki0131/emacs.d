;;; appearance-conf.el --- packages
;;; Commentary:
;;;  外観 / appearance-conf.el
;;;  色を調べるときは、M-x list-faces-display
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
  (set-face-fore-with-bfcbi 'ac-completion-face           color/deeppink  "gray"         nil t)
  (set-face-fore-with-bfcbi 'ac-slime-menu-face           color/deeppink  "gray"         nil t)
  ;; candidates
  (set-face-fore-with-bfcbi 'ac-candidate-face            color/lightcyan color/inactive t nil)
  (set-face-fore-with-bfcbi 'ac-candidate-mouse-face      color/lightcyan color/inactive t nil)
  (set-face-fore-with-bfcbi 'ac-yasnippet-candidate-face  color/lightcyan color/inactive t nil)
  (set-face-fore-with-bfcbi 'ac-gtags-candidate-face      color/lightcyan color/inactive t nil)
  ;; selections
  (set-face-fore-with-bfcbi 'ac-selection-face            color/deeppink color/inactive nil t)
  (set-face-fore-with-bfcbi 'ac-gtags-selection-face      color/deeppink color/inactive nil t)
  (set-face-fore-with-bfcbi 'ac-selection-face            color/deeppink color/inactive nil t)
  (set-face-fore-with-bfcbi 'ac-slime-selection-face      color/deeppink color/inactive nil t)
  (set-face-fore-with-bfcbi 'ac-yasnippet-selection-face  color/deeppink color/inactive nil t)
  ;; popup
  (set-face-fore-with-bfcbi 'popup-tip-face               color/lightcyan color/darkcyan nil t)
  (set-face-fore-with-bfcbi 'pulse-highlight-face         color/lightcyan color/darkcyan nil t)
  (set-face-fore-with-bfcbi 'pulse-highlight-start-face   color/darkcyan  color/darkcyan nil t))

;; coloring bm
(ignore-errors
  (set-face-fore-with-bfcbi 'bm-face                      color/lightcyan color/deeppink nil t)
  (set-face-fore-with-bfcbi 'bm-fringe-face               color/lightcyan color/deeppink nil t)
  (set-face-fore-with-bfcbi 'bm-fringe-persistent-face    "white"         color/orange   nil t)
  (set-face-fore-with-bfcbi 'bm-persistent-face           "white"         color/orange   nil t))

;; coloring isearch
(ignore-errors
  (set-face-fore-with-bfcbi 'isearch                      "white"         color/darkcyan nil t)
  (set-face-fore-with-bfcbi 'isearch-fail                 "white"         color/darkred  nil t))

;; coloring swoop
(ignore-errors
  (set-face-fore-with-bfcbi 'swoop-face-header-format-line color/lightcyan color/inactive nil t)
  (set-face-fore-with-bfcbi 'swoop-face-line-buffer-name   "white"         color/darkcyan nil t)
  (set-face-fore-with-bfcbi 'swoop-face-line-number        "white"         color/inactive nil t)
  (set-face-fore-with-bfcbi 'swoop-face-target-line        "white"         color/deeppink nil t)
  (set-face-fore-with-bfcbi 'swoop-face-target-words       "white"         color/darkcyan nil t))

;; coloring paren
(ignore-errors
  (set-face-fore-with-bfcbi 'show-paren-match              "white"         color/darkcyan nil t)
  (set-face-fore-with-bfcbi 'show-paren-mismatch           "white"         color/deeppink nil t))

;; color after inserted
(ignore-errors
  (set-face-fore-with-bfcbi 'secondary-selection           "deeppink"         color/inactive nil t))

;; カーソルの色
(set-cursor-color color/deeppink)

;; カーソル行ハイライト
(defface hlline-face
  `((((class color) (background dark))  (:background ,color/inactive))
    (((class color) (background light)) (:background "light gray"))
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

(ignore-errors
  (custom-set-variables '(hl-sexp-background-color color/lightpink)))

;;;---------------------------------------------------------------------------
;;; ウィンドウ幅などの設定
;;;---------------------------------------------------------------------------
;; カーソルタイプ
(setq default-cursor-type '(bar . 2))

;; タイトルバー
(setq frame-title-format "Emacs: %b")

;; モードライン(時間非表示) => もう不要
;; (setq display-time-day-and-date -1)

;; 行番号フォーマット
;; (setq linum-format " %4d")

;; 画面サイズ初期化
(setq initial-frame-alist
      '((top . 20) (left . 0) (width . 128) (height . 75)
        (alpha . (100 100))
        ;(minibuffer . nil)
        ))

'(setq minibuffer-frame-alist
      '((top . 1) (left . 1) (height . 2)
        ;; You'll need to adjust the following number.
        (width . 127)))

;;;---------------------------------------------------------------------------
;;; provide
;;;---------------------------------------------------------------------------
(provide 'appearance-conf)
