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
(defvar show-paren-style 'mixed)

;; 選択範囲に色をつける
(transient-mark-mode 1)
(setq font-lock-maximum-decoration t)

;;; ---------------------------------------------------------------------------
;;; 色設定
;;; ---------------------------------------------------------------------------
;; cyan系
(defvar color/darkcyan   "#0997B6")
(defvar color/lightcyan  "lightcyan")

;; red系
(defvar color/red        "red")
(defvar color/darkred    "darkred")

;; magenta系
(defvar color/deeppink   "#E5266A")
(defvar color/purpledark "#9B8B9B")
(defvar color/lightpink  "lightpink")

;; orange系
(defvar color/orange     "#FF4C00")

;; lime green系
(defvar color/limegreen  "#32cd32")

;; gray系
(defvar color/active     "#101010")
(defvar color/inactive   "#393939")
(defvar color/bggray     "#FFFFFF") ;; foreground gary
(defvar color/fggray     "#101010") ;; background gary
(defvar color/comment    "#777777")

;; 背景/前景の設定
(set-background-color color/bggray)
(set-foreground-color color/fggray)

;; 各構文要素の色/タイプ(normal/italic/bold)の設定
(defun set-face-app1 (attr-symbol color-name bold-param italic-param)
  (set-face-foreground attr-symbol color-name)
  (set-face-bold       attr-symbol bold-param)
  (set-face-italic     attr-symbol italic-param))

;; 各構文要素の色(foreground, background)/タイプ(normal/italic/bold)の設定
(defun set-face-app2 (attr-symbol color-fg color-bg bold italic)
  (set-face-foreground attr-symbol color-fg)
  (set-face-background attr-symbol color-bg)
  (set-face-bold       attr-symbol bold)
  (set-face-italic     attr-symbol italic))

;; coloring program
(set-face-app1 'font-lock-comment-face           color/comment   nil t)
(set-face-app1 'font-lock-comment-delimiter-face color/comment   nil t)
(set-face-app1 'font-lock-doc-face               color/deeppink  nil t)
(set-face-app1 'font-lock-string-face            color/limegreen t   nil)
(set-face-app1 'font-lock-keyword-face           color/darkcyan  t   nil)
(set-face-app1 'font-lock-builtin-face           color/darkcyan  nil nil)
(set-face-app1 'font-lock-function-name-face     color/orange    t   nil)
(set-face-app1 'font-lock-variable-name-face     color/orange    nil nil)
(set-face-app1 'font-lock-type-face              color/darkcyan  t   nil)
(set-face-app1 'font-lock-constant-face          color/deeppink  t   nil)
(set-face-app1 'font-lock-warning-face           color/deeppink  nil t)
(set-face-app1 'font-lock-preprocessor-face      color/darkcyan  nil nil)
(set-face-app1 'font-lock-negation-char-face     color/darkcyan  nil nil)

;; fringe colors
(set-face-attribute 'fringe nil :foreground "white" :background "white")

;; for grep
(ignore-report
 (set-face-app2 'compilation-info   color/deeppink "white" nil nil)
 (set-face-app2 'wgrep-delete-face "gray95"        color/darkcyan nil t)
 (set-face-app2 'wgrep-done-face    color/darkcyan "white"        nil nil)
 (set-face-app2 'wgrep-face        "white"         color/darkcyan nil t)
 (set-face-app2 'wgrep-file-face   "white"         color/darkcyan nil t))

;; coloring property
(ignore-report
  (set-face-app1 'info-header-xref  color/darkcyan nil t)
  (set-face-app1 'info-xref         color/darkcyan nil t)
  (set-face-app1 'link              color/deeppink nil t)
  (set-face-app1 'escape-glyph      color/darkcyan nil nil)
  (set-face-app1 'minibuffer-prompt color/deeppink t   nil))

;; coloring ac
(ignore-report
  (set-face-app2 'ac-completion-face           color/deeppink  "white"        nil nil))

(ignore-report
 ;; candidates
 (set-face-app2 'ac-candidate-face            color/lightcyan color/inactive t nil)
 (set-face-app2 'ac-candidate-mouse-face      color/lightcyan color/inactive t nil)
 (set-face-app2 'ac-yasnippet-candidate-face  color/lightcyan color/inactive t nil)
 (set-face-app2 'ac-gtags-candidate-face      color/lightcyan color/inactive t nil)
 ;; selections
 (set-face-app2 'ac-selection-face            color/deeppink color/inactive nil t)
 (set-face-app2 'ac-gtags-selection-face      color/deeppink color/inactive nil t)
 (set-face-app2 'ac-selection-face            color/deeppink color/inactive nil t)
 (set-face-app2 'ac-yasnippet-selection-face  color/deeppink color/inactive nil t))

;; popup
(ignore-report
  (set-face-app2 'popup-face                       color/deeppink "gray95" nil t)
  (set-face-app2 'popup-isearch-match              color/deeppink "gray95" nil t)
  (set-face-app2 'popup-menu-face                  color/deeppink "gray95" nil t)
  (set-face-app2 'popup-menu-mouse-face            color/deeppink "gray95" nil t)
  (set-face-app2 'popup-menu-selection-face        color/deeppink "gray95" nil t)
  (set-face-app2 'popup-menu-summary-face          color/deeppink "gray95" nil t)
  (set-face-app2 'popup-scroll-bar-background-face color/darkcyan "black"  nil t)
  (set-face-app2 'popup-scroll-bar-foreground-face color/darkcyan "gray95" nil t)
  (set-face-app2 'popup-summary-face               color/darkcyan "gray95" nil t)
  (set-face-app2 'popup-tip-face                   color/darkcyan "gray95" nil t)
  (set-face-app2 'pulse-highlight-face             color/darkcyan "gray95" nil t)
  (set-face-app2 'pulse-highlight-start-face       color/darkcyan "gray95" nil t))

;; coloring bm
(ignore-report
  (set-face-app2 'bm-face                      color/lightcyan color/deeppink nil t)
  (set-face-app2 'bm-fringe-face               color/lightcyan color/deeppink nil t)
  (set-face-app2 'bm-fringe-persistent-face    "white"         color/orange   nil t)
  (set-face-app2 'bm-persistent-face           "white"         color/orange   nil t))

;; coloring isearch
(ignore-report
  (set-face-app2 'isearch                      "white"         color/darkcyan nil t)
  (set-face-app2 'isearch-fail                 "white"         color/darkred  nil t))

;; coloring swoop
(ignore-report
  (set-face-app2 'swoop-face-header-format-line color/lightcyan color/inactive nil t)
  (set-face-app2 'swoop-face-line-buffer-name   "white"         color/darkcyan nil t)
  (set-face-app2 'swoop-face-line-number        "white"         color/inactive nil t)
  (set-face-app2 'swoop-face-target-line        "white"         color/deeppink nil t)
  (set-face-app2 'swoop-face-target-words       "white"         color/darkcyan nil t))

;; fontset swoop
(ignore-report
  (set-face-attribute
   'swoop-face-header-format-line nil
   :weight 'bold
   :height 100
   :font default-font-family)
  (set-face-attribute
   'swoop-face-line-buffer-name nil
   :weight 'bold
   :height 100
   :font default-font-family))

;; coloring paren
(ignore-report
  (set-face-app2 'show-paren-match              "white"         color/darkcyan nil t)
  (set-face-app2 'show-paren-mismatch           "white"         color/deeppink nil t))

;; color after inserted
(ignore-report
  (set-face-app2 'secondary-selection           "deeppink"      color/inactive nil t))

;; カーソルの色
(set-cursor-color color/deeppink)

;; カーソル行ハイライト
(defface hlline-face
  `((((class color) (background dark))  (:background ,color/inactive))
    (((class color) (background light)) (:background "light pink"))
    (t ())) "*Face used by hl-line.")

(setq hl-line-face 'hlline-face)

;; カーソル桁ハイライト
(ignore-report
  (custom-set-faces '(col-highlight ((t (:inherit hl-line))))))

;; 選択範囲
(set-face-foreground 'region "gray80")
(set-face-background 'region "gray20")

;; 行番号(line-num)の色の設定
(ignore-report
  (set-face-attribute
   'linum nil
   :foreground color/inactive
   :background "white"
   :weight 'bold))

;; ヘッダーラインの設定(active)
(ignore-report
  (set-face-attribute 'header-line nil
   :foreground color/lightcyan
   :background color/inactive
   :weight 'bold
   :height 100
   :font default-font-family
   :box `(:line-width 4 :color ,color/inactive :style nil)
   :overline "orange"))

;; モードラインの設定(active)
(ignore-report
  (set-face-attribute 'mode-line nil
   :foreground color/lightcyan
   :background color/inactive
   :inverse-video nil
   :weight 'extra-light
   :height 110
   :font default-font-family
   :box '(:line-width 1 :color "black" :style nil)))

;; モードラインの設定(inactive)
(ignore-report
  (set-face-attribute 'mode-line-inactive nil
   :foreground color/lightcyan
   :background color/inactive
   :inverse-video nil
   :weight 'extra-light
   :height 110
   :font default-font-family
   :box '(:line-width 1 :color "gray30" :style nil)))

(ignore-report
  (custom-set-variables '(hl-sexp-background-color color/lightpink)))

;;;---------------------------------------------------------------------------
;;; ウィンドウ幅などの設定
;;;---------------------------------------------------------------------------
;; カーソルタイプ
(setq-default cursor-type '(bar . 2))

;; タイトルバー
(setq frame-title-format "emacs: %b")

;; モードライン(時間非表示) => もう不要
;; (setq display-time-day-and-date -1)

;; 行番号フォーマット
;; (setq linum-format " %4d")

;; 画面サイズ初期化
;; (setq initial-frame-alist
;;       '((top . 20) (left . 0) (width . 128) (height . 75)
;;         (alpha . (100 100))
;;         ))

;; 余計なマージンを削除
(set-fringe-mode 0)

;;;---------------------------------------------------------------------------
;;; provide
;;;---------------------------------------------------------------------------
(provide 'appearance-conf)
;;; appearance-conf.el ends here
