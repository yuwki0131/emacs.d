;;; appearance-conf.el --- packages
;;; Commentary:
;;;  外観 / appearance-conf.el
;;;  色を調べるときは、M-x list-faces-display
;;; Code:
(require 'util-elisp)

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
(defvar color/darkcyan           "#0997B6")
(defvar color/lightcyan          "lightcyan")

;; red系
(defvar color/red                "red")
(defvar color/darkred            "darkred")

;; magenta系
(defvar color/deeppink           "#E5266A")
(defvar color/purpledark         "#9B8B9B")
(defvar color/lightpink          "light pink")

;; orange系
(defvar color/orange             "#FF4C00")

;; lime green系
(defvar color/limegreen          "#32cd32")

;; gray系
(defvar color/active             "#101010") ;; for foreground color
(defvar color/inactive           "#393939") ;; for foreground color, but not active
(defvar color/invert             "#393939") ;; for inverted background color
(defvar color/out-strong         "#666666") ;; kind of comment out, but strong
(defvar color/out                "#777777") ;; kind of comment out (removed from context)
(defvar color/popup-active       "#D0D0D0")
(defvar color/popup-inactive     "#E4E4E4")
(defvar color/popup-hide         "#F0F0F0")
(defvar color/inverted-active    "#F1F1F1") ;; for foreground color in inverted background color
(defvar color/fade               "#F2F2F2")
(defvar color/hide               "#FFFFFF") ;; for background color

;; 背景/前景の設定
(set-foreground-color color/active)
(set-background-color color/hide)

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

;; 各構文要素の色(foreground, background)/タイプ(normal/italic/bold/underline)の設定
(defun set-face-app3 (attr-symbol color-fg color-bg bold italic underline)
  (set-face-foreground  attr-symbol color-fg)
  (set-face-background  attr-symbol color-bg)
  (set-face-bold        attr-symbol bold)
  (set-face-italic      attr-symbol italic)
  (set-face-underline-p attr-symbol underline))

;; coloring program
(set-face-app1 'font-lock-comment-delimiter-face color/out-strong t t)
(set-face-app1 'font-lock-comment-face           color/out        nil t)
(set-face-app1 'font-lock-doc-face               color/deeppink   nil t)
(set-face-app1 'font-lock-string-face            color/limegreen  t   nil)
(set-face-app1 'font-lock-keyword-face           color/darkcyan   t   nil)
(set-face-app1 'font-lock-builtin-face           color/darkcyan   nil nil)
(set-face-app1 'font-lock-function-name-face     color/orange     t   nil)
(set-face-app1 'font-lock-variable-name-face     color/orange     nil nil)
(set-face-app1 'font-lock-type-face              color/darkcyan   t   nil)
(set-face-app1 'font-lock-constant-face          color/deeppink   t   nil)
(set-face-app1 'font-lock-warning-face           color/deeppink   nil t)
(set-face-app1 'font-lock-preprocessor-face      color/darkcyan   nil nil)
(set-face-app1 'font-lock-negation-char-face     color/darkcyan   nil nil)

;; fringe colors
(set-face-attribute 'fringe nil :foreground color/hide :background color/hide)

;; for grep
(ignore-report
 (set-face-app2 'compilation-info   color/deeppink        color/hide     nil nil)
 (set-face-app2 'wgrep-delete-face  color/inverted-active color/darkcyan nil t)
 (set-face-app2 'wgrep-done-face    color/darkcyan        color/hide     nil nil)
 (set-face-app2 'wgrep-face         color/hide            color/darkcyan nil t)
 (set-face-app2 'wgrep-file-face    color/hide            color/darkcyan nil t))

;; coloring property
(ignore-report
 (set-face-app1 'info-header-xref  color/darkcyan nil t)
 (set-face-app1 'info-xref         color/darkcyan nil t)
 (set-face-app1 'link              color/deeppink nil t)
 (set-face-app1 'escape-glyph      color/darkcyan nil nil)
 (set-face-app1 'minibuffer-prompt color/deeppink t   nil))

;; coloring company
(ignore-errors
  (set-face-app3 'company-preview-common           color/darkcyan color/hide                 nil t t)
  (set-face-app2 'company-tooltip                  color/deeppink color/popup-hide           nil t)
  (set-face-app2 'company-tooltip-common           color/deeppink color/popup-hide           nil t)
  (set-face-app2 'company-tooltip-common-selection color/darkcyan color/popup-active         nil t)
  (set-face-app2 'company-tooltip-selection        color/darkcyan color/popup-active         nil t)
  (set-face-app2 'company-scrollbar-fg             color/deeppink color/deeppink             nil t)
  (set-face-app2 'company-scrollbar-bg             color/popup-inactive color/popup-inactive nil t)
  )

;; ivy colors
(ignore-report
  (set-face-app2 'ivy-current-match                color/inverted-active color/deeppink  nil t)
  (set-face-app2 'ivy-minibuffer-match-face-2      color/active          color/lightpink t   t)
  (set-face-app2 'counsel-outline-default          color/darkcyan        color/hide      nil t)
  (set-face-app2 'ivy-grep-info                    color/darkcyan        color/hide      nil t)
  (set-face-app2 'ivy-separator                    color/darkcyan        color/hide      nil t)
  )

;; coloring bm
(ignore-report
  (set-face-app2 'bm-face                          color/lightcyan  color/deeppink nil t)
  (set-face-app2 'bm-fringe-face                   color/lightcyan  color/deeppink nil t)
  (set-face-app2 'bm-fringe-persistent-face        color/inverted-active      color/orange   nil t)
  (set-face-app2 'bm-persistent-face               color/inverted-active      color/orange   nil t))

;; coloring isearch
(ignore-report
  (set-face-app2 'isearch                          color/inverted-active      color/darkcyan nil t)
  (set-face-app2 'isearch-fail                     color/inverted-active      color/darkred  nil t))

;; coloring swoop
(ignore-report
  (set-face-app2 'swoop-face-header-format-line    color/lightcyan  color/invert nil t)
  (set-face-app2 'swoop-face-line-buffer-name      color/inverted-active      color/darkcyan nil t)
  (set-face-app2 'swoop-face-line-number           color/inverted-active      color/invert nil t)
  (set-face-app2 'swoop-face-target-line           color/inverted-active      color/deeppink nil t)
  (set-face-app2 'swoop-face-target-words          color/inverted-active      color/darkcyan nil t))

;; fontset for swoop
(ignore-report
  (set-face-attribute
   'swoop-face-header-format-line nil
   :weight 'bold
   :height 100)
  (set-face-attribute
   'swoop-face-line-buffer-name nil
   :weight 'bold
   :height 100))

;; coloring paren
(ignore-report
  (set-face-app2 'show-paren-match    color/inverted-active    color/darkcyan nil t)
  (set-face-app2 'show-paren-mismatch color/inverted-active    color/deeppink nil t))

;; colors after inserted
(ignore-report
  (set-face-app2 'secondary-selection color/deeppink color/inactive nil t))

;; coloring git-gutter+
(ignore-report
  (set-face-app2 'git-gutter+-added    color/inverted-active   color/limegreen nil t)
  (set-face-app2 'git-gutter+-deleted  color/inverted-active   color/deeppink  nil t)
  (set-face-app2 'git-gutter+-modified color/inverted-active   color/orange    nil t))

;; カーソルの色
(set-cursor-color color/deeppink)

;; カーソル行ハイライト
(set-face-attribute 'hl-line nil :inherit nil :background color/fade)

;; 選択範囲
(set-face-foreground 'region "gray80")
(set-face-background 'region "gray20")

;; 行番号(line-number)の色の設定
(ignore-report
  (set-face-attribute
   'line-number nil
   :foreground color/inactive
   :background color/hide
   :weight 'bold))

;; ヘッダーラインの設定(active)
(ignore-report
  (set-face-attribute 'header-line nil
   :foreground color/lightcyan
   :background color/invert
   :weight 'bold
   :height 100
   :overline color/orange))

;; モードラインの設定(active)
(ignore-report
  (set-face-attribute 'mode-line nil
   :foreground color/lightcyan
   :background color/invert
   :weight 'extra-light
   :height 100))

;; モードラインの設定(inactive)
(ignore-report
  (set-face-attribute 'mode-line-inactive nil
   :foreground color/lightcyan
   :background color/invert
   :weight 'extra-light
   :height 100))

;;;---------------------------------------------------------------------------
;;; その他の設定
;;;---------------------------------------------------------------------------
;; カーソルタイプ
(setq-default cursor-type '(bar . 2))

;; タイトルバー
(setq frame-title-format "emacs: %b")

;; 余計なマージンを削除
(set-fringe-mode 0)

;;;---------------------------------------------------------------------------
;;; provide
;;;---------------------------------------------------------------------------
(provide 'appearance-conf)
;;; appearance-conf.el ends here
