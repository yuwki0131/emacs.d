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
(defvar color/black              "black")
(defvar color/white              "white")
(defvar color/near-white         "gray95")
(defvar color/material           "#393939")
(defvar color/active             "#101010")
(defvar color/inactive           "#393939")
(defvar color/tooltip-fg         "#D0D0D0")
(defvar color/tooltip-bg         "#F0F0F0")
(defvar color/bggray             "#FFFFFF") ;; foreground gary
(defvar color/fggray             "#101010") ;; background gary
(defvar color/comment            "#777777")
(defvar color/comment-delimitter "#666666")

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

;; 各構文要素の色(foreground, background)/タイプ(normal/italic/bold/underline)の設定
(defun set-face-app3 (attr-symbol color-fg color-bg bold italic underline)
  (set-face-foreground  attr-symbol color-fg)
  (set-face-background  attr-symbol color-bg)
  (set-face-bold        attr-symbol bold)
  (set-face-italic      attr-symbol italic)
  (set-face-underline-p attr-symbol underline))

;; coloring program
(set-face-app1 'font-lock-comment-delimiter-face color/comment-delimitter t t)
(set-face-app1 'font-lock-comment-face           color/comment   nil t)
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
(set-face-attribute 'fringe nil :foreground color/white :background color/white)

;; for grep
(ignore-report
 (set-face-app2 'compilation-info   color/deeppink   color/white    nil nil)
 (set-face-app2 'wgrep-delete-face  color/near-white color/darkcyan nil t)
 (set-face-app2 'wgrep-done-face    color/darkcyan   color/white    nil nil)
 (set-face-app2 'wgrep-face         color/white      color/darkcyan nil t)
 (set-face-app2 'wgrep-file-face    color/white      color/darkcyan nil t))

;; coloring property
(ignore-report
 (set-face-app1 'info-header-xref  color/darkcyan nil t)
 (set-face-app1 'info-xref         color/darkcyan nil t)
 (set-face-app1 'link              color/deeppink nil t)
 (set-face-app1 'escape-glyph      color/darkcyan nil nil)
 (set-face-app1 'minibuffer-prompt color/deeppink t   nil))

;; coloring company
(ignore-errors
  (set-face-app3 'company-preview-common           color/darkcyan color/bggray     nil t t)
  (set-face-app2 'company-tooltip                  color/deeppink color/tooltip-bg nil t)
  (set-face-app2 'company-tooltip-common           color/deeppink color/tooltip-bg nil t)
  (set-face-app2 'company-tooltip-common-selection color/darkcyan color/tooltip-fg nil t)
  (set-face-app2 'company-tooltip-selection        color/darkcyan color/tooltip-fg nil t)
  (set-face-app2 'company-scrollbar-fg             color/material color/deeppink   nil t)
  (set-face-app2 'company-scrollbar-bg             color/deeppink color/material   nil t)
  )

;; doom-modeline bar
'(ignore-report
  (set-face-app2 'doom-modeline-bar                color/material   color/deeppink nil t))

;; ivy colors
(ignore-report
  (set-face-app2 'ivy-current-match                color/near-white color/deeppink  nil t)
  (set-face-app2 'ivy-minibuffer-match-face-2      color/material   color/lightpink t   t)
  (set-face-app2 'counsel-outline-default          color/darkcyan   color/white     nil t)
  (set-face-app2 'ivy-grep-info                    color/darkcyan   color/white     nil t)
  (set-face-app2 'ivy-separator                    color/darkcyan   color/white     nil t)
  )

;; popup
(ignore-report
  (set-face-app2 'popup-face                       color/deeppink   color/near-white nil t)
  (set-face-app2 'popup-isearch-match              color/deeppink   color/near-white nil t)
  (set-face-app2 'popup-menu-face                  color/deeppink   color/near-white nil t)
  (set-face-app2 'popup-menu-mouse-face            color/deeppink   color/near-white nil t)
  (set-face-app2 'popup-menu-selection-face        color/deeppink   color/near-white nil t)
  (set-face-app2 'popup-menu-summary-face          color/deeppink   color/near-white nil t)
  (set-face-app2 'popup-scroll-bar-background-face color/darkcyan   color/black      nil t)
  (set-face-app2 'popup-scroll-bar-foreground-face color/darkcyan   color/near-white nil t)
  (set-face-app2 'popup-summary-face               color/darkcyan   color/near-white nil t)
  (set-face-app2 'popup-tip-face                   color/darkcyan   color/near-white nil t)
  ;; (set-face-app2 'pulse-highlight-face             color/darkcyan color/near-white nil t)
  ;; (set-face-app2 'pulse-highlight-start-face       color/darkcyan color/near-white nil t)
  )

;; coloring bm
(ignore-report
  (set-face-app2 'bm-face                          color/lightcyan  color/deeppink nil t)
  (set-face-app2 'bm-fringe-face                   color/lightcyan  color/deeppink nil t)
  (set-face-app2 'bm-fringe-persistent-face        color/white      color/orange   nil t)
  (set-face-app2 'bm-persistent-face               color/white      color/orange   nil t))

;; coloring isearch
(ignore-report
  (set-face-app2 'isearch                          color/white      color/darkcyan nil t)
  (set-face-app2 'isearch-fail                     color/white      color/darkred  nil t))

;; coloring swoop
(ignore-report
  (set-face-app2 'swoop-face-header-format-line    color/lightcyan  color/material nil t)
  (set-face-app2 'swoop-face-line-buffer-name      color/white      color/darkcyan nil t)
  (set-face-app2 'swoop-face-line-number           color/white      color/material nil t)
  (set-face-app2 'swoop-face-target-line           color/white      color/deeppink nil t)
  (set-face-app2 'swoop-face-target-words          color/white      color/darkcyan nil t))

;; fontset swoop
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
  (set-face-app2 'show-paren-match    color/white    color/darkcyan nil t)
  (set-face-app2 'show-paren-mismatch color/white    color/deeppink nil t))

;; color after inserted
(ignore-report
  (set-face-app2 'secondary-selection color/deeppink color/inactive nil t))

;; coloring git-gutter+
(ignore-report
  (set-face-app2 'git-gutter+-added    color/white   color/limegreen nil t)
  (set-face-app2 'git-gutter+-deleted  color/white   color/deeppink  nil t)
  (set-face-app2 'git-gutter+-modified color/white   color/orange    nil t))

;; カーソルの色
(set-cursor-color color/deeppink)

;; カーソル行ハイライト
(set-face-attribute 'hl-line nil :inherit nil :background color/lightpink)

;; 選択範囲
(set-face-foreground 'region "gray80")
(set-face-background 'region "gray20")

;; 行番号(line-number)の色の設定
(ignore-report
  (set-face-attribute
   'line-number nil
   :foreground color/material
   :background color/white
   :weight 'bold))

;; ヘッダーラインの設定(active)
(ignore-report
  (set-face-attribute 'header-line nil
   :foreground color/lightcyan
   :background color/material
   :weight 'bold
   :height 100
   :overline color/orange))

;; モードラインの設定(active)
(ignore-report
  (set-face-attribute 'mode-line nil
   :foreground color/lightcyan
   :background color/material
   :weight 'extra-light
   :height 100))

;; モードラインの設定(inactive)
(ignore-report
  (set-face-attribute 'mode-line-inactive nil
   :foreground color/lightcyan
   :background color/material
   :weight 'extra-light
   :height 100))

(ignore-report
 (custom-set-variables '(hl-sexp-background-color color/lightpink)))

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
