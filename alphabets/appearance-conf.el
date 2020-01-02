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
;; emphasis
(defvar color/main               "#0997B6") ;; darkcyan
(defvar color/secondary          "#FF4C00") ;; orange
(defvar color/emphasis           "#E5266A") ;; deep pink
(defvar color/string-like        "#3CB371") ;; green

;; normal (gray)
(defvar color/active-strong      "#080808")
(defvar color/active             "#101010") ;; for foreground color
(defvar color/inactive           "#393939") ;; for foreground color, but not active
(defvar color/invert             "#393939") ;; for inverted background color
(defvar color/out-strong         "#666666") ;; kind of comment out, but strong
(defvar color/out                "#777777") ;; kind of comment out (removed from context)
(defvar color/fade-strong        "#A0A0A0")
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
(set-face-app1 'font-lock-comment-delimiter-face color/out-strong  t   t)
(set-face-app1 'font-lock-comment-face           color/out         nil t)
(set-face-app1 'font-lock-doc-face               color/emphasis    nil t)
(set-face-app1 'font-lock-string-face            color/string-like t   nil)
(set-face-app1 'font-lock-keyword-face           color/main        t   nil)
(set-face-app1 'font-lock-builtin-face           color/main        nil nil)
(set-face-app1 'font-lock-function-name-face     color/secondary   t   nil)
(set-face-app1 'font-lock-variable-name-face     color/secondary   nil nil)
(set-face-app1 'font-lock-type-face              color/main        t   nil)
(set-face-app1 'font-lock-constant-face          color/emphasis    t   nil)
(set-face-app1 'font-lock-warning-face           color/emphasis    nil t)
(set-face-app1 'font-lock-preprocessor-face      color/main        nil nil)
(set-face-app1 'font-lock-negation-char-face     color/main        nil nil)

;; fringe colors
(set-face-attribute 'fringe nil :foreground color/hide :background color/hide)

;; for grep
(ignore-report
 (set-face-app2 'compilation-info   color/emphasis        color/hide nil nil)
 (set-face-app2 'wgrep-delete-face  color/inverted-active color/main nil t)
 (set-face-app2 'wgrep-done-face    color/main            color/hide nil nil)
 (set-face-app2 'wgrep-face         color/hide            color/main nil t)
 (set-face-app2 'wgrep-file-face    color/hide            color/main nil t))

;; coloring property
(ignore-report
 (set-face-app1 'info-header-xref  color/main     nil t)
 (set-face-app1 'info-xref         color/main     nil t)
 (set-face-app1 'link              color/emphasis nil t)
 (set-face-app1 'escape-glyph      color/main     nil nil)
 (set-face-app1 'minibuffer-prompt color/emphasis t   nil))

;; coloring company
(ignore-errors
  (set-face-app3 'company-preview-common           color/main           color/hide           nil t t)
  (set-face-app2 'company-tooltip                  color/emphasis       color/popup-hide     nil t)
  (set-face-app2 'company-tooltip-common           color/emphasis       color/popup-hide     nil t)
  (set-face-app2 'company-tooltip-common-selection color/main           color/popup-active   nil t)
  (set-face-app2 'company-tooltip-selection        color/main           color/popup-active   nil t)
  (set-face-app2 'company-tooltip-mouse            color/main           color/popup-active   nil t)
  (set-face-app2 'company-tooltip-search           color/main           color/popup-active   nil t)
  (set-face-app2 'company-tooltip-search-selection color/main           color/popup-active   nil t)
  (set-face-app2 'company-scrollbar-fg             color/emphasis       color/emphasis       nil t)
  (set-face-app2 'company-scrollbar-bg             color/popup-inactive color/popup-inactive nil t)
  )

;; ivy colors
(ignore-report
  (set-face-app2 'ivy-current-match                color/inverted-active color/emphasis  nil t)
  (set-face-app2 'ivy-minibuffer-match-face-2      color/inverted-active color/emphasis  t   t)
  (set-face-app2 'counsel-outline-default          color/main            color/hide      nil t)
  (set-face-app2 'ivy-grep-info                    color/main            color/hide      nil t)
  (set-face-app2 'ivy-separator                    color/main            color/hide      nil t)
  (set-face-app1 'ivy-highlight-face               color/emphasis        color/fade      nil t)
  )

;; coloring bm
(ignore-report
  (set-face-app2 'bm-face                          color/inverted-active color/emphasis  nil t)
  (set-face-app2 'bm-fringe-face                   color/inverted-active color/emphasis  nil t)
  (set-face-app2 'bm-fringe-persistent-face        color/inverted-active color/secondary nil t)
  (set-face-app2 'bm-persistent-face               color/inverted-active color/secondary nil t))

;; coloring isearch
(ignore-report
  (set-face-app2 'isearch                          color/inverted-active color/main     nil t)
  (set-face-app2 'isearch-fail                     color/inverted-active color/emphasis nil t))

;; coloring swoop
(ignore-report
  (set-face-app2 'swoop-face-header-format-line    color/inverted-active color/invert   nil t)
  (set-face-app2 'swoop-face-line-buffer-name      color/inverted-active color/main     nil t)
  (set-face-app2 'swoop-face-line-number           color/inverted-active color/invert   nil t)
  (set-face-app2 'swoop-face-target-line           color/inverted-active color/emphasis nil t)
  (set-face-app2 'swoop-face-target-words          color/inverted-active color/main     nil t))

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
  (set-face-app2 'show-paren-match    color/active-strong color/fade-strong t t)
  (set-face-app2 'show-paren-mismatch color/active-strong color/fade-strong t t))

;; colors after inserted
(ignore-report
  (set-face-app2 'secondary-selection color/emphasis color/inactive nil t))

;; coloring git-gutter+
(ignore-report
  (set-face-app2 'git-gutter+-added    color/inverted-active color/main      nil t)
  (set-face-app2 'git-gutter+-deleted  color/inverted-active color/emphasis  nil t)
  (set-face-app2 'git-gutter+-modified color/inverted-active color/secondary nil t))

;; coloring doom
(ignore-report
 (set-face-app2 'doom-modeline-bar color/main color/main nil t)
)

;; カーソルの色
(set-cursor-color color/emphasis)

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
   :foreground color/inverted-active
   :background color/invert
   :weight 'bold
   :height 100
   :overline color/secondary))

;; モードラインの設定(active)
(ignore-report
  (set-face-attribute 'mode-line nil
   :foreground color/inverted-active
   :background color/invert
   :weight 'extra-light
   :height 100))

;; モードラインの設定(inactive)
(ignore-report
  (set-face-attribute 'mode-line-inactive nil
   :foreground color/inverted-active
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
