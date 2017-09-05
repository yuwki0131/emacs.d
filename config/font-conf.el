;;; font-conf.el --- packages
;;; Commentary:
;;;  フォント設定 / font-conf.el
;;; Code:

;;; ---------------------------------------------------------------------------
;;; フォント設定
;;; ---------------------------------------------------------------------------

(defvar default-font-family "Ubuntu Mono")
(defvar default-font-family-jp "Takaoゴシック")

;; デフォルトフォント設定
(set-face-attribute
 'default nil :family default-font-family :height 100)

;; 日本語のフォントセット : あいうえお ... 日本語
(set-fontset-font
 'nil 'japanese-jisx0208 (font-spec :family default-font-family-jp :height 90))

;; ギリシャ文字のフォントセット : αβγκλ ... ΛΩ
(set-fontset-font
 'nil '(#x0370 . #x03FF) (font-spec :family default-font-family :height 100))

;; キリル文字のフォントセット : Эта статья ... Русский
(set-fontset-font
 'nil '(#x0400 . #x04FF) (font-spec :family default-font-family :height 100))

;; 行間
(setq-default line-spacing 2)

;;;---------------------------------------------------------------------------
;;; provide
;;;---------------------------------------------------------------------------
(provide 'font-conf)
