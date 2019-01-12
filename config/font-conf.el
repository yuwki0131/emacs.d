;;; font-conf.el --- packages
;;; Commentary:
;;;  フォント設定 / font-conf.el
;;; Code:

;;; ---------------------------------------------------------------------------
;;; フォント設定
;;; ---------------------------------------------------------------------------

(defvar default-font-family "Ubuntu Mono")
(setq default-font-family "Fira Code")

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
;;; リガチャ(for fira code)
;;;---------------------------------------------------------------------------
;; https://github.com/tonsky/FiraCode/wiki/Setting-up-Emacs

(let ((alist
       '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
         (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
         (36 . ".\\(?:>\\)")
         (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
         (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
         (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
         (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
         (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
         (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
         (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
         (48 . ".\\(?:x[a-zA-Z]\\)")
         (58 . ".\\(?:::\\|[:=]\\)")
         (59 . ".\\(?:;;\\|;\\)")
         (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
         (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
         (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
         (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
         (91 . ".\\(?:]\\)")
         (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
         (94 . ".\\(?:=\\)")
         (119 . ".\\(?:ww\\)")
         (123 . ".\\(?:-\\)")
         (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
         (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
         )))
  (dolist (char-regexp alist)
    (set-char-table-range
     composition-function-table (car char-regexp)
     `([,(cdr char-regexp) 0 font-shape-gstring]))))

;;;---------------------------------------------------------------------------
;;; provide
;;;---------------------------------------------------------------------------
(provide 'font-conf)
;;; font-conf.el ends here
