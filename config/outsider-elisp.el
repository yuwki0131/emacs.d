;;; package --- outsider-elisp.el
;;; Commentary:
;;;  外部elisp(package化されてないもの)
;;; Code:

;;; ---------------------------------------------------------------------------
;;; fast cursor move : カーソル移動を高速化させる(rubikitch氏より)
;;; ---------------------------------------------------------------------------
;; code from http://emacs.rubikitch.com/global-hl-line-mode-timer/
(require 'hl-line)

;;; hl-lineを無効にするメジャーモードを指定する
(defvar global-hl-line-timer-exclude-modes '(todotxt-mode))

(defun global-hl-line-timer-function ()
  (unless (memq major-mode global-hl-line-timer-exclude-modes)
    (global-hl-line-unhighlight-all)
    (let ((global-hl-line-mode t))
      (global-hl-line-highlight))))

(defvar global-hl-line-timer
      (run-with-idle-timer 0.03 t 'global-hl-line-timer-function))
;; (cancel-timer global-hl-line-timer)

;;; ---------------------------------------------------------------------------
;;; 末尾スペース、タブ、全角スペースを強調表示
;;; ---------------------------------------------------------------------------
;; code from http://d.hatena.ne.jp/end0tknr/20080719/1216417270
(defface my-face-b-1 '((t (:background "bisque"))) nil)
(defface my-face-b-2 '((t (:background "LemonChiffon2"))) nil)
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) nil)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(("　" 0 my-face-b-1 append)
     ("\t" 0 my-face-b-2 append)
     ("[ \t]+$" 0 my-face-u-1 append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)

;;; ---------------------------------------------------------------------------
;;; provide
;;; ---------------------------------------------------------------------------
(provide 'outsider-elisp)
;;; outsider-elisp.el ends here
