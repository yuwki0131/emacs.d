;;; modeline-conf.el --- packages
;;; Commentary:
;;;  モードライン設定 / modeline-conf.el
;;; Code:

;;; ---------------------------------------------------------------------------
;; no-saved / saved / read-only notification
;;; ---------------------------------------------------------------------------
(defvar fly-notice
  '(:eval (cond
           (buffer-read-only
            (propertize "□ 🚧"
                        'face '(:foreground "#FF4C00" :weight 'bold)
                        'help-echo "buffer is read-only !!!"))
           ((buffer-modified-p)
            (propertize "💾 ✗"
                        'face '(:foreground "deeppink")
                        'help-echo "buffer modified."))
           ((not (buffer-modified-p))
            (propertize "□ ✓"
                        'face '(:foreground "#0997B6")
                        'help-echo "no buffer modified.")))))

;;; ---------------------------------------------------------------------------
;;; tanakh(tanakov)-chain (´･_･`) ( ´･_･) (  ´･_)
;;; ---------------------------------------------------------------------------
(defvar tanakh-chain-list
  '("(´･_･`)"
    "( ´･_･)"
    "(  ´･_)"
    "(   ´･)"
    "(    ´)"
    "(      )"
    "(`     )"
    "(･`    )"
    "(_･`   )"
    "(･_･`  )"))

(defun insert-tanakh-chain ()
    (let ((i (% (cadr (current-time)) (length tanakh-chain-list))))
      (propertize (nth i tanakh-chain-list))))

;;; ---------------------------------------------------------------------------
;;; customized mode-line
;;; ---------------------------------------------------------------------------
(defvar mode-line-format-customized
  (list "%e "
        ;; mode-line-front-space
        ;; * エンコーディングシステム
        mode-line-mule-info
        ;; mode-line-client
        ;; * カレントバッファの修正通知 (fly-noticeがあるのでoff)
        ;; mode-line-modified
        ;; * 修正/readonly通知
        "  "  fly-notice "  "
        ;; mode-line-auto-compile
        ;; mode-line-remote
        ;; (:eval (mode-line-frame-control))
        ;; * バッファ名
        mode-line-buffer-identification
        "  "
        mode-line-position
        "  "
        '(:eval (insert-tanakh-chain))
        ;; * VC + modes
        ;; '(:eval (vc-mode vc-mode))
        ;; mode-line-modes
        ;; mode-line-misc-info
        ;; mode-line-end-spaces
        ))

;; カスタマイズ済みのフォーマットの設定
(setq-default mode-line-format mode-line-format-customized)

(setq global-mode-string
      '((t jabber-activity-mode-string)
        "" display-time-string appt-mode-string))

;;;---------------------------------------------------------------------------
;;; provide
;;;---------------------------------------------------------------------------
(provide 'modeline-conf)
;;; modeline-conf.el ends here
