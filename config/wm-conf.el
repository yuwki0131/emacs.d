;;; package --- wm-conf.el
;;; Commentary:
;;;  WindowManager用の諸設定 / wm-conf.el
;;; Code:

;; ---------------------------------------------------------------------------
;; exwm (emacsによるwindows manager)
;; ---------------------------------------------------------------------------
;; exwm

(require 'exwm)
(require 'exwm-config)
(exwm-config-default)
(setq exwm-workspace-show-all-buffers t)
(setq exwm-layout-show-all-buffers t)

(require 'exwm-randr)
(setq exwm-randr-workspace-output-plist '(0 "VGA1"))
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output VGA1 --left-of LVDS1 --auto")))
(exwm-randr-enable)

;; on your magic
;; setxkbmap -option ctrl:swapcaps

;;; ---------------------------------------------------------------------------
;;; provide
;;; ---------------------------------------------------------------------------
(provide 'wm-conf)
;;; wm-conf.el ends here
