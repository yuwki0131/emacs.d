;;; package --- helm-binding.el
;;; Commentary:
;;;  helmキーバインド設定
;;;  search-package-conf.elから呼び出す
;;;  global-safe-set-key from config-utils
;;; Code:

;;; ---------------------------------------------------------------------------
;;; alternate helm keybindings
;;; ---------------------------------------------------------------------------

;; alternate find-files
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; alternate M-x
;; (global-set-key (kbd "M-x") 'helm-M-x)

;; helmのfind-fileで編集
(define-key helm-read-file-map (kbd "C-h") 'delete-backward-char)
(define-key helm-read-file-map (kbd "<tab>") 'helm-execute-persistent-action)

;;; ---------------------------------------------------------------------------
;;; E-Z prefix (to do helm eazily)
;;; ---------------------------------------------------------------------------

(gssk-category "helm")

(gssk-subcategory "バッファ間")

(gssk-explain-function "バッファ一覧表示")
(gssk-bind "C-e C-z C-b" 'helm-buffers-list)

(gssk-subcategory "キルリング")

(gssk-explain-function "キルリング表示")
(gssk-bind "C-e C-z C-b" 'helm-show-kill-ring)

;;; ---------------------------------------------------------------------------
;;; provide
;;; ---------------------------------------------------------------------------
(provide 'helm-binding)
;;; helm-binding.el ends here


;;     helm--minor-mode 	helm--remap-mouse-mode
;;     helm-adaptive-mode
;;     helm-adaptive-save-history 	helm-addressbook-bookmarks
;; helm-all-mark-rings 	helm-apropos
;; helm-apt 	helm-autoresize-mode
;; helm-beginning-of-buffer 	helm-bookmark-rename
;; helm-bookmark-run-browse-project 	helm-bookmark-run-delete
;; helm-bookmark-run-edit 	helm-bookmark-run-jump-other-window
;; helm-bookmark-toggle-filename 	helm-bookmarks
;; helm-browse-project 	helm-browse-url-chromium
;; helm-browse-url-conkeror 	helm-browse-url-firefox
;; helm-browse-url-opera 	helm-browse-url-uzbl
;; helm-buffer-diff-persistent 	helm-buffer-revert-persistent
;; helm-buffer-run-ediff 	helm-buffer-run-ediff-merge
;; helm-buffer-run-grep 	helm-buffer-run-kill-buffers
;; helm-buffer-run-kill-persistent 	helm-buffer-run-query-replace
;; helm-buffer-run-query-replace-regexp 	helm-buffer-run-zgrep
;; helm-buffer-save-persistent 	helm-buffer-switch-other-frame
;; helm-buffer-switch-other-window 	helm-buffers-list
;; helm-buffers-mark-similar-buffers 	helm-buffers-run-browse-project
;; helm-buffers-run-multi-occur 	helm-buffers-toggle-show-hidden-buffers
;;     helm-calcul-expression 	helm-colors
;; helm-comint-input-ring 	helm-complete-file-name-at-point
;; helm-complex-command-history 	helm-configuration
;; helm-confirm-and-exit-minibuffer 	helm-copy-to-buffer
;; helm-cr-empty-string 	helm-customize-group
;; helm-cycle-resume 	helm-dabbrev
;; helm-debug-open-last-log 	helm-debug-output
;; helm-delete-minibuffer-contents 	helm-delete-tramp-connection
;; helm-display-all-sources 	helm-do-grep-ag
;; helm-documentation 	helm-enable-or-switch-to-debug
;; helm-end-of-buffer 	helm-enlarge-window
;; helm-esh-pcomplete 	helm-eshell-history
;; helm-etags-run-switch-other-frame 	helm-etags-run-switch-other-window
;; helm-etags-select 	helm-eval-expression
;; helm-eval-expression-with-eldoc 	helm-execute-kmacro
;; helm-execute-persistent-action 	helm-ff-RET
;; helm-ff-bookmark-set 	helm-ff-delete-char-backward
;; helm-ff-file-name-history 	helm-ff-persistent-delete
;; helm-ff-properties-persistent 	helm-ff-rotate-left-persistent
;; helm-ff-rotate-right-persistent 	helm-ff-run-browse-project
;; helm-ff-run-byte-compile-file 	helm-ff-run-complete-fn-at-point
;; helm-ff-run-copy-file 	helm-ff-run-delete-file
;; helm-ff-run-ediff-file 	helm-ff-run-ediff-merge-file
;; helm-ff-run-eshell-command-on-file 	helm-ff-run-etags
;; helm-ff-run-find-alternate-file 	helm-ff-run-find-file-as-root
;; helm-ff-run-find-sh-command 	helm-ff-run-gid
;; helm-ff-run-git-grep 	helm-ff-run-grep
;; helm-ff-run-grep-ag 	helm-ff-run-hardlink-file
;; helm-ff-run-insert-org-link 	helm-ff-run-kill-buffer-persistent
;; helm-ff-run-load-file 	helm-ff-run-locate
;; helm-ff-run-mail-attach-files 	helm-ff-run-open-file-externally
;; helm-ff-run-open-file-with-default-tool 	helm-ff-run-pdfgrep
;; helm-ff-run-print-file 	helm-ff-run-query-replace
;; helm-ff-run-query-replace-on-marked 	helm-ff-run-query-replace-regexp
;; helm-ff-run-rename-file 	helm-ff-run-switch-other-frame
;; helm-ff-run-switch-other-window 	helm-ff-run-switch-to-eshell
;; helm-ff-run-switch-to-history 	helm-ff-run-symlink-file
;; helm-ff-run-toggle-auto-update 	helm-ff-run-toggle-basename
;; helm-ff-run-zgrep 	helm-filtered-bookmarks
;; helm-find 	helm-find-files
;; helm-find-files-down-last-level 	helm-find-files-toggle-to-bookmark
;; helm-find-files-up-one-level 	helm-follow-action-backward
;; helm-follow-action-forward 	helm-follow-mode
;; helm-for-files 	helm-gid
;; helm-global-mark-ring 	helm-gm-next-file
;; helm-gm-precedent-file 	helm-google-suggest
;; helm-goto-next-file 	helm-goto-precedent-file
;; helm-grep-do-git-grep 	helm-grep-mode
;; helm-grep-mode-jump 	helm-grep-mode-jump-other-window
;; helm-grep-mode-jump-other-window-backward 	helm-grep-mode-jump-other-window-forward
;; helm-grep-mode-mouse-jump 	helm-grep-run-default-action
;; helm-grep-run-other-frame-action 	helm-grep-run-other-window-action
;; helm-grep-run-save-buffer 	helm-help
;; helm-imenu 	helm-imenu-in-all-buffers
;; helm-info 	helm-info-Xlibscm
;; helm-info-ada-mode 	helm-info-at-point
;; helm-info-auth 	helm-info-autoconf-archive
;; helm-info-automake-1 	helm-info-autosprintf
;; helm-info-autotype 	helm-info-bc
;; helm-info-bovine 	helm-info-calc
;; helm-info-ccmode 	helm-info-cl
;; helm-info-com_err 	helm-info-coreutils
;; helm-info-dbus 	helm-info-dc
;; helm-info-diffutils 	helm-info-dired-x
;; helm-info-doclicense 	helm-info-dvips
;; helm-info-ebrowse 	helm-info-ed
;; helm-info-ede 	helm-info-ediff
;; helm-info-edt 	helm-info-efaq
;; helm-info-eieio 	helm-info-eintr
;; helm-info-elisp 	helm-info-emacs
;; helm-info-emacs-gnutls 	helm-info-emacs-mime
;; helm-info-enscript 	helm-info-epa
;; helm-info-erc 	helm-info-ert
;; helm-info-eshell 	helm-info-eudc
;; helm-info-evil 	helm-info-eww
;; helm-info-fastjar 	helm-info-fdl-1
;; helm-info-find 	helm-info-find-maint
;; helm-info-flex 	helm-info-flymake
;; helm-info-forms 	helm-info-geiser
;; helm-info-gettext 	helm-info-gnupg
;; helm-info-gnupg1 	helm-info-gnus
;; helm-info-gperf 	helm-info-gpl
;; helm-info-grep 	helm-info-grub
;; helm-info-grub-dev 	helm-info-gv
;; helm-info-gzip 	helm-info-haskell-mode
;; helm-info-htmlfontify 	helm-info-idlwave
;; helm-info-ido 	helm-info-info
;; helm-info-ivy 	helm-info-kpathsea
;; helm-info-libffi 	helm-info-m4
;; helm-info-macros 	helm-info-magit
;; helm-info-magit-popup 	helm-info-mairix-el
;; helm-info-menu 	helm-info-message
;; helm-info-mh-e 	helm-info-mmm
;; helm-info-mtools 	helm-info-nano
;; helm-info-nettle 	helm-info-newsticker
;; helm-info-nxml-mode 	helm-info-octave-mode
;; helm-info-org 	helm-info-pcl-cvs
;; helm-info-pgg 	helm-info-rcirc
;; helm-info-reftex 	helm-info-remember
;; helm-info-rluserman 	helm-info-sasl
;; helm-info-sc 	helm-info-scm
;; helm-info-sed 	bhelm-info-semantic
;; helm-info-ses 	helm-info-sieve
;; helm-info-slib 	helm-info-slime
;; helm-info-smtpmail 	helm-info-spd-say
;; helm-info-speech-dispatcher 	helm-info-speedbar
;; helm-info-srecode 	helm-info-ssip
;; helm-info-tds 	helm-info-time
;; helm-info-tlbuild 	helm-info-todo-mode
;; helm-info-top 	helm-info-tramp
;; helm-info-url 	helm-info-version
;; helm-info-vhdl-mode 	helm-info-vip
;; helm-info-viper 	helm-info-web
;; helm-info-web-server 	helm-info-web2c
;; helm-info-wget 	helm-info-widget
;; helm-info-wisent 	helm-info-with-editor
;; helm-info-woman 	helm-insert-latex-math
;; helm-jedi-related-names 	helm-keyboard-quit
;; helm-kill-selection-and-quit 	helm-lisp-completion-at-point
;; helm-lisp-completion-or-file-name-at-point 	helm-lisp-indent
;; helm-list-elisp-packages 	helm-list-elisp-packages-no-fetch
;; helm-list-emacs-process 	helm-locate
;; helm-locate-library 	helm-major-mode
;; helm-man-woman 	helm-manage-advice
;; helm-mark-all 	helm-mark-ring
;; helm-maybe-exit-minibuffer 	helm-migemo-mode
;; helm-mini 	helm-minibuffer-history
;; helm-moccur-mode 	helm-moccur-mode-goto-line
;; helm-moccur-mode-goto-line-ow 	helm-moccur-mode-goto-line-ow-backward
;; helm-moccur-mode-goto-line-ow-forward 	helm-moccur-mode-mouse-goto-line
;; helm-moccur-run-default-action 	helm-moccur-run-goto-line-of
;; helm-moccur-run-goto-line-ow 	helm-moccur-run-save-buffer
;; helm-mode 	helm-mouse-select-candidate
;; helm-multi-files 	helm-multi-files-toggle-to-locate
;; helm-multi-occur-from-isearch 	helm-narrow-window
;; helm-next-line 	helm-next-page
;; helm-next-source 	helm-next-visible-mark
;; helm-occur 	helm-occur-from-isearch
;; helm-org-agenda-files-headings 	helm-org-capture-templates
;; helm-org-in-buffer-headings 	helm-org-parent-headings
;; helm-popup-tip-mode 	helm-prev-visible-mark
;; helm-previous-line 	helm-previous-page
;; helm-previous-source 	helm-projects-find-files
;; helm-projects-history 	helm-push-mark-mode
;; helm-quit-and-find-file 	helm-quit-and-helm-mini
;; helm-ratpoison-commands 	helm-recenter-top-bottom-other-window
;; helm-recentf 	helm-refresh
;; helm-regexp 	helm-register
;; helm-reposition-window-other-window 	helm-reset-adaptive-history
;; helm-resume 	helm-resume-list-buffers-after-quit
;; helm-resume-previous-session-after-quit 	helm-run-cycle-resume
;; helm-run-external-command 	helm-scroll-other-window
;; helm-scroll-other-window-down 	helm-select-action
;; helm-select-xfont 	helm-semantic
;; helm-semantic-or-imenu 	helm-show-all-in-this-source-only
;; helm-show-kill-ring 	helm-stumpwm-commands
;; helm-surfraw 	helm-swap-windows
;; helm-timers 	helm-toggle-all-marks
;; helm-toggle-buffers-details 	helm-toggle-resplit-and-swap-windows
;; helm-toggle-resplit-window 	helm-toggle-suspend-update
;; helm-toggle-truncate-line 	helm-toggle-visible-mark
;; helm-top 	helm-top-poll-mode
;; helm-ucs 	helm-undo-yank-text-at-point
;; helm-unmark-all 	helm-wikipedia-show-summary
;; helm-wikipedia-show-summary-action 	phelm-wikipedia-suggest
;; helm-world-time 	helm-xrandr-set
;; helm-yank-selection 	helm-yank-text-at-point
