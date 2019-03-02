;;; package --- builtin-conf.el
;;; Commentary:
;;;  デフォルト機能の諸設定 / bizz-conf.el
;;; Code:

;; ツールバー非表示
(tool-bar-mode -1)

;; メニューバー表示(Linux Mint/Cinnamonだと綺麗な表示)
(menu-bar-mode -1)

;; スクロールバー非表示
(scroll-bar-mode -1)

;; 画像ファイル表示
(auto-image-file-mode t)

;; 列数を表示
(column-number-mode t)

;; バッファ自動再読み込み
(global-auto-revert-mode t)

;; 行番号を表示(標準) => 使用しない
;; (global-linum-mode t)

;; beep音消す => ミニバッファへ反映
;; (setq visible-bell t)
;; (setq ring-bell-function 'ignore)

;; マークのリージョンに色を付ける
;; (setq transient-mark-mode t)

(setq
 ;; scratchの初期のメッセージ消去
 initial-scratch-message nil
 ;; backupfile(*.~) つくらない
 make-backup-files nil
 ;; backupfile(*.# つくらない)
 auto-save-default nil
 ;; 折り返しを表示
 truncate-lines t
 ;; 折り返しを表示(ウインドウ分割時)
 truncate-partial-width-windows nil
 ;; スタートメッセージを非表示
 inhibit-startup-message t
 ;; file名の補完で大文字小文字を区別しない
 completion-ignore-case t
 ;; 最終行に1行挿入
 require-final-newline t
 ;; 再帰的ミニバッファ
 enable-recursive-minibuffers t
 ;; ファイル読込補完、大文字/小文字無視
 read-file-name-completion-ignore-case t
 ;; デフォルト起動時の画面非表示
 inhibit-startup-message t
 ;; 規則文字のdisable
 enable-kinsoku nil
 )

;; バッファの終端を表示(空行表示)
(setq-default indicate-empty-lines t)

;; インデントでタブを挿入しない
(setq-default indent-tabs-mode nil)

;; デフォルトインデント幅: 2
(setq standard-indent 2)

;; 再帰的ミニバッファの深さを表示
(minibuffer-depth-indicate-mode 1)

;; from yes-or-no to y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; ファイル保存時に空白削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; すべてのプログラムモードに対してタブ幅設定
(add-hook 'prog-mode-hook '(lambda () (setq tab-width 2)))

;; ダイアログボックス非表示
(defalias 'message-box 'message)
(setq use-dialog-box nil)

;; URL強調表示、URLをブラウザで表示
(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(add-hook 'text-mode-hook 'goto-address-mode)

;; ‘isearch-word’ is an obsolete variable (as of 25.1)対策
(defvar search-default-regexp-mode nil)

;;; 画像ファイル表示
(auto-image-file-mode t)

;; find-file時に、elcファイルを無視
(setq counsel-find-file-ignore-regexp "\\.elc\\'")

;;; ---------------------------------------------------------------------------
;;; デフォルトエンコーディング
;;; ---------------------------------------------------------------------------
(set-language-environment "Japanese")
(set-terminal-coding-system   'utf-8)
(prefer-coding-system         'utf-8)
(setq coding-system-for-read  'utf-8
      coding-system-for-write 'utf-8
      default-process-coding-system '(utf-8 . utf-8))
(setenv "LANG" "ja_JP.UTF-8")

;;; ---------------------------------------------------------------------------
;;; 別ディレクトリの同名バッファにディレクトリ名を付与する
;;; ---------------------------------------------------------------------------
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;; ---------------------------------------------------------------------------
;;; 最近開いたファイルを再度開く
;;; ---------------------------------------------------------------------------
(setq
 ;; 2000ファイルまで履歴保存する
 recentf-max-saved-items 2000
 ;; 存在しないファイルは消さない
 recentf-auto-cleanup 'never
 ;; resentfで無視するファイル
 recentf-exclude '("/recentf")
 recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))

;; enable
(recentf-mode 1)

;;; ---------------------------------------------------------------------------
;;; 特定のバッファではlinum-modeをoff
;;; ---------------------------------------------------------------------------
(defvar disable-nlinum-hooks
  '(shell-mode-hook eshell-mode-hook emacs-startup-hook grep-mode-hook))

(while (not (null disable-nlinum-hooks))
  (add-hook (car disable-nlinum-hooks) (lambda () (nlinum-mode -1)))
  (setq disable-nlinum-hooks (cdr disable-nlinum-hooks)))

;;; ---------------------------------------------------------------------------
;;; provide
;;; ---------------------------------------------------------------------------
(provide 'builtin-conf)
;;; builtin-conf.el ends here
