;; ---------------------------------------------------------------------------
;; パッケージマネージャ
(require 'package)

(add-to-list
 'package-archives
 '("melpa"          . "https://melpa.milkbox.net/packages/") t)

(package-initialize)

;; ---------------------------------------------------------------------------
;; 諸設定
(setq inhibit-startup-message t) ;; 起動時の画面は、いらない
(tool-bar-mode -1)               ;; ツールバーは、いらない
(menu-bar-mode -1)               ;; メニューバーは、いらない

;; beep音消す
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; backupfileつくらない
(setq make-backup-files nil)
(setq auto-save-default nil)

;; 折り返しを表示
(setq truncate-lines t)

;; 行番号を表示
(global-linum-mode t)

;; 行番号フォーマット
(setq linum-format " %4d ")

;; file名の補完で大文字小文字を区別しない
(setq completion-ignore-case t)

;; バッファ自動再読み込み
(global-auto-revert-mode 1)

;; カーソルタイプ
(setq default-cursor-type '(bar . 2))

;; エンコーディング
(set-language-environment "Japanese")
(setq default-buffer-file-coding-system 'utf-8)

;; フォント
(set-default-font "Ubuntu Mono-8")

;;---------------------------------------------------------------------------
;; 行末のwhitespaceを削除
(setq delete-trailing-whitespace-exclude-patterns
      (list "\\.md$" "\\.markdown$"))
(require 'cl)
(defun delete-trailing-whitespace-with-exclude-pattern ()
  (interactive)
  (cond
   ((equal
     nil
     (loop for pattern in delete-trailing-whitespace-exclude-patterns
           thereis (string-match pattern buffer-file-name)))
    (delete-trailing-whitespace))))
(add-hook 'before-save-hook
          'delete-trailing-whitespace-with-exclude-pattern)

;;---------------------------------------------------------------------------
;; ファイル末尾の改行を削除
;; http://www.emacswiki.org/emacs/DeletingWhitespace
(defun my-delete-trailing-blank-lines ()
  "Deletes all blank lines at the end of the file."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (delete-blank-lines))))

(add-hook 'before-save-hook 'my-delete-trailing-blank-lines)

;; ---------------------------------------------------------------------------
;; 普通のredo

(require 'redo+)
 (setq undo-no-redo t)
 (setq undo-limit 60000)
 (setq undo-strong-limit 90000)

;; ---------------------------------------------------------------------------
;; アスタリスク付バッファは飛ばす

(defun astarisked? (buf-name)
  (= 42 (car (string-to-list buf-name))))

(defun next-buffer-with-skip* ()
  (interactive)
  (let ((current-buffer-name (buffer-name)))
    (next-buffer)
    (while (and (astarisked? (buffer-name))
                (not (string= current-buffer-name (buffer-name))))
      (next-buffer))))

(defun previous-buffer-with-skip* ()
  (interactive)
  (let ((current-buffer-name (buffer-name)))
    (previous-buffer)
    (while (and (astarisked? (buffer-name))
                (not (string= current-buffer-name (buffer-name))))
      (previous-buffer))))

;;---------------------------------------------------------------------------

(global-unset-key "\C-e")

(global-unset-key "\C-a")

(global-unset-key "\C-z") ;; (prev ctrl-zでwindowが最小化) しない

;;---------------------------------------------------------------------------
;; prefix なし
(global-set-key (kbd "C-S-k") 'backward-kill-line)

(global-set-key "\C-h"   'delete-backward-char)
(global-set-key "\M-h"   'backward-kill-word)

(global-set-key "\C-q"   'undo)
(global-set-key "\M-q"   'redo)

;; カーソル位置固定のままスクロール
(global-set-key "\M-p"   'scroll-up-in-place)
(global-set-key "\M-n"   'scroll-down-in-place)

;; 置換
(global-set-key "\C-z\C-r" 'replace-string)

;; コメントアウト
(global-set-key "\C-a\C-a" 'comment-dwim)

;; Shell
(global-set-key "\C-e\C-c" 'shell)

;; バッファ移動 (アスタリスク付バッファはスキップ)
(global-set-key "\C-e\C-b" 'previous-buffer-with-skip*)
(global-set-key "\C-e\C-f" 'next-buffer-with-skip*)
