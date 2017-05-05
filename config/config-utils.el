;;; package --- emacs config-utils.el
;;; Commentary:
;;;  config-utils.el
;;;  - spit (export files)
;;;  - use-package-with-report
;;;  - global-safe-set-key
;;;  - gssk-report
;;;  - configuration report
;;;  - generate readme
;;; Code:

;;; ---------------------------------------------------------------------------
;;; spit text to the file
;;; ---------------------------------------------------------------------------
(defun spit (file-name text)
  (ignore-errors
	(if (file-exists-p file-name)
		(delete-file file-name))
	(find-file file-name)
	(insert text)
	(save-buffer)
	(kill-buffer)
    t))

;;; ---------------------------------------------------------------------------
;;; interpose
;;; ---------------------------------------------------------------------------
(defun interpose (xs obj)
  (let ((size (length xs))
        (count 0)
        (ys (reverse xs))
        (zs '()))
    (while (not (null ys))
      (setq zs (cons obj (cons (car ys) zs)))
      (setq ys (cdr ys)))
    (cdr zs)))

;;; ---------------------------------------------------------------------------
;;; concat & interpose newline
;;; ---------------------------------------------------------------------------
(defun concat-interpose-newline (text-ls)
  (apply #'concat (interpose text-ls "\n")))

;;; ---------------------------------------------------------------------------
;;; interpose comment out
;;; ---------------------------------------------------------------------------
(defun add-comment-out (text)
  (concat ";;; " text))

(defun comment-out-message (text)
  (concat-interpose-newline
   (mapcar #'add-comment-out (split-string text "\n"))))

;;; ---------------------------------------------------------------------------
;;; failed-packages report : use-packageに失敗したパッケージのレポート
;;; ---------------------------------------------------------------------------
;; パッケージのLoading 状況をレポートする。 *scratch*バッファに結果出力
(defvar failed-packages '())

(defmacro use-package-with-report (&rest body)
  `(when (not (use-package . ,(append body '(:config 't))))
     (add-to-list 'failed-packages ,(symbol-name (car body)))))

(defun to-report-message (line)
  (concat "  - failed to load: " line))

(defun report-failed-packages ()
  (if (not failed-packages)
      "all defined packages have been installed successfully"
    (concat
     "use-package-with-report error or not used packages: \n"
     (concat-interpose-newline
      (mapcar #'to-report-message failed-packages)))))

(defun generate-package-install-scinario ()
  (if failed-packages
      (spit "~/.emacs.d/install-scinario"
            (concat-interpose-newline failed-packages))))

(font-lock-add-keywords 'emacs-lisp-mode
  '(("\\(use-package-with-report\\)" . font-lock-keyword-face)))

;;; ---------------------------------------------------------------------------
;;; global-safe-set-key : 安全なglobalsetkeyとエラーレポート、キーバインドレポート
;;; ---------------------------------------------------------------------------

;; キーバインド情報用(標準以外)
;; ((category binding-key emacs-function-name function-explaination))
(defvar gssk-keybind-report '())

(defvar gssk-current-category-state "")

(defvar gssk-current-subcategory-state "")

(defvar gssk-current-function-name-state "")

(defun gssk-category
  (text)
  (setq gssk-current-category-state text))

(defun gssk-subcategory
  (text)
  (setq gssk-current-subcategory-state text))

(defun gssk-explain-function
  (text)
  (setq gssk-current-function-name-state text))

(defun gssk-category-function
  (category-text subcategory-text function-text)
  (setq gssk-current-category-state category-text)
  (setq gssk-current-subcategory-state subcategory-text)
  (setq gssk-current-function-name-state function-text))

(defvar gsskey-report-text nil)

(defun gssk-add-keybind-report
 (keybind-str sym)
 (add-to-list
  'gssk-keybind-report
  (list gssk-current-category-state
		gssk-current-subcategory-state
		keybind-str (symbol-name sym)
		gssk-current-function-name-state)))

(defmacro gssk-bind (keybind-str sym)
  `(cond
    ((fboundp ,sym)
	 (progn
	   (gssk-add-keybind-report ,keybind-str ,sym)
	   (global-set-key (kbd ,keybind-str) ,sym)))
    (t
     (setq gsskey-report-text
	   (concat gsskey-report-text
               "  - failed to bind: "
               (symbol-name ,sym)
               "\n")))))

(defun report-gsskey ()
  (if (not gsskey-report-text)
      "all keybindings defined successfully"
    (concat " gsskey error: \n" gsskey-report-text)))

;;; ---------------------------------------------------------------------------
;;; gssk : binding report
;;; ---------------------------------------------------------------------------
(defconst grm-keybind-header
  "|分類1|分類2|キー|関数名|内容|")

(defconst grm-keybind-table-line
  (concat
   "| -------- |:----|:-------- "
   "| -------------------- |:-------|"))

(defun gssk-setting-md "")

(defun generate-explanation-text ()
  (apply 'concat
		 (mapcar '(lambda (x) (concat "|" (car x)
									  "|" (car (cdr x))
									  "|" (car (cdr (cdr x)))
									  "|" (car (cdr (cdr (cdr x))))
									  "|" (car (cdr (cdr (cdr (cdr x))))) "|\n"))
				 (reverse gssk-keybind-report))))

(defun keybinding-md ()
  (concat-interpose-newline
   (list grm-keybind-header
         grm-keybind-table-line
         (generate-explanation-text))))

;;; ---------------------------------------------------------------------------
;;; configuration report
;;; ---------------------------------------------------------------------------
(defun report-configuration ()
  (insert
   (concat
    ;; fortune message
    (ignore-errors
      (shell-command-to-string "fortune | rev | cowsay -f ghostbusters" ))
    (comment-out-message
     (concat-interpose-newline
      (list
       (concat-interpose-newline
        '("hello world, emacs !!" "('･_･`) ↓" "reports in loading init.el"))
       (report-failed-packages)
       (report-gsskey)))))))

;;; ---------------------------------------------------------------------------
;;; generate readme
;;; ---------------------------------------------------------------------------
(defvar config-composition-md
  "~/.emacs.d/configディレクトリ以下

|*.el file|設定|
|:-------------|:------------------------------------------------------|
| config-util | configファイル用のユーティリティ |
| package-conf | 外部パッケージ(elpaからパッケージ要取得)の設定項目 |
| bizz-conf | emacsデフォルト(elpaからパッケージの取得が不要)の設定項目 |
| appearance-conf | bizzに引続き、emacsデフォルトの外見設定 |
| common-lang-conf | 言語共通設定 or 複数言語に共通する設定(要elpaの設定) |
| language-conf | 特定の言語設定、1言語ごとの設定 |
| external-eslip | 外部から持ち込んだコードなど |
| internal-eslip | 自作したコード |
| key-binding | キーバインドは一括してここにまとめる |")

(defconst readme-file-md "~/.emacs.d/README.md")

(defun generate-readme-text ()
  (concat
   ;; header
   "# 自分用 ~/.emacs.d\n\n修正中 & 未確認 (´・_・`)\n\n"
   "以下イメージ\n\n"
   "![画面](img/image.png)\n\n"
   ;; read-me text
   "修正して使う.
設定など.
```
$ git clone https://github.com/yuwki0131/emacs.d
$ mv emacs.d ~/.emacs.d
```
### 要use-package

```
M-x package-install use-package
```

### 要fortune, cowsayコマンド

```
$ sudo apt-get install fortune cowsay
```
"
   ;; config composition
   "\n## elispファイル構成\n\n"
   config-composition-md
   ;; explain keybinds
   "\n\n## キーバインド\n\n"
   "デフォルト以外のglobal-set-key設定\n\n"
   (keybinding-md)))

;;; --------------------------------------------------------------------------------
;;; provide
;;; --------------------------------------------------------------------------------
(provide 'config-utils)
;;; config-utils.el ends here
