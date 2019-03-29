;;; package --- Emacs util-elip.el
;;; Commentary:
;;;  util-elisp.el
;;;  - spit (export files)
;;;  - use-package-with-report
;;;  - global-safe-set-key
;;;  - gssk-report
;;;  - configuration report
;;;  - generate README.md
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

(defun slurp (file-name)
  (ignore-errors
	  (if (not (file-exists-p file-name))
		    (concat "file-not-found in slurp: " file-name)
      (with-temp-buffer
        (insert-file-contents file-name)
        (buffer-string)))))

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
;;; wget-construct-package : wgetで*.elをDownload、ロード、configを記述
;;; ---------------------------------------------------------------------------
(defmacro wconst-pakcage (name url-string &rest body)
  (let* ((file-name (car (last (split-string url-string "/"))))
         (file-path (concat "~/.emacs.d/wrepo/" file-name))
         (exist-that? (file-exists-p file-path)))
    (when (not exist-that?)
      (cd "~/.emacs.d/wrepo/")
      (shell-command-to-string (concat "wget " url-string))))
  `(progn (require ,name) . ,body))

;;; ---------------------------------------------------------------------------
;;; git-package : git cloneで*.elをDownload、ロード、configを記述
;;; ---------------------------------------------------------------------------
(defmacro git-package (name-repo-save-path body)
  (let* ((name (car name-repo-save-path))
         (git-repository (car (cdr name-repo-save-path)))
         (save-path (car (cdr (cdr name-repo-save-path))))
         (file-path (concat "~/.emacs.d/gitrepo/" save-path "/"))
         (exist-that? (file-directory-p file-path)))
    (print "git-packages")
    `(progn
       (when (not ,exist-that?)
         (cd "~/.emacs.d/gitrepo/")
         (shell-command (concat "git clone " ,git-repository)))
       (add-to-list 'load-path ,file-path)
       (require (quote ,name))
       ,body)))

;;; ---------------------------------------------------------------------------
;;; failed-packages ignore&report : ignore-errorでエラーが発生した箇所のログを取る
;;; ---------------------------------------------------------------------------

(defvar ignore&report-log-file "~/.emacs.d/error.log")

(defvar ignore&report-error-seq '())

(defmacro ignore-report (&rest body)
  `(let ((report (condition-case err
                     (progn (progn . ,body) "")
                   (error (format "\nThe error was: %s" err)))))
     (when (not (string= "" report))
       (setq ignore&report-error-seq
             (cons (concat report (format "\nsexp=%s\n" (quote (,body))))
                   ignore&report-error-seq)))))

(defun report-ignore&report ()
  (delete-file ignore&report-log-file)
  (when ignore&report-error-seq
    (spit ignore&report-log-file (apply 'concat ignore&report-error-seq))))

;;; ---------------------------------------------------------------------------
;;; failed-packages report : use-packageに失敗したパッケージのレポート
;;; ---------------------------------------------------------------------------
;; パッケージのLoading 状況をレポートする。 *scratch*バッファに結果出力
(defvar failed-packages '())

(defvar other-configuration-reports '())

(defmacro use-package-with-report (package-name &rest body)
  `(progn
     (use-package ,package-name . ,body)
     (when (not (package-installed-p (quote ,package-name)))
       (add-to-list 'failed-packages ,(symbol-name package-name)))))

(defmacro ignore-require-with-report (comment-if-failed &rest body)
  `(progn
     (if (not (ignore-errors . ,(append body '(t))))
         (setq other-configuration-reports
               (cons ,comment-if-failed other-configuration-reports)))))

(defun to-report-message (line)
  (concat "  - failed to load: " line))

(defun report-failed-packages ()
  (if (and (not failed-packages) (not other-configuration-reports))
      "all defined packages have been installed successfully"
    (concat
     "use M-x: install-complements \n"
     "use-package-with-report error or not used packages: \n"
     (concat-interpose-newline
      (mapcar #'to-report-message failed-packages))
     (if failed-packages "\n" "")
     (concat-interpose-newline other-configuration-reports))))

(defun to-package-install-sexp (text)
  (concat "(ignore-errors (package-install '" text "))"))

(defun generate-package-install-scenario ()
  (if failed-packages
      (let* ((failed-ls (mapcar #'to-package-install-sexp failed-packages))
             (scenario (concat-interpose-newline failed-ls)))
        (spit "~/.emacs.d/install-scenario.el" scenario))))

(font-lock-add-keywords
 'emacs-lisp-mode
 '(("\\(use-package-with-report\\)" . font-lock-keyword-face)))

(defun install-complements ()
  (interactive)
  (load "~/.emacs.d/install-scenario.el"))

;;; ---------------------------------------------------------------------------
;;; global-safe-set-key : 安全なglobalsetkeyとエラーレポート、キーバインドレポート
;;; ---------------------------------------------------------------------------

;; キーバインド情報用(標準以外)
(defvar gssk-keybind-report '())

(defvar gssk-current-category-state "")

(defvar gssk-current-subcategory-state "")

(defvar gssk-current-function-name-state "")

(defun gssk-category (text)
  (setq gssk-current-category-state text))

(defun gssk-subcategory (text)
  (setq gssk-current-subcategory-state text))

(defun gssk-explain-function (text)
  (setq gssk-current-function-name-state text))

(defun gssk-category-function
  (category-text subcategory-text function-text)
  (setq gssk-current-category-state category-text)
  (setq gssk-current-subcategory-state subcategory-text)
  (setq gssk-current-function-name-state function-text))

(defun gssk-subcategory-function
  (subcategory-text function-text)
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
      "all key-bindings defined successfully"
    (concat " gsskey error: \n" gsskey-report-text)))

;;; ---------------------------------------------------------------------------
;;; gssk : binding report
;;; ---------------------------------------------------------------------------
(defconst grm-keybind-header
  "|分類1|分類2|キー|関数名|内容|")

(defconst grm-keybind-table-line
  "| -------- |:----|:-------- | -------------------- |:-------|")

(defvar gssk-setting-md "")

(defun generate-explanation-text ()
  (apply 'concat
		     (mapcar #'(lambda (x) (concat "|" (car x)
									                     "|" (car (cdr x))
									                     "|" (car (cdr (cdr x)))
									                     "|" (car (cdr (cdr (cdr x))))
									                     "|" (car (cdr (cdr (cdr (cdr x))))) "|\n"))
				         (reverse gssk-keybind-report))))

(defun key-binding-md ()
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
(defconst readme-file-md "~/.emacs.d/README.md")

(defun generate-readme-text ()
  (concat
   (slurp "~/.emacs.d/datafiles/README.template.md")
   ;; explain key binds
   "\n## キーバインド\n\n"
   "デフォルト以外のglobal-set-key設定\n\n"
   (key-binding-md)))

;;; --------------------------------------------------------------------------------
;;; provide
;;; --------------------------------------------------------------------------------
(provide 'util-elisp)
;;; util-elisp.el ends here
