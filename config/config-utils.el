;;; package --- emacs config-utils.el
;;; Commentary:
;;;  config-utils.el
;;; Code:

;;; --------------------------------------------------------------------------------
;;; spit text to the file
;;; --------------------------------------------------------------------------------
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
;;; failed-packages report : use-packageに失敗したパッケージのレポート
;;; ---------------------------------------------------------------------------
;; パッケージのLoading 状況をレポートする。 *scratch*バッファに結果出力
(defvar failed-packages '())

(defmacro use-package-with-report (&rest body)
  `(when (not (use-package . ,(append body '(:config 't))))
     (add-to-list 'failed-packages ,(symbol-name (car body)))))

(defun interpose (ls obj)
  (if (null ls)
      nil
    `(,(car ls) ,obj . ,(interpose (cdr ls) obj))))

(defun to-report-message (line)
  (concat ";;  - failed to load: " line))

(defun report-failed-packages ()
  (if (not failed-packages)
      ";; all defined packages have been installed successfully"
    (concat
     ";; use-package-with-report error: \n"
     (apply 'concat
	    (interpose (mapcar #'to-report-message failed-packages) "\n")))))

(font-lock-add-keywords 'emacs-lisp-mode
  '(("\\(use-package-with-report\\)" . font-lock-keyword-face)))

;;; --------------------------------------------------------------------------------
;;; provide
;;; --------------------------------------------------------------------------------
(provide 'config-utils)
;;; config-utils.el ends here
