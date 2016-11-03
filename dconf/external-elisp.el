;;---------------------------------------------------------------------------
;;---------------------------------------------------------------------------
;;
;; 外部elisp(package化されてないもの) / external-elisp.el
;;
;;---------------------------------------------------------------------------
;;---------------------------------------------------------------------------

;;---------------------------------------------------------------------------
;; 行末のwhitespaceを削除 (from http://qiita.com/scalper/items/12b211b246dfbcb6bc6d)
;;---------------------------------------------------------------------------
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
;; ファイル末尾の改行を削除 (from http://www.emacswiki.org/emacs/DeletingWhitespace)
;;---------------------------------------------------------------------------
(defun my-delete-trailing-blank-lines ()
  "Deletes all blank lines at the end of the file."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (delete-blank-lines))))

(add-hook 'before-save-hook 'my-delete-trailing-blank-lines)

;;---------------------------------------------------------------------------
;; provide
;;---------------------------------------------------------------------------
(provide 'external-elisp)
