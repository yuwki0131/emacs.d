;;; package --- emacs config-utils.el
;;; Commentary:
;;;  config-utils.el
;;; Code:

(defun spit (file-name text)
  (ignore-errors
	(if (file-exists-p file-name)
		(delete-file file-name))
	(find-file file-name)
	(insert text)
	(save-buffer)
	(kill-buffer)
    t))

;;; --------------------------------------------------------------------------------
;;; provide
;;; --------------------------------------------------------------------------------
(provide 'config-utils)
;;; config-utils.el ends here
