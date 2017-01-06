;;; ---------------------------------------------------------------------------
;;; ---------------------------------------------------------------------------
;;;
;;; use-package checker / use-package-checker.el
;;;
;;; use-pakcage-checker checks whether the use-package has returned true or not
;;;
;;; ---------------------------------------------------------------------------
;;; ---------------------------------------------------------------------------

(defvar use-package-checker-report "")

(defun append-fail-report (sym)
  (concat use-package-checker-report
	  (symbol-name sym)
	  " : failed to load" "\n"))

(defmacro use-package-with-check (package-name & config)
  (let ((package-name-desc (symbol-name package-name)))
    `(if (use-package ,package-name . config)
	 t
       (append-fail-report package-name-desc))))
