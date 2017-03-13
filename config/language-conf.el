;;; package --- language-conf.el (specific settings for languages)
;;; Commentary:
;;;  言語設定 / language-conf.el
;;;  Lisp
;;;  - Gauche/Scheme
;;;  - Racket/Scheme
;;;  - Guile/Scheme
;;;  - SBCL/CommonLisp
;;;  - Clojure
;;;  - Hylang
;;;  Functional Programming Languages
;;;  - Haskell
;;;  - OCaml
;;;  - SML
;;;  - Erlang
;;;  Lightweight Languages
;;;  - Lua
;;;  - Python
;;;  Procedual Programming Languages
;;;  - C
;;;  Other Languages
;;;  - Prolog
;;;  - Vimrc
;;; Code:

;;; --------------------------------------------------------------------------------
;;; config : Lisp dialects
;;; --------------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; Gauche/Scheme (checked)
;;; ---------------------------------------------------------------------------
(setq scheme-program-name "/usr/bin/gosh")

;;; ---------------------------------------------------------------------------
;;; racket-mode : Racket/Scheme (checked)
;;; ---------------------------------------------------------------------------
(use-package-with-report racket-mode
  :config
  (add-hook 'racket-mode-hook
  '(lambda ()
     (define-key racket-mode-map (kbd "C-c r") 'racket-run))))

;;; ---------------------------------------------------------------------------
;;; geiser-mode : Guile/Scheme (checked)
;;; ---------------------------------------------------------------------------
(use-package-with-report geiser
  :config
  (setq geiser-active-implementations '(guile)))

;;; ---------------------------------------------------------------------------
;;; Common Lisp (SBCL/SLIME)の設定 (checked)
;;; ---------------------------------------------------------------------------
(setq inferior-lisp-program "/usr/bin/sbcl")

(use-package-with-report slime
  :config
  (setq slime-contribs '(slime-fancy))
  (slime-setup '(slime-repl slime-fancy slime-banner slime-indentation)))

(use-package-with-report ac-slime
  :config
  (add-hook 'slime-mode-hook 'set-up-slime-ac)
  (add-hook 'slime-repl-mode-hook 'set-up-slime-ac))

;;; ---------------------------------------------------------------------------
;;; clojure-mode : Clojure (unchecked)
;;; ---------------------------------------------------------------------------
(use-package-with-report clojure-mode)
(use-package-with-report clj-refactor
  :config
  (defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  (add-hook 'clojure-mode-hook #'my-clojure-mode-hook))

;;; ---------------------------------------------------------------------------
;;; hy-mode : Hylang (checked)
;;; ---------------------------------------------------------------------------
(use-package-with-report hy-mode)

;;; --------------------------------------------------------------------------------
;;; config : Functional programming languages
;;; --------------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; haskell-mode : Haskell (checked)
;;; ---------------------------------------------------------------------------
(use-package-with-report inf-haskell)
(use-package-with-report haskell-mode
  :mode (("\\.hs$" . haskell-mode)
         ("\\.lhs$" . literate-haskell-mode))
  :config
  (setq haskell-program-name "ghci"))
(use-package-with-report haskell-cabal
  :mode (("\\.cabal\\'" . haskell-cabal-mode)))
(setq haskell-font-lock-symbols t)

;;; ---------------------------------------------------------------------------
;;; taureg-mode ocaml : OCaml (checked)
;;; ---------------------------------------------------------------------------
(use-package-with-report taureg-mode
  :mode (("\\.ml[iylp]?" . tuareg-mode)))
(use-package-with-report taureg-mode-run-ocaml)
(use-package-with-report ocamldebug)

;;; ---------------------------------------------------------------------------
;;; sml-mode : SML (checked)
;;; ---------------------------------------------------------------------------
;; 設定はSML/NJ
(use-package-with-report sml-mode
  :config
  (setq sml-program-name "sml"))

;;; ---------------------------------------------------------------------------
;;; erlang-mode : Erlang (unchecked)
;;; ---------------------------------------------------------------------------
;; erlangのinstall pathを指定 (設定場所が微妙)
(add-to-list 'load-path "/usr/lib/erlang/lib/tools-2.8.3/emacs")
(setq erlang-root-dir "/usr/local/otp")
(setq exec-path (cons "/usr/local/otp" exec-path))
(use-package-with-report erlang-start) ;; TODO : ここもまだ

;;; --------------------------------------------------------------------------------
;;; config : LL
;;; --------------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; lua-mode : Lua (checked)
;;; ---------------------------------------------------------------------------
;; luaのinstall pathを指定
(add-to-list 'load-path "/path/to/directory/where/lua-mode-el/resides")
(use-package-with-report lua-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))

;;; ---------------------------------------------------------------------------
;;; jedi, epc, elpy, flycheck : Python (unchecked)
;;; ---------------------------------------------------------------------------
(use-package-with-report python-environment)

(use-package-with-report jedi
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))

(use-package-with-report epc)
(use-package-with-report elpy)

;; PEP8 check
(use-package-with-report py-autopep8
  :config
  (setq py-autopep8-options '("--max-line-length=200"))
  (setq flycheck-flake8-maximum-line-length 200)
  (py-autopep8-enable-on-save))

(use-package-with-report flycheck-pyflakes
  :config
  (require 'tramp-cmds)
  (add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
  (setq flymake-python-pyflakes-executable "/usr/bin/pyflakes")
  (flymake-python-pyflakes-load)
  (setq flymake-gui-warnings-enabled t)
  (custom-set-variables
   '(flymake-python-pyflakes-extra-arguments
     (quote ("--max-line-length=120" "--ignore=E128"))))
  (defun flymake-pyflakes-init ()
    (when (not (subsetp (list (current-buffer)) (tramp-list-remote-buffers)))
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
			 'flymake-create-temp-inplace))
	     (local-file (file-relative-name
			  temp-file
			  (file-name-directory buffer-file-name))))
	(list "pyflakes" (list local-file)))))
  (add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-pyflakes-init)))

(defun my-python-mode ()
  (progn
    (setq indent-tabs-mode nil)
    (setq indent-level 4)
    (setq python-indent 4)
    (setq tab-width 4)
    (flycheck-mode 1)
    (setq imenu-create-index-function 'python-imenu-create-index)
    (flymake-mode t)))

(add-hook 'python-mode-hook 'my-python-mode)

;;; --------------------------------------------------------------------------------
;;; config : Other Languages
;;; --------------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; prolog-mode : Prolog (checked)
;;; ---------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.pl" . prolog-mode))
(setq prolog-program-name "swipl")
(setq prolog-consult-string "[user].\n")

;;; ---------------------------------------------------------------------------
;;; vimrc-mode : vimrc編集用 (checked)
;;; ---------------------------------------------------------------------------
(use-package-with-report vimrc-mode)

;;; ---------------------------------------------------------------------------------
;;; provide
;;; ---------------------------------------------------------------------------------
(provide 'language-conf)
;;; language-conf.el ends here
