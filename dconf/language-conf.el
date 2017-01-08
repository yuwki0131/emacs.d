;;;; ---------------------------------------------------------------------------
;;;; ---------------------------------------------------------------------------
;;;;
;;;; 言語設定 / language-conf.el
;;;;
;;;; ---------------------------------------------------------------------------
;;;; ---------------------------------------------------------------------------
;;;; language install & setup checklist (2016/11/03)
;;;; - Scheme implementations or dialects (Gauche, Racket, Guile ... etc)
;;;; - Common Lisp Implementations or dialects (SBCL, Allgero, Clsip, ... etc)
;;;; - The other lisps (Clojure, Hy, LFE, ... etc)
;;;; - on JVM (Scala, Groovy, Kotlin, Frege, Golo)
;;;; - Functional Languages (Haskell, Gofer, OCaml, SML/nj, SML#, Erlang)
;;;; - 2 (Elixir)
;;;; - Prolog
;;;; - Languages without emacs (Java, C#,  ... etc)
;;;; - JS (JavaScript, TypeScript, CoffeeScript ... etc)
;;;; - Maijor LLs (Python, Ruby, Perl, PHP, Lua, Awk,  ... etc)
;;;; - C (gcc, clang, ... etc)
;;;; - C-like (Go, Dart, )
;;;; - OOP (Eiffel, Self)
;;;; - DSLs (SQL, tex, XML, HTML, ... etc)
;;;; - The other languages (Mathematica, Mel, Nim, Tcl/Tk, APL, Julia   ..)
;;;; memo : プログラムのディレクトリは環境毎に異なるのでインストール時にチェック.
;;;; TODO : コメント欄修正
;;;; TODO : Coq
;;;; TODO : unchecked要修正

;;; ---------------------------------------------------------------------------
;;; Gauche/Scheme (checked)
;;; ---------------------------------------------------------------------------
(setq scheme-program-name "/usr/bin/gosh")

;;; ---------------------------------------------------------------------------
;;; racket-mode : Racket/Scheme (checked)
;;; ---------------------------------------------------------------------------
(use-package racket-mode
  :config
  (add-hook 'racket-mode-hook
  '(lambda ()
     (define-key racket-mode-map (kbd "C-c r") 'racket-run))))

;;; ---------------------------------------------------------------------------
;;; geiser-mode : Guile/Scheme (checked)
;;; ---------------------------------------------------------------------------
(use-package geiser
  :config
  (setq geiser-active-implementations '(guile)))

;;; ---------------------------------------------------------------------------
;;; Common Lisp (SBCL/SLIME)の設定 (checked)
;;; ---------------------------------------------------------------------------
(setq inferior-lisp-program "/usr/bin/sbcl")

(use-package slime
  :config
  (setq slime-contribs '(slime-fancy))
  (slime-setup '(slime-repl slime-fancy slime-banner slime-indentation)))

(use-package ac-slime
  :config
  (add-hook 'slime-mode-hook 'set-up-slime-ac)
  (add-hook 'slime-repl-mode-hook 'set-up-slime-ac))

;;; ---------------------------------------------------------------------------
;;; clojure-mode : Clojure (unchecked)
;;; ---------------------------------------------------------------------------
(use-package clojure-mode)
(use-package clj-refactor
  :config
  (defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  (add-hook 'clojure-mode-hook #'my-clojure-mode-hook))

;;; ---------------------------------------------------------------------------
;;; hy-mode : Hylang (checked)
;;; ---------------------------------------------------------------------------
(use-package hy-mode)

;;; ---------------------------------------------------------------------------
;;; haskell-mode : Haskell (checked)
;;; ---------------------------------------------------------------------------
(use-package inf-haskell)
(use-package haskell-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
  (add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
  (setq haskell-program-name "ghci"))
(use-package haskell-cabal
  :config
  (add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode)))

;;; ---------------------------------------------------------------------------
;;; taureg-mode ocaml : OCaml (checked)
;;; ---------------------------------------------------------------------------
(use-package taureg-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . tuareg-mode)))
(use-package taureg-mode-run-ocaml)
(use-package ocamldebug)

;;; ---------------------------------------------------------------------------
;;; sml-mode : SML (checked)
;;; ---------------------------------------------------------------------------
;; 設定はSML/NJ
(use-package sml-mode
  :config
  (setq sml-program-name "sml"))

;;; ---------------------------------------------------------------------------
;;; erlang-mode : Erlang (unchecked)
;;; ---------------------------------------------------------------------------
;; erlangのinstall pathを指定 (設定場所が微妙)
(add-to-list 'load-path "/usr/lib/erlang/lib/tools-2.8.3/emacs")
(setq erlang-root-dir "/usr/local/otp")
(setq exec-path (cons "/usr/local/otp" exec-path))
(use-package erlang-start) ;; TODO : ここもまだ

;;; ---------------------------------------------------------------------------
;;; prolog-mode : Prolog (checked)
;;; ---------------------------------------------------------------------------
(setq auto-mode-alist (append '(("\\.pl" . prolog-mode)) auto-mode-alist))
(setq prolog-program-name "swipl")
(setq prolog-consult-string "[user].\n")

;;; ---------------------------------------------------------------------------
;;; lua-mode : Lua (checked)
;;; ---------------------------------------------------------------------------
;; luaのinstall pathを指定
(add-to-list 'load-path "/path/to/directory/where/lua-mode-el/resides")
(use-package lua-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))

;;; ---------------------------------------------------------------------------
;;; vimrc-mode : vimrc編集用 (checked)
;;; ---------------------------------------------------------------------------
(use-package vimrc-mode)

;;; ---------------------------------------------------------------------------
;;; jedi, epc, elpy, flycheck : Python (unchecked)
;;; ---------------------------------------------------------------------------
(use-package python-environment)

(use-package jedi
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))

(use-package epc)
(use-package elpy)

;; PEP8 check
(use-package py-autopep8
  :config
  (setq py-autopep8-options '("--max-line-length=200"))
  (setq flycheck-flake8-maximum-line-length 200)
  (py-autopep8-enable-on-save))

(use-package flycheck-pyflakes
  :config
  (require 'tramp-cmds)
  (add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
  (setq flymake-python-pyflakes-executable "/usr/bin/pyflakes")
  (flymake-python-pyflakes-load)
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

(add-hook
 'python-mode-hook
 '(lambda ()
    (setq indent-tabs-mode nil)
    (setq indent-level 4)
    (setq python-indent 4)
    (setq tab-width 4)
    (setq imenu-create-index-function 'python-imenu-create-index)
    (flymake-mode t)))

;;; ---------------------------------------------------------------------------
;;; provide
;;; ---------------------------------------------------------------------------
(provide 'language-conf)
