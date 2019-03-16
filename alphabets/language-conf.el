;;; package --- language-conf.el (specific settings for languages)
;;; Commentary:
;;; flymake and ...
;;;  言語設定 / language-conf.el
;;;  Lisp
;;;  - Gauche/Scheme
;;;  - Racket/Scheme
;;;  - Guile/Scheme
;;;  - SBCL/CommonLisp
;;;  - Clojure
;;;  + Hylang
;;;  Functional Programming Languages
;;;  - GHC/Haskell
;;;  - OCaml
;;;  - SML
;;;  - Erlang
;;;  - Elixir
;;;  + Scala
;;;  Lightweight Languages
;;;  - Lua
;;;  + Python
;;;  AltJavascripts
;;;  + JavaScript/TypeScript
;;;  Other Languages
;;;  - Rust
;;;  - Processing
;;;  - Prolog
;;;  - Vimrc
;;;  - yaml
;;;  + Web
;;; Code:
(require 'package)
(require 'use-package)
(require 'util-elisp)

;;; ---------------------------------------------------------------------------
;;; flymake : 文法チェッカ
;;; ---------------------------------------------------------------------------
(use-package-with-report flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))))

(use-package-with-report flymake-cursor)

;;; --------------------------------------------------------------------------------
;;; config : Lisp dialects
;;; --------------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; Gauche/Scheme (checked)
;;; ---------------------------------------------------------------------------
;; (defvar scheme-program-name "/usr/bin/gosh")

;;; ---------------------------------------------------------------------------
;;; racket-mode : Racket/Scheme (checked)
;;; ---------------------------------------------------------------------------
;; (use-package-with-report racket-mode
;;   :mode (("\\.rkt$" . racket-mode))
;;   :config
;;   (defun my-racket-mode ()
;;     (define-key racket-mode-map (kbd "C-c r") 'racket-run))
;;   (add-hook 'racket-mode-hook 'my-racket-mode))

;;; ---------------------------------------------------------------------------
;;; geiser-mode : Guile/Scheme (checked)
;;; ---------------------------------------------------------------------------
;; (use-package-with-report geiser
;;   :config
;;   (defvar geiser-active-implementations '(guile)))

;;; ---------------------------------------------------------------------------
;;; Common Lisp (SBCL/SLIME)の設定 (checked)
;;; ---------------------------------------------------------------------------
;; (defvar inferior-lisp-program "/usr/bin/sbcl")

;; (use-package-with-report slime
;;   :config
;;   (setq slime-contribs '(slime-fancy))
;;   (slime-setup '(slime-repl slime-fancy slime-banner slime-indentation)))

;; (use-package-with-report ac-slime
;;   :config
;;   (add-hook 'slime-mode-hook 'set-up-slime-ac)
;;   (add-hook 'slime-repl-mode-hook 'set-up-slime-ac))

;;; ---------------------------------------------------------------------------
;;; clojure-mode : Clojure (unchecked)
;;; ---------------------------------------------------------------------------
;; (use-package-with-report clojure-mode)
;; '(use-package-with-report clj-refactor
;;   :config
;;   (defun my-clojure-mode-hook ()
;;     (clj-refactor-mode 1)
;;     (yas-minor-mode 1)
;;     (cljr-add-keybindings-with-prefix "C-c C-m"))
;;   (add-hook 'clojure-mode-hook 'my-clojure-mode-hook))

;;; ---------------------------------------------------------------------------
;;; hy-mode : Hylang (checked)
;;; ---------------------------------------------------------------------------
(use-package-with-report hy-mode)

;;; --------------------------------------------------------------------------------
;;; config : Functional programming languages
;;; --------------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; scala-mode : Scala (unchecked)
;;; ---------------------------------------------------------------------------
(use-package-with-report scala-mode)

(use-package-with-report ensime
  :config
  (setq ensime-completion-style 'auto-complete)
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))

;;; ---------------------------------------------------------------------------
;;; haskell-mode : Haskell (checked)
;;; ---------------------------------------------------------------------------
;; (use-package-with-report haskell-mode
;;   :mode (("\\.hs$" . haskell-mode)
;;          ("\\.lhs$" . literate-haskell-mode))
;;   :config
;;   (defvar haskell-program-name "ghci"))

;; (ignore-require-with-report
;;  "failed tot load inf-haskell, haskell-cabal"
;;  (use-package inf-haskell)
;;  (use-package haskell-cabal
;;    :mode (("\\.cabal$" . haskell-cabal-mode)))
;;  (setq haskell-font-lock-symbols t))

;;; ---------------------------------------------------------------------------
;;; taureg-mode ocaml : OCaml (unchecked)
;;; ---------------------------------------------------------------------------
;; (ignore-require-with-report
;;  "failed to load taureg-mode & others ... (OCaml)"
;;  (use-package taureg-mode
;;    :mode (("\\.ml[iylp]?" . tuareg-mode))))

;;; ---------------------------------------------------------------------------
;;; sml-mode : SML (checked)
;;; ---------------------------------------------------------------------------
;; 設定はSML/NJ
;; (use-package-with-report sml-mode
;;   :config
;;   (setq sml-program-name "sml"))

;;; ---------------------------------------------------------------------------
;;; erlang-mode : Erlang (unchecked)
;;; ---------------------------------------------------------------------------
;; erlangのinstall pathを指定 (設定場所が微妙)
;; (ignore-require-with-report
;;  "failed tot load taureg-start & others ... (Erlang)"
;;  (add-to-list 'load-path "/usr/lib/erlang/lib/tools-2.8.3/emacs")
;;  (setq erlang-root-dir "/usr/local/otp")
;;  (setq exec-path (cons "/usr/local/otp" exec-path))
;;  (use-package erlang-start)
;;  (use-package erlang-flymake))

;;; --------------------------------------------------------------------------------
;;; config : LL
;;; --------------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; lua-mode : Lua (checked)
;;; ---------------------------------------------------------------------------
;; luaのinstall pathを指定
;; (add-to-list 'load-path "/path/to/directory/where/lua-mode-el/resides")
;; (use-package-with-report lua-mode
;;   :mode
;;   (("\\.lua$" . lua-mode))
;;   :interpreter
;;   (("lua" . lua-mode)))

;;; ---------------------------------------------------------------------------
;;; jedi, epc, elpy, flycheck : Python (unchecked)
;;; ---------------------------------------------------------------------------
(use-package-with-report python-environment)

(use-package-with-report jedi
  :bind (("\C-cd" . jedi:goto-definition)
         ("\C-cp" . jedi:goto-definition-pop-marker)
         ("\C-cr" . helm-jedi-related-names))
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
  (setq flymake-gui-warnings-enabled t)
  (custom-set-variables
   '(flymake-python-pyflakes-extra-arguments '("--max-line-length=120" "--ignore=E128")))
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
    (require 'python)
    (setq indent-tabs-mode nil)
    (setq indent-level 4)
    (setq python-indent 4)
    (setq tab-width 4)
    (flycheck-mode t)
    (setq imenu-create-index-function 'python-imenu-create-index)
    ;; 元々のauto-complete補完候補を消す
    (setq ac-sources (delete 'ac-source-words-in-same-mode-buffers ac-sources))
    (add-to-list 'ac-sources 'ac-source-filename)
    (add-to-list 'ac-sources 'ac-source-jedi-direct)
    (flymake-mode t)))

(add-hook 'python-mode-hook 'my-python-mode)

;;; ---------------------------------------------------------------------------
;;; js2(js/ts)-mode : JavaScript/TypeScript (unchecked)
;;; ---------------------------------------------------------------------------
(use-package-with-report typescript-mode
  :mode
  (("\\.ts$" . typescript-mode))
  :config
  (defun my-ts-mode ()
    (progn
      (setq indent-level 2)
      (setq tab-width 2)
      (flycheck-mode t)
      ))
  (add-hook 'ts-mode-hook 'my-ts-mode))

(use-package-with-report js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (defun my-js-mode ()
    (progn
      (setq indent-level 2)
      (setq tab-width 2)
      (flycheck-mode t)
      ))
  (add-hook 'js2-mode-hook 'my-js-mode))

;;; --------------------------------------------------------------------------------
;;; config : Other Languages
;;; --------------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; rust-mode : rust用 (rust)
;;; ---------------------------------------------------------------------------
;; (use-package-with-report rust-mode)

;;; ---------------------------------------------------------------------------
;;; processing-mode : Processing
;;; ---------------------------------------------------------------------------
;; (use-package-with-report processing-mode)

;;; ---------------------------------------------------------------------------
;;; prolog-mode : Prolog (checked)
;;; ---------------------------------------------------------------------------
;; (add-to-list 'auto-mode-alist '("\\.pl$" . prolog-mode))
;; (setq prolog-program-name "swipl")
;; (setq prolog-consult-string "[user].\n")

;;; ---------------------------------------------------------------------------
;;; vimrc-mode : vimrc編集用 (checked)
;;; ---------------------------------------------------------------------------
;; (use-package-with-report vimrc-mode)

;;; ---------------------------------------------------------------------------
;;; yaml-mode : yaml (checked)
;;; ---------------------------------------------------------------------------
(use-package-with-report yaml-mode)

;;; ---------------------------------------------------------------------------
;;; dockerfile-mode : for docker (checked)
;;; ---------------------------------------------------------------------------
(use-package-with-report dockerfile-mode)

;;; ---------------------------------------------------------------------------
;;; web-mode : web編集用
;;; ---------------------------------------------------------------------------
(use-package-with-report web-mode
  :mode
  (("\\.js$" . web-mode))
  (("\\.html$" . web-mode))
  (("\\.css$" . css-mode))
  :config
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-indent-style 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package-with-report emmet-mode
  :mode
  (("\\.html$" . emmet-mode)))

;;; ---------------------------------------------------------------------------------
;;; provide
;;; ---------------------------------------------------------------------------------
(provide 'language-conf)
;;; language-conf.el ends here
