;;;  -*- lexical-binding: t; -*-
;;; init.el -- Emacs startup file
;;;

(defun load-these (files)
  (mapcar
   (lambda (file)
     (load-file (locate-user-emacs-file file)))
   files))

(load-these '("setup-setup.el"

	      ;; * Packages
	      "package-setup.el"
	      ;; ^ (load this first so that the remaining *-setup
	      ;; files can declare their dependencies with
	      ;; `use-package')

	      ;; * Custom functions
	      "functions-setup.el"

	      ;; * Keybinds
	      "keys-setup.el"

	      ;; * Appearance
	      "appearance-setup.el"
	      "mode-line-setup.el"

	      ;; * Builtins
	      "builtin-modes-setup.el"
	      "builtin-options-setup.el"
	      "path-setup.el"
	      "tabs-setup.el"
	      "dired-setup.el"

	      ;; * Completion
	      "completion-setup.el"

	      ;; * Email
	      "mail-setup.el"

	      ;; * Terminal
	      "terminal-setup.el"

	      ;; * Shell
	      "shell-setup.el"

	      ;; * Tramp
	      "tramp-setup.el"

	      ;; * Languages
	      ;; * * natural
	      "plaintext-setup.el"
	      "notes-setup.el"
	      "poem-setup.el"

	      ;; * * markup
	      "markdown-setup.el"
	      "org-setup.el"
	      "lilypond-setup.el"

	      ;; * * programming
	      "programming-setup.el"
	      "c-family-setup.el"
	      "d-setup.el"
	      "elisp-setup.el"
	      "haskell-setup.el"
	      "html-setup.el"
	      "js-setup.el"
	      "lisp-setup.el"
	      "lsp-setup.el"
	      "ocaml-setup.el"
	      "python-setup.el"

	      ;; * Documents
	      "pdf-setup.el"

	      ;; * Diffs
	      "diff-setup.el"

	      ;; * Version control
	      "magit-setup.el"))

;; M-x customize
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)
(put 'narrow-to-region 'disabled nil)
