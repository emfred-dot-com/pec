;;;  -*- lexical-binding: t; -*-
;;; init.el -- Emacs startup file
;;;

;; Platform
(defcustom myOs "linux"
  "Variable used to guard settings that only work on Linux or MacOs"
  :type 'string)

;; Uncomment on mac
(setq myOs "mac")

(defmacro when-mac (then)
  `(when (string-equal myOs "mac")
     ,then))

(defmacro when-linux (then)
  `(when (string-equal myOs "linux")
     ,then))

(defun load-these (files)
  (mapcar
   (lambda (file)
     (load-file (locate-user-emacs-file file)))
   files))

;; Free up the leader so that files loaded below can define keybinds
;; off of it
(global-unset-key (kbd "C-z"))

(load-these '("package-setup.el"
	      ;; ^ (load this first so that the remaining *-setup
	      ;; files can declare their dependencies with
	      ;; `use-package')

	      ;; * Custom functions
	      "functions-setup.el"

	      ;; * Appearance
	      "appearance-setup.el"
	      "mode-line-setup.el"

	      ;; * Builtins
	      "builtin-modes-setup.el"
	      "builtin-options-setup.el"
	      "path-setup.el"
	      "tabs-setup.el"
	      "dired-setup.el"

	      ;; * Keybinds
	      "keys-setup.el"

	      ;; * Completion
	      "completion-setup.el"

	      ;; * Email
	      "mail-setup.el"

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
	      "html-setup.el"
	      "js-setup.el"
	      "lisp-setup.el"
	      "lsp-setup.el"
	      "python-setup.el"
	      "ocaml-setup.el"

	      ;; * Documents
	      "pdf-setup.el"))

;; M-x customize
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)
(put 'narrow-to-region 'disabled nil)
