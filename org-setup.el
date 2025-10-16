;;;  -*- lexical-binding: t; -*-
;;; org-setup.el -- Configurations for org-mode
;;;

(use-package org
  :config

  ;; Load the `org-mode' templates for the built-in `tempo' templating
  ;; package. This provides a menu of snippets of the form "<X" where
  ;; "X" is some letter, and all of these snippets expand to org block
  ;; directives. For example, typing "<s" followed by typing "TAB"
  ;; will expand to the following (where "." is where it leaves the
  ;; cursor):
  ;;
  ;; "#+begin_source .
  ;;
  ;; #+end_source"
  ;;
  ;; Other out-of-the-box snippets are "<q" for a quote block, "<v"
  ;; for a verse block, etc.
  (require 'org-tempo)

  (add-hook 'org-mode-hook
	    (=> (flyspell-mode 0)
		(org-indent-mode 1)
		(abbrev-mode 1)

		(setq-local
		 org-image-max-width 50
		 completion-at-point-functions
		 '(pcomplete-completions-at-point cape-dabbrev)
		 electric-pair-inhibit-predicate
		 (lambda (char) (eq char ?<)))

		(keybind-local "C-c h" consult-org-heading))))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))
