;;;  -*- lexical-binding: t; -*-
;;; org-setup.el -- Configurations for org-mode
;;;

(use-package org
  :bind
  ("C-z a" . org-agenda)

  :custom
  (org-goto-auto-isearch nil)

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

		(keybind-local "C-c h" consult-org-heading)))

  ;; The following commands and bindings are intended to help with
  ;; managing todo lists. They target a workflow wherein subtrees of
  ;; todo items are used like a stack: new todo items are added as the
  ;; top-most item in the subtree (i.e. they're "pushed" onto the
  ;; stack), and completed todo items are moved to the bottom of the
  ;; subtree (i.e. "popped") upon completion.
  ;;
  ;; Three kinds of commands are defined below:
  ;; (1). Go to the {top, bottom} of the current subtree.
  ;; (2). Insert a new todo heading at the {top, bottom} of the current subtree.
  ;; (3). Move the current heading to the {top, bottom} of the current subtree.
  ;;
  ;; In relation to the "todo-stack" workflow described above,
  ;;   (2){top} = Push,
  ;;   (3){bottom} = Pop.
  ;;
  ;; Note that, for the sake of symmetry, (2){bottom} and (3){top} are
  ;; implemented alongside (2){top} and (3){bottom}, even though they're
  ;; not immediately useful within the "todo-stack" workflow. (These can
  ;; be thought of as an "anti-Push" and "anti-Pop".)

  (add-hook 'org-mode-hook
	    (=> (keybinds-local
		 ;; (1){top,bottom}:
		 "C-z 9" org-top-of-subtree
		 "C-z 0" org-bottom-of-subtree

		 ;; (2){top,bottom}:
		 "C-z (" org-insert-todo-heading-at-top-of-subtree
		 "C-z )" org-insert-todo-heading-at-bottom-of-subtree

		 ;; (3){top,bottom}:
		 "C-z C-9" org-move-heading-to-top-of-subtree
		 "C-z C-0" org-move-heading-to-bottom-of-subtree)))

  (defun org-top-of-subtree ()
    "Move to the top-most heading in the current subtree."
    (interactive)
    (org-back-to-heading t)
    (while (org-up-heading-safe))
    (org-goto-first-child))

  (defun org-bottom-of-subtree ()
    "Move to the bottom-most heading in the current subtree."
    (interactive)
    (let (prev)
      (while (not (eq (point) prev))
	(setq prev (point))
	(org-forward-heading-same-level 1))))

  (defun org-insert-todo-heading-at-top-of-subtree ()
    "Insert a new TODO heading at the top of the current subtree."
    (interactive)
    (org-top-of-subtree)
    (org-insert-todo-heading nil))

  (defun org-insert-todo-heading-at-bottom-of-subtree ()
    "Insert a new TODO heading at the bottom of the current subtree."
    (interactive)
    (org-bottom-of-subtree)
    (org-insert-todo-heading-respect-content))

  (defun org-move-heading-to-top-of-subtree ()
    "Move the current heading (and its inner content) to the top of its
containing subtree."
    (interactive)
    (org-back-to-heading t)
    (let ((n (save-excursion
               (let ((count 0))
		 (while (outline-get-last-sibling)
                   (setq count (1+ count)))
		 count))))
      (when (> n 0)
	(org-move-subtree-up n))))

  (defun org-move-heading-to-bottom-of-subtree ()
    "Move the current heading (and its inner content) to the bottom of its
containing subtree."
    (interactive)
    (org-back-to-heading t)
    (let ((n (save-excursion
               (let ((count 0))
		 (while (outline-get-next-sibling)
                   (setq count (1+ count)))
		 count))))
      (when (> n 0)
	(org-move-subtree-down n)))))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(keybind "C-z TAB"
	 (==> (find-file
	       personal/todo-list-for-work)))

