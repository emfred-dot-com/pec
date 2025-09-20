;;;  -*- lexical-binding: t; -*-
;;; org-setup.el -- Configurations for org-mode
;;;

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(add-hook 'org-mode-hook
	  (=> (flyspell-mode 0)
	      (org-indent-mode 1)
	      (abbrev-mode 1)
	      (setq captain-predicate
		    (=> (not (org-in-src-block-p)))
		    org-image-max-width 50)
	      (keybind-local "C-c h" consult-org-heading)))
