;;;  -*- lexical-binding: t; -*-
;;; pdf-setup.el -- Setup for viewing PDF documents
;;;

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

(add-hook 'pdf-view-mode-hook
	  (=> (keybinds-local
	       "j" pdf-view-next-line-or-next-page
	       "k" pdf-view-previous-line-or-previous-page)))
