;;;  -*- lexical-binding: t; -*-
;;; pdf-setup.el -- Setup for viewing PDF documents
;;;

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

(add-hook 'pdf-view-mode-hook
	  (=> (local-set-key (kbd "j")
			     'pdf-view-next-line-or-next-page)
	      (local-set-key (kbd "k")
			     'pdf-view-previous-line-or-previous-page)))
