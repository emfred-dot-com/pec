;;;  -*- lexical-binding: t; -*-
;;; poem-setup.el -- Custom file extension for poetry
;;;

(use-package olivetti
  :bind ("C-z O" . olivetti-mode)
  :custom (olivetti-minimum-body-width 110))

(add-to-list 'auto-mode-alist
             '("\\.poem\\'" .
	       (=> (text-mode)
		   (olivetti-mode 1)
		   (text-scale-adjust 1)
		   (setq-local
		    completion-at-point-functions nil))))
