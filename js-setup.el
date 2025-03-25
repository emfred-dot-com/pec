;;;  -*- lexical-binding: t; -*-
;;; js-setup.el -- Configurations for Javascript editing
;;;

(add-hook 'js-mode-hook
	  (lambda ()
	    (setq js-indent-level 2)))

(add-hook 'typescript-ts-mode-hook
	  (lambda ()
	    (setq typescript-ts-mode-indent-offset 4)))
