;;;  -*- lexical-binding: t; -*-
;;; c-family-setup.el -- Configurations for C-like languages
;;;

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (c-set-style "stroustrup")
	    (filladapt-mode 1)))
