;;;  -*- lexical-binding: t; -*-
;;; python-setup.el -- Configurations for Python3
;;;

(when-mac
 (add-hook 'python-mode-hook
	   (lambda ()
	     (setq python-shell-exec-path '("/usr/local/bin")))))
