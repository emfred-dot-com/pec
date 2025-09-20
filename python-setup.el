;;;  -*- lexical-binding: t; -*-
;;; python-setup.el -- Configurations for Python3
;;;

(when-mac
 (add-hook 'python-mode-hook
	   (=> (setq python-shell-exec-path '("/usr/local/bin")))))
