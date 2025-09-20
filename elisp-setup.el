;;;  -*- lexical-binding: t; -*-
;;; elisp-setup.el -- Configurations for editing Emacs lisp
;;;

(add-hook 'emacs-lisp-mode-hook
	  (=> (setq-local
	       completion-at-point-functions
	       '(elisp-completion-at-point cape-dabbrev cape-keyword))))
