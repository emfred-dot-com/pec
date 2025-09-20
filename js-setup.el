;;;  -*- lexical-binding: t; -*-
;;; js-setup.el -- Configurations for Javascript editing
;;;

(add-hook 'js-mode-hook
	  (=> (setq js-indent-level 2)))

(add-hook 'typescript-ts-mode-hook
	  (=> (setq typescript-ts-mode-indent-offset 4)))
