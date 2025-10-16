;;;  -*- lexical-binding: t; -*-
;;; programming-setup.el -- Customizations for coding
;;;

(use-package emacs
  :config
  (keybind "C-z /" compile))

(use-package cape
  :bind ("M-/" . cape-dabbrev))

(add-hook 'prog-mode-hook
	  (=> (setq-local completion-at-point-functions
			  '(cape-dabbrev cape-keyword))
	    (display-line-numbers-mode 1)))

(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))
