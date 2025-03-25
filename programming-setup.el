;;;  -*- lexical-binding: t; -*-
;;; programming-setup.el -- Customizations for coding
;;;

(use-package cape
  :bind ("M-/" . cape-dabbrev))

(add-hook 'prog-mode-hook
	  (lambda ()
	    (setq-local completion-at-point-functions '(cape-dabbrev cape-keyword))
	    (setq captain-predicate
		  (lambda () (nth 8 (syntax-ppss (point)))))
	    (display-line-numbers-mode 1)))

(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))
