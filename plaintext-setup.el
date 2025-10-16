;;;  -*- lexical-binding: t; -*-
;;; plaintext-setup.el -- Customizations for writing plain text
;;;

(use-package filladapt
  :ensure t
  :config
  (setq-default filladapt-mode t))

(add-hook 'text-mode-hook
	  (=> (setq-local
	       completion-at-point-functions 'cape-dabbrev
	       corfu-auto t
	       corfu-auto-delay 0.2
	       fill-column 70)
	      (auto-fill-mode 1)
	      (flyspell-mode 1)))
