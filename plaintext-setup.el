;;;  -*- lexical-binding: t; -*-
;;; plaintext-setup.el -- Customizations for writing plain text
;;;

(setq sentence-end-double-space nil)

;; Capitalization
(use-package captain
  :bind ("C-c u" . captain-capitalize-word))

(use-package filladapt
  :ensure t
  :config
  (setq-default filladapt-mode t))

(add-hook 'text-mode-hook
	  (=> (setq-local
	       captain-predicate (=> t)
	       completion-at-point-functions 'cape-dabbrev
	       corfu-auto t
	       corfu-auto-delay 0.2
	       fill-column 70)
	      (auto-fill-mode 1)
	      (flyspell-mode 1)))
