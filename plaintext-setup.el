;;;  -*- lexical-binding: t; -*-
;;; plaintext-setup.el -- Customizations for writing plain text
;;;

(setq sentence-end-double-space nil)

;; Capitalization
(use-package captain
  :bind ("C-c u" . captain-capitalize-word))

(add-hook 'text-mode-hook (lambda ()
			    (setq captain-predicate (lambda () t))
			    (setq-local
			     completion-at-point-functions 'cape-dabbrev
			     corfu-auto t
			     corfu-auto-delay 0.2)
			    (set-fill-column 80)
			    (auto-fill-mode 1)
			    (flyspell-mode 1)))
