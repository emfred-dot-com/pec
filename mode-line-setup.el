;;;  -*- lexical-binding: t; -*-
;;; mode-line-setup.el -- Mode-line configurations
;;;

(use-package diminish
  :demand t
  :config
  (diminish 'auto-revert-mode)
  (mapcar (lambda (mode) (diminish mode))
	  (mapcar #'cdr beginend-modes))
  (diminish 'eldoc-mode)
  (diminish 'which-key-mode)
  (diminish 'yas-minor-mode))

