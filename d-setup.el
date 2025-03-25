;;;  -*- lexical-binding: t; -*-
;;; d-setup.el -- Customizations for programming in D
;;;

(use-package eglot
  :config
  (add-hook 'd-mode-hook 'eglot-ensure)
  (add-to-list 'eglot-server-programs
	       `(d-mode . (executable-find "serve-d"))))

