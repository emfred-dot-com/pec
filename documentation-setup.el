;;;  -*- lexical-binding: t; -*-
;;; documentation-setup.el -- Ways to easily read docs
;;;

(use-package eldoc
  :config 
  (keybind "C-z '" eldoc))

(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package which-key
  :config
  (which-key-mode 1))
