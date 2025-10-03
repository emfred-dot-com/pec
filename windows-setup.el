;;;  -*- lexical-binding: t; -*-
;;; buffers-setup.el -- Window management
;;;

(use-package emacs
  :config
  ;; Easy window switch
  (keybind "C-<tab>" other-window))

(use-package ace-window
  :config
  (keybind "C-z )" ace-swap-window))
