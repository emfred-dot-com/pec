;;;  -*- lexical-binding: t; -*-
;;; windows-setup.el -- Window management
;;;

(use-package emacs
  :config
  ;; Easy window switch
  (keybinds "C-<tab>" other-window
            "C-S-i" enlarge-window
            "C-S-d" shrink-window))

(use-package ace-window
  :config
  (keybind "C-z )" ace-swap-window))
