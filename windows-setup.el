;;;  -*- lexical-binding: t; -*-
;;; windows-setup.el -- Window management
;;;

(defun split-window-below-and-focus ()
  (interactive)
  (split-window-below)
  (other-window 1))

(defun split-window-right-and-focus ()
  (interactive)
  (split-window-right)
  (other-window 1))

(use-package emacs
  :config
  ;; Easy window switch
  (keybinds "C-<tab>" other-window
            "C-x 2" split-window-below-and-focus
            "C-x 3" split-window-right-and-focus
            "C-S-i" enlarge-window
            "C-S-d" shrink-window))

(use-package ace-window
  :config
  (keybind "C-z )" ace-swap-window))
