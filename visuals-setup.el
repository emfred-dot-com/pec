;;;  -*- lexical-binding: t; -*-
;;; visuals-setup.el -- Visual bells and whistles
;;;

(use-package emacs
  :config
  (keybinds
   "C-z h" hl-line-mode
   "C-z l" display-line-numbers-mode
   "C-z w" whitespace-mode))
