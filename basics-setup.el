;;;  -*- lexical-binding: t; -*-
;;; basic-setup.el -- Basic functionality
;;;

(use-package emacs
  :config
  (keybinds
   "C-x c" restart-emacs
   "C-z f" toggle-frame-fullscreen))
