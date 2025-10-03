;;;  -*- lexical-binding: t; -*-
;;; basic-setup.el -- Basic functionality
;;;

(use-package emacs
  :config
  (keybinds
   "C-x c" restart-emacs
   "C-z f" toggle-frame-fullscreen))

;; Better yes-or-no prompt (e.g. upon "C-c C-c")
(use-package ido-yes-or-no
  :config
  (ido-yes-or-no-mode 1))
