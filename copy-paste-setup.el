;;;  -*- lexical-binding: t; -*-
;;; copy-paste-setup.el -- Copying ("Killing") and Pasting ("Yanking")
;;;

(use-package consult
  :config
  (keybind "M-y" consult-yank-from-kill-ring))
