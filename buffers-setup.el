;;;  -*- lexical-binding: t; -*-
;;; buffers-setup.el -- Buffer management
;;;

(use-package ibuffer
  :config
  (keybind "C-x C-b" ibuffer-other-window))
