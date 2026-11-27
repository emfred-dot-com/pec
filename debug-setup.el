;;;  -*- lexical-binding: t; -*-
;;; debug-setup.el -- Setup for Debug Adapter Protocol integration
;;;

(use-package dape
  :custom
  (dape-breakpoint-global-mode 1)
  (dape-buffer-window-arrangement 'right)
  (dape-debug t)

  :config
  (add-hook 'dape-display-source-hook #'pulse-momentary-highlight-one-line))
