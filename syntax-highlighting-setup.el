;;;  -*- lexical-binding: t; -*-
;;; syntax-highlighting-setup.el -- Better syntax highlighting
;;;

(use-package treesit
  :ensure nil
  :custom
  (treesit-enabled-modes t)
  (treesit-font-lock-level 4))
