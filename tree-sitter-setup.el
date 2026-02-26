;;;  -*- lexical-binding: t; -*-
;;; tree-sitter-setup.el -- Better syntax highlighting
;;;

(use-package tree-sitter
  :config
  (global-tree-sitter-mode 1))

(use-package tree-sitter-langs)

(add-hook
 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
