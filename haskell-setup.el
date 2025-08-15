;;;  -*- lexical-binding: t; -*-
;;; haskell-setup.el -- configurations for editing Haskell files
;;;

(use-package haskell-mode
  :defer t
  :config
  (setq haskell-process-type 'ghci
	haskell-process-show-debug-tips nil))
