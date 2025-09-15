;;;  -*- lexical-binding: t; -*-
;;; haskell-setup.el -- configurations for editing Haskell files
;;;

(use-package haskell-mode
  :defer t
  :config
  (setq haskell-process-type 'cabal-repl
	haskell-process-show-debug-tips nil))

(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

(let ((my-ghcup-path (expand-file-name "~/.ghcup/bin")))
  (setenv "PATH" (concat my-ghcup-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path my-ghcup-path))

