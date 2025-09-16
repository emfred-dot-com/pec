;;;  -*- lexical-binding: t; -*-
;;; diff-setup.el -- Configurations for viewing diffs
;;;

(use-package ediff
  :config
  (setq ediff-window-setup-function
	#'ediff-setup-windows-plain))
