;;;  -*- lexical-binding: t; -*-
;;; visuals-setup.el -- Visual bells and whistles
;;;

(use-package emacs
  :config
  (keybinds
   "C-z h" hl-line-mode
   "C-z l" display-line-numbers-mode
   "C-z w" whitespace-mode))

(use-package pulsar
  :hook ((next-error . pulsar-highlight-line)
	 (isearch-mode-end . pulsar-pulse-line)))

(use-package rainbow-mode
  :defer t
  :init
  (setq rainbow-ansi-colors nil
	rainbow-html-colors nil
	rainbow-latex-colors nil
	rainbow-r-colors nil
	rainbow-x-colors nil))
