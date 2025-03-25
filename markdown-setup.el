;;;  -*- lexical-binding: t; -*-
;;; markdown-setup.el -- Customizations for writing Markdown
;;;

(setq markdown-fontify-code-blocks-natively t)

(use-package adaptive-wrap
  :ensure t
  :commands adaptive-wrap-prefix-mode)

(add-hook 'markdown-mode-hook
	  (lambda ()
	    (setq-local
	     completion-at-point-functions 'cape-dabbrev
	     fill-column 80)
	    (auto-fill-mode 1)
	    (display-line-numbers-mode 1)
	    (flyspell-mode 1)
	    (adaptive-wrap-prefix-mode 1)))
