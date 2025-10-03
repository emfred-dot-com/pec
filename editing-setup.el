;;;  -*- lexical-binding: t; -*-
;;; editing-setup.el -- Ways to swiftly edit text
;;;

(use-package dynamic-spaces
  :defer t
  :commands dynamic-spaces-mode)

(use-package smartparens
  :config
  (keybind "C-z i" sp-change-inner))

(use-package multiple-cursors
  :bind (("C-<" . mc/mmlte--up)
	 ("C->" . mc/mmlte--down)
	 ("C-M->" . mc/mark-next-like-this)
	 ("C-M-<" . mc/mark-previous-like-this)
	 ("C-c C->" . mc/mark-all-dwim)))

(use-package substitute
  :bind (("M-# b" . substitute-target-in-buffer)
	 ("M-# d" . substitute-target-in-defun)
	 ("M-# s" . substitute-target-below-point)
	 ("M-# r" . substitute-target-above-point))
  :config
  (add-to-list 'substitute-post-replace-hook
	       'substitute-report-operation))
