;;;  -*- lexical-binding: t; -*-
;;; dired-setup.el -- configurations for Dired
;;;

(use-package dired
  :ensure nil
  :config
  ;; Overwrite useless `list-directory' binding to call `dired' instead
  (keybind "C-x C-d" dired)

  (defun dired-sysopen-file-at-point ()
    (interactive)
    (async-shell-command (concat "open " (dired-file-name-at-point)) "open (from dired)"))

  (add-hook 'dired-mode-hook
	    (=> (keymap-set dired-mode-map (kbd ";")
			    'dired-sysopen-file-at-point))))

(use-package dired-hide-dotfiles
  :hook (dired-after-readin . dired-hide-dotfiles--hide)
  :bind (:map dired-mode-map
	      ("," . dired-hide-dotfiles-mode)))

(setq dired-listing-switches "-alh")

(defun xah-dired-sort ()
  "Sort dired dir listing in different ways.
Prompt for a choice.
Works in linux, MacOS, Microsoft Windows.

URL `http://xahlee.info/emacs/emacs/dired_sort.html'
Created: 2018-12-23
Version: 2025-01-05"
  (interactive)
  (let ((xmenu '(("date" . "-Al -t")
                 ("size" . "-Al -S")
                 ("name" . "-Al ")
                 ("dir" . "-Al --group-directories-first")))
        xsortBy)
    (setq xsortBy (completing-read "Sort by (default date):"
				   xmenu nil t nil nil (caar xmenu)))
    (dired-sort-other (cdr (assoc xsortBy xmenu)))))
