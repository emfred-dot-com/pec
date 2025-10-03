;;;  -*- lexical-binding: t; -*-
;;; navigation-setup.el -- Ways to get around
;;;

(use-package emacs
  :config
  (keybinds
   "M-[" backward-sexp
   "M-]" forward-sexp))

(use-package beginend
  :demand t
  :config
  (beginend-global-mode 1))

(use-package ffap
  :config
  (keybind "C-z [" find-file-at-point))

(use-package embark
  :bind (("C-." . embark-act)))

(use-package embark-consult
  :after (embark consult))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Scrolling

(defun scroll-up-one ()
  (interactive)
  (scroll-up-command 1))

(defun scroll-down-one ()
  (interactive)
  (scroll-down-command 1))

(keybinds
 "M-n" scroll-up-one
 "M-p" scroll-down-one)

;; gd: Goto directory.
;; (See `https://emfred.com/programs/gd' for context)

(when-mac
 (defun gd (&optional arg)
   "Goto Directory"
   (interactive "P")
   (let ((target-dir (completing-read
		      "Directory: "
		      (with-temp-buffer
			(insert-file-contents "~/.gd_idx")
			(split-string (buffer-string) "\n" t)))))
     (if arg
	 (dired-other-window target-dir)
       (dired (concat "~/" target-dir))))))

(when-linux
 (defun gd (&optional arg)
   "Goto Directory"
   (interactive "P")
   (let ((target-dir (completing-read
		      "Directory: "
		      (with-temp-buffer
			(insert-file-contents "/home/eric/.gd_idx")
			(split-string (buffer-string) "\n" t)))))
     (if arg
	 (dired-other-window target-dir)
       (dired target-dir)))))

(keybind "C-z d" gd)
