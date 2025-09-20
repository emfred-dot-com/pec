;;;  -*- lexical-binding: t; -*-
;;; lilypond-setup.el -- Configure LilyPond mode installed with homebrew
;;;

(if (executable-find "lilypond")
    (progn
      (when-mac
       (load-file
	"/opt/homebrew/share/emacs/site-lisp/lilypond/lilypond-init.el"))
      (when-linux
       (load-file
	"/usr/share/emacs/site-lisp/lilypond-init.el"))

      (when-mac
       (add-to-list 'load-path
		    "/opt/homebrew/share/emacs/site-lisp/lilypond"))

      (add-hook 'LilyPond-mode-hook
		(=> (flycheck-mode 1)
		    (display-line-numbers-mode 1)))))
