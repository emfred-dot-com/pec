;;;  -*- lexical-binding: t; -*-
;;; keys-setup.el -- Keybindings
;;;

(defmacro keybind (key func)
  "Bind @key, given as a string (e.g. 'C-x s') to the function @func (given
  unquoted)"
  `(global-set-key (kbd ,key) (quote ,func)))

(defmacro keybinds (&rest keys-functions)
  "Apply the `keybind' macro pairwise through the argument list"
  (let ((key (car keys-functions))
	(func (cadr keys-functions))
	(rest (cddr keys-functions)))
    (when func
	`(progn
	   (keybind ,key ,func)
	   (keybinds ,@rest)))))

;; Tweaks to default bindings

(keybinds
 ; Better buffer menu
 "C-x C-b" ibuffer-other-window

 ; Overwrite useless `list-directory' binding to call `dired' instead
 "C-x C-d" dired

 ; Better yank-from-kill-ring
 "M-y" consult-yank-from-kill-ring

 ; Easy restart
 "C-x c" restart-emacs

 ; Easy window switch
 "C-<tab>" other-window

 ; (Remap tab switch)
 "C-S-<tab>" tab-next)

;; Custom bindings

(keybinds
 "C-z ," vterm-other-window
 "C-z ." eshell-other-window
 "C-z 1" consult-todo
 "C-z 6" file-info-show
 "C-z =" edit-config
 "C-z C-a" incr-num-at-point
 "C-z C-x" decr-num-at-point
 "C-z SPC" consult-recent-file
 "C-z S-SPC" consult-recent-file-other-window
 "C-z )" ace-swap-window
 "C-z [" find-file-at-point
 "C-z b" consult-buffer
 "C-z c" copy-line
 "C-z d" gd
 "C-z f" toggle-frame-fullscreen
 "C-z g" consult-ripgrep
 "C-z h" hl-line-mode
 "C-z i" sp-change-inner
 "C-z k" kill-paragraph
 "C-z K" backward-kill-paragraph
 "C-z l" display-line-numbers-mode
 "C-z o" sp-change-enclosing
 "C-z p" package-list-packages
 "C-z q" logos-focus-mode
 "C-z s" consult-line
 "C-z t" ef-themes-select
 "C-z C-t" ef-themes-toggle
 "C-z T" modus-themes-select
 "C-z u d" update-directory-index
 "C-z u g" update-git-repos
 "C-z w" whitespace-mode
 "C-z {" beginning-of-defun
 "C-z }" end-of-defun
 "C-z #" goto-line-num-at-point-in-recent-file
 "M-n" scroll-up-one
 "M-p" scroll-down-one
 "M-Q" xah-unfill-paragraph
 "C-z ; w b" pw-build
 "C-z ; w s" pw-serve
 "C-z ; w S" pw-serve-disable-fast-render
 "C-z ; w u" pw-git-pull
 "C-z ; w p" pw-git-push
 "C-z /" compile)
