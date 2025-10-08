;;;  -*- lexical-binding: t; -*-
;;; terminal-setup.el -- Setup for (multiple) terminals
;;;

;; Use the excellent `vterm' terminal emulator. Fetch it from Github
;; since it's not on `melpa-stable'.
(use-package vterm
  :demand t
  :vc (:fetcher github :repo akermu/emacs-libvterm)
  :config
  (setq vterm-timer-delay nil
	vterm-max-scrollback 10000)
  (define-key vterm-mode-map (kbd "C-z") nil)
  (define-key vterm-mode-map (kbd "C-z C-z") #'vterm--self-insert))

;; Some helper functions that will be used in the definitions of the
;; key-bound vterm spawning function:

(defun collapse-tilde (path)
  "Un-expand the `~' character in a path, i.e. `string-replace' the full
home directory path with a `~/'."
  (string-replace (expand-file-name "~/") "~/" path))

(defun cwd ()
  "Get the current working directory. Implemented as `pwd' with the
returned string cleaned up."
  (let ((inhibit-message t))
    (mapconcat #'identity
	       (cdr (split-string
		     (collapse-tilde (pwd))
		     " "))
	       " ")))

(defun buffer-names-matching-regexp (regexp)
  "Return a list of buffers whose names match `regexp'."
  (require 'cl-lib)
  (cl-remove-if-not
   (lambda (buf-name)
     (string-match-p regexp buf-name))
   (mapcar #'buffer-name (buffer-list))))

(defun vterm-session/go (dir &optional force-new)
  "Go to a `vterm' session for the directory DIR. If a session already
exists for DIR, jump to it; else, create a new vterm instance in DIR.

If FORCE-NEW is `t', then a new session will be created unconditionally
(even if one already exists for DIR)."
  (let ((vterm-buf-name (concat "*vterm* " dir)))
    (if (and (get-buffer vterm-buf-name) (not force-new))
	(pop-to-buffer vterm-buf-name)
      (let ((default-directory dir))
	(vterm-other-window vterm-buf-name)))))

;; Now the main event: wrappers around `vterm' which enable:
;; - creating new sessions with descriptive names
;;   (`vterm-session/create-*')
;; - switching to existsing sessions by way of a completion menu
;;   (`vterm-session/select')

(defun vterm-session/create-in-current-directory (arg)
  "Create or jump to a new `vterm' session in the current directory
(that from which this command was executed). If one already exists there,
then jump to it, else create a new session and name the buffer after
the current directory.

If a prefix argument is passed, then a new session will be created
unconditionally (even if one already exists for the current directory)."
  (interactive "P")
  (let ((current-directory (cwd)))
    (vterm-session/go current-directory arg)))

(defun vterm-session/create-in-chosen-directory (arg)
  "Prompt for a directory in which to create a new, or jump to an existing,
`vterm' session.

If a prefix argument is passed, then a new session will be created
unconditionally."
  (interactive "P")
  (let ((dir (read-directory-name "Open vterm in: ")))
    (if dir
        (vterm-session/go dir arg))))

(defun vterm-session/select (arg)
  "Provide a completion menu of all active `vterm' sessions (whose names
match the format used by `vterm-create'), and jump to the selected
candidate.

If a prefix argument is passed, then the jump will happen in the current
window, instead of popping to another window"
  (interactive "P")
  (let ((vterm-buffer-names (buffer-names-matching-regexp "\*vterm\*")))
    (if (not vterm-buffer-names)
	(message "No vterm sessions to choose from.")
      (let ((buf-name (completing-read
		       "Select vterm session: " vterm-buffer-names)))
	(if arg
	    (switch-to-buffer buf-name)
	  (pop-to-buffer buf-name
			 #'display-buffer-use-least-recent-window))))))

(keybinds
 "C-z ,"   vterm-session/create-in-current-directory
 "C-z M-," vterm-session/create-in-chosen-directory
 "C-z C-," vterm-session/select)

(defcustom vterm-session/normal-line-length 80
  "Number of columns considered to be a \"normal\" line length by the
command `vterm-session/toggle-long-lines'.")

(defcustom vterm-session/long-line-length 5000
  "Number of columns considered to be a \"long\" line length by the
command `vterm-session/toggle-long-lines'.")

(defun vterm-session/toggle-long-lines ()
  "Set the variable `vterm-min-window-width' to a large value if it's
small, or a small value if it's large. The values used for \"small\" and
\"large\" are determined by the variables `vterm-session/normal-line-length'
and `vterm-session/long-line-length'.

The rationale behind this command, and the aforementioned variables, is as
follows:

A known issue (URL `https://github.com/akermu/emacs-libvterm/issues/179') in
`vterm' makes it such that text which gets wrapped when fallling outside of
a frame boundary (due to resizing from the window manager) gets clipped out
of existence when the window is resized back to a size which should
re-include that text.

It's possible to work around this problem by setting
`vterm-min-window-width' to a number of columns that's large enough such
that the lines never wrap. However, this messes with the output of commands
like `ls', as with a large `vterm-min-window-width' the output will lie all
on a single line of text (since it isn't wrapping).

With all of this in mind, a good workaround workflow can be achieved by
toggling this long line length on when we need it (i.e. are about to have
long-lined text on screen which we may need to resize), and then back off
when we no longer need it (so that the output of common commands can wrap
normally)."
  (interactive)
  (message (format "%s" vterm-min-window-width))
  (if (< vterm-min-window-width vterm-session/long-line-length)
      (progn
	(setq vterm-min-window-width vterm-session/long-line-length)
	(message (format "Vterm: line length set to LONG (%s columns)."
			 vterm-session/long-line-length)))
    (progn
      (setq vterm-min-window-width vterm-session/normal-line-length)
      (message (format
		"Vterm: line length set to NORMAL (%s columns)."
		vterm-session/normal-line-length)))))

(keybind "C-z <" vterm-session/toggle-long-lines)
; a.k.a. "C-z S-,"
