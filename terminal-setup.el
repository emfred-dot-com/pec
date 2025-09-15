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

(defmacro maybe-in-project (body)
  "If we're in a project, set the current working directory to the root
directory of that project."
  `(let ((proj (project-current nil)))
     (let ((default-directory (if proj
				  (project-root proj)
				default-directory)))
       ,body)))

(defun cwd ()
  "Get the current working directory. Implemented as `pwd' with the
returned string cleaned up, and with support for `project.el'."
  (let ((inhibit-message t))
    (maybe-in-project
     (mapconcat #'identity
		(cdr (split-string
		      (collapse-tilde (pwd))
		      " "))
		" "))))

(defun buffer-names-matching-regexp (regexp)
  "Return a list of buffers whose names match `regexp'."
  (require 'cl-lib)
  (cl-remove-if-not
   (lambda (buf-name)
     (string-match-p regexp buf-name))
   (mapcar #'buffer-name (buffer-list))))

;; Now the main event: wrappers around `vterm' which enable:
;; - creating new sessions with descriptive names (`vterm-create')
;; - switching to existsing sessions by way of a completion menu
;;   (`vterm-select')

(defun vterm-create (arg)
  "Create or jump to a new `vterm' session. If one already exists for the
current directory (that from which this command was executed), then
jump to it, else create a new session and name the buffer after the
current directory.

If a prefix argument is passed, then a new session will be created
unconditionally (even if one already exists for the current directory)."
  (interactive "P")
    (let* ((current-directory
	    (cwd))
	   (vterm-buf-name
	    (concat "*vterm* " current-directory)))
      (if (and (get-buffer vterm-buf-name) (not arg))
	  (pop-to-buffer vterm-buf-name)
	(vterm-other-window vterm-buf-name))))

(defun vterm-select (arg)
  "Provide a completion menu of all active `vterm' sessions (whose names
match the format used by `vterm-create'), and jump to the selected
candidate.

If a prefix argument is passed, then the jump will happen in the current
 window, instead of popping to another window"
  (interactive "P")
  (let* ((vterm-buffer-names (buffer-names-matching-regexp "\*vterm\*"))
	 (buf-name (completing-read "Select vterm instance: " vterm-buffer-names)))
    (if arg
	(switch-to-buffer buf-name)
      (pop-to-buffer buf-name))))

(keybinds
 "C-z ," vterm-create
 "C-z C-," vterm-select)
