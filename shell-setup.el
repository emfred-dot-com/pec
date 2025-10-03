;;;  -*- lexical-binding: t; -*-
;;; shell-setup.el -- Setup for shells
;;;

;; Eshell

(use-package eshell
  :config
  (add-hook 'eshell-mode-hook
	    (=> (setq-local corfu-auto-prefix 4)))

  (defun eshell-other-window ()
    "Open `eshell' in a new window; opens in the caller's working directory"
    (interactive)
    (let ((curdir (pwd)) (buf (eshell)))
      (switch-to-buffer (other-buffer buf))
      (switch-to-buffer-other-window buf)
      (eshell/cd (substring curdir (length "Directory ")))
      (eshell/clear-scrollback)
      (eshell-send-input)))

  (keybind "C-z ." eshell-other-window))

(use-package eat
  :hook (eshell-mode . eat-eshell-mode))

;; Shell commands -- interactive

(use-package shell-command+
  :config
  (keybind "M-!" shell-command+))

;; Shell commands -- keybound

(defmacro defun-login-shell-command
    (fun-name cmd-string &optional output-buffer-name)
  "Define an interactive function which runs cmd-string in a zsh shell with
my usual login environment"
  `(defun ,fun-name ()
     (interactive)
     (async-shell-command
      (concat "zsh -c \"source ~/.zshrc && " ,cmd-string "\"")
      (or ,output-buffer-name ,cmd-string))))

(defun-login-shell-command
 update-directory-index
 "~/scripts/s3/gd -u"
 "gd -u")

(defun-login-shell-command
 update-git-repos
 "~/scripts/pullall"
 "pullall")

(keybinds
 "C-z u d" update-directory-index
 "C-z u g" update-git-repos)

(defun-login-shell-command
 pw-build
 "cd ~/web/personal-website && hugo")

(defun-login-shell-command
 pw-serve
 "cd ~/web/personal-website && hugo serve"
 "hugo serve")

(defun-login-shell-command
 pw-serve-disable-fast-render
 "rm -rf ~/web/personal-website/public;
  cd ~/web/personal-website && hugo serve --disableFastRender"
 "hugo serve (disable fast render)")

(defun-login-shell-command
 pw-git-pull
 "cd ~/web/personal-website && git pull --ff-only")

(defun-login-shell-command
 pw-git-push
 "cd ~/web/personal-website && git push origin main")

(keybinds
 "C-z ; w b" pw-build
 "C-z ; w s" pw-serve
 "C-z ; w S" pw-serve-disable-fast-render
 "C-z ; w u" pw-git-pull
 "C-z ; w p" pw-git-push)
