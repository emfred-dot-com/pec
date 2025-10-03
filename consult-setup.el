;;;  -*- lexical-binding: t; -*-
;;; consult-setup.el -- "Consulting completing-read"
;;;

(use-package consult
  :demand t
  :config
  (setq register-preview-delay 0.8
        register-preview-function #'consult-register-format)

  (defun consult-recent-file-other-window ()
    "Find a recent file in another window using `completing-read'."
    (interactive)
    (find-file-other-window
     (consult--read
      (or
       (mapcar #'consult--fast-abbreviate-file-name (bound-and-true-p recentf-list))
       (user-error "No recent files, `recentf-mode' is %s"
                   (if recentf-mode "enabled" "disabled")))
      :prompt "Find recent file: "
      :sort nil
      :require-match t
      :category 'file
      :state (consult--file-preview)
      :history 'file-name-history)))

  (keybinds
   "C-z SPC" consult-recent-file
   "C-z S-SPC" consult-recent-file-other-window
   "C-z b" consult-buffer
   "C-z g" consult-ripgrep
   "C-z s" consult-line))
