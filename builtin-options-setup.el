;;;  -*- lexical-binding: t; -*-
;;; builtin-options-setup.el -- Settings for builtin options
;;;

(setq
 backup-by-copying t
 backup-directory-alist '(("." . "~/.emacs.d/backups"))
 bookmark-save-flag 1
 custom-file (locate-user-emacs-file "custom-vars.el")
 delete-old-versions t
 display-time-day-and-date nil
 display-time-default-load-average nil
 eshell-hitory-size 1024
 gc-cons-threshold 20000000
 global-auto-revert-non-file-buffers t
 grep-use-headings t
 inhibit-startup-screen t
 initial-buffer-choice t
 initial-scratch-message ";; Scratch\n\n"
 kept-new-versions 6
 kept-old-versions 2
 recentf-max-saved-items 500
 set-mark-command-repeat-pop t
 tab-bar-show 1
 undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))
 use-dialog-box nil
 version-control t)

(when-mac
 (setq
  confirm-kill-emacs (lambda (prompt) (yes-or-no-p "Quit?"))))

(setq
 native-comp-async-report-warnings-errors 'silent
 native-compile-prune-cache t)
