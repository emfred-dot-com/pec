;;;  -*- lexical-binding: t; -*-
;;; activities-setup.el -- Save and restore sessions (a bit like `tmux')
;;;

(use-package activities
  :init
  (activities-mode)
  (activities-tabs-mode)
  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)
  :bind
  (("C-z a n" . activities-new)
   ("C-z a d" . activities-define)
   ("C-z a a" . activities-resume)
   ("C-z a s" . activities-suspend)
   ("C-z a k" . activities-kill)
   ("C-z a SPC" . activities-switch)
   ("C-z a b" . activities-switch-buffer)
   ("C-z a g" . activities-revert)
   ("C-z a l" . activities-list)))
