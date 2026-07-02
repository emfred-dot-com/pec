;;;  -*- lexical-binding: t; -*-
;;; registers-setup.el -- Conveniences for working with registers
;;;

(use-package emacs
  :bind
  (("C-z r" . consult-register-load)
   ("C-z R" . consult-register)
   ("C-z C-r" . consult-register-store)))
