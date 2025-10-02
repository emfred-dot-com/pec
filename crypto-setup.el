;;;  -*- lexical-binding: t; -*-
;;; crypto-setup.el -- Setup for encrypting/decrypting text
;;;

(use-package epa
  :config
  (setq epa-pinentry-mode 'loopback))
