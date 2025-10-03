;;;  -*- lexical-binding: t; -*-
;;; vertico-setup.el -- "VERTical Interactive COmpletion"
;;;

(use-package vertico
  :config
  (vertico-mode 1))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))
