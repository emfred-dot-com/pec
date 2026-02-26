;;;  -*- lexical-binding: t; -*-
;;; ai-setup.el -- Setup for AI LLM integration
;;;

(use-package claudemacs
  :vc (:url "https://github.com/cpoile/claudemacs" :rev :newest)
  :config
  (keybind "C-c c" claudemacs-transient-menu))
