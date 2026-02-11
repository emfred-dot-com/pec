;;;  -*- lexical-binding: t; -*-
;;; lsp-setup.el -- Setup for Language Server Protocol integration
;;;

(use-package eglot
  :after orderless
  :init
  (setq eglot-autoshutdown t
        eglot-code-action-indications '(margin)
        completion-category-overrides '((eglot (styles orderless))
                                        (eglot-capf (styles orderless)))))

(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (add-to-list 'eglot-ignored-server-capabilities
                         :documentOnTypeFormattingProvider)))

