;;;  -*- lexical-binding: t; -*-
;;; package-setup.el -- Package management
;;;

(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" .
	       "https://stable.melpa.org/packages/"))

(setq package-install-upgrade-builtin t)

(package-initialize)

(setq use-package-always-ensure t)

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

(use-package accent
  :vc (:fetcher github :repo eliascotto/accent)
  :bind ("C-x M-a" . accent-menu))

(use-package ace-window
  :defer t)

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


(use-package beginend
  :demand t
  :config
  (beginend-global-mode 1))

(use-package bufferfile)

(use-package consult
  :demand t
  :config
  (setq register-preview-delay 0.8
        register-preview-function #'consult-register-format))

(use-package csv-mode
  :defer t)

(use-package dired-hide-dotfiles
  :hook (dired-after-readin . dired-hide-dotfiles--hide)
  :bind (:map dired-mode-map
	      ("," . dired-hide-dotfiles-mode)))

(use-package dynamic-spaces
  :defer t
  :commands dynamic-spaces-mode)

;; (use-package eat
;;   :hook (eshell-mode . eat-eshell-mode))

(use-package embark
  :bind (("C-." . embark-act)))

(use-package embark-consult
  :after (embark consult))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package filladapt
  :ensure t
  :config
  (setq-default filladapt-mode t))

(use-package geiser
  :config
  (setq geiser-active-implementations '(gambit)))

(use-package geiser-gambit)

(use-package geiser-racket)

(use-package git-gutter-fringe)

(use-package haskell-mode
  :defer t)

(use-package ido-yes-or-no
  :config
  (ido-yes-or-no-mode 1))

(use-package magit
  :defer t
  :config
  (keymap-set magit-section-mode-map "C-<tab>" #'other-window))

(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package markdown-mode
  :defer t)

(use-package multiple-cursors
  :bind (("C-<" . mc/mmlte--up)
	 ("C->" . mc/mmlte--down)
	 ("C-M->" . mc/mark-next-like-this)
	 ("C-M-<" . mc/mark-previous-like-this)
	 ("C-c C->" . mc/mark-all-dwim)))

(use-package pdf-tools
  :hook (doc-view-mode . pdf-tools-install))

(use-package proof-general
  :config
  (setq proof-splash-enable nil))

(use-package pulsar
  :hook ((next-error . pulsar-highlight-line)
	 (isearch-mode-end . pulsar-pulse-line)))

(use-package rainbow-mode
  :defer t
  :init
  (setq rainbow-ansi-colors nil
	rainbow-html-colors nil
	rainbow-latex-colors nil
	rainbow-r-colors nil
	rainbow-x-colors nil))

(use-package spacious-padding
  :init (setq spacious-padding-subtle-mode-line nil)
  :hook ((after-init . spacious-padding-mode)
	 (server-after-make-frame . spacious-padding-mode)))

(use-package smartparens)

(use-package substitute
  :bind (("M-# b" . substitute-target-in-buffer)
	 ("M-# d" . substitute-target-in-defun)
	 ("M-# s" . substitute-target-below-point)
	 ("M-# r" . substitute-target-above-point))
  :config
  (add-to-list 'substitute-post-replace-hook
	       'substitute-report-operation))

(use-package vertico
  :config
  (vertico-mode 1))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vterm
  :demand t
  :load-path "~/repos/emacs-libvterm/"
  :config
  (setq vterm-timer-delay nil)
  (setq vterm-max-scrollback 10000))

(use-package which-key
  :config
  (which-key-mode 1))

(use-package yasnippet
  :config
  (yas-global-mode 1))
