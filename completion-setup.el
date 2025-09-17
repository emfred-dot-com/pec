;;;  -*- lexical-binding: t; -*-
;;; completion-setup.el -- Configurations for (auto) completion
;;;

;; Orderless: brilliant completion style that I use everywhere
(use-package orderless
  :config
  (setq completion-styles
	'(orderless basic)
	orderless-matching-styles
	'(orderless-literal orderless-regexp)))

(setq completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      bookmark-completion-ignore-case t)

;; Corfu: frontend for auto completion with various backends
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 3)
  (corfu-cycle t)
  (corfu-echo-delay 0.25)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match t)
  (corfu-preview-current 'insert)
  (corfu-preselect 'valid)
  (corfu-on-exact-match 'insert)
  (corfu-scroll-margin 5)
  :init
  (corfu-echo-mode)
  (corfu-history-mode)
  (global-corfu-mode))

(setq tab-always-indent 'complete)

;; Pabbrev: a great simple completion solution.
;; Using Corfu at the moment but keeping
;; this here in case I switch back.
;; (use-package pabbrev
;;   :ensure t
;;   :config
;;   (setq pabbrev-idle-timer-verbose nil)
;;   (global-pabbrev-mode 1))
