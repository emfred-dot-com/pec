;;;  -*- lexical-binding: t; -*-
;;; completion-setup.el -- Configurations for (auto) completion
;;;

;; Hotfuzz (new): better version of the builtin `flex' completion
;; style
(use-package hotfuzz)

;; Orderless: brilliant completion style that I use everywhere
(use-package orderless
  :config (setq completion-styles '(orderless hotfuzz basic)))

(setq
 completion-ignore-case t
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t
 bookmark-completion-ignore-case t
 completion-category-defaults nil
 completion-category-overrides
 '((file (styles . (partial-completion hotfuzz)))))

;; Corfu: frontend for auto completion with various backends
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  (corfu-cycle t)              ;; Enable cycling for `corfu-next/previous'
  (corfu-echo-delay 0.25)
  (corfu-separator ?\s)        ;; Orderless field separator
  (corfu-quit-at-boundary nil) ;; Never quit at completion boundary
  (corfu-quit-no-match t)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)  ;; Disable current candidate preview
  (corfu-preselect 'first)     ;; Preselect the prompt
  (corfu-on-exact-match nil)   ;; Configure handling of exact matches
  (corfu-scroll-margin 5)      ;; Use scroll margin
  :init
  (corfu-echo-mode)
  (corfu-history-mode)
  (global-corfu-mode))

(setq
 completion-cycle-threshold 1
 tab-always-indent 'complete)

;; Pabbrev: a great simple completion solution.
;; Using Corfu at the moment but keeping
;; this here in case I switch back.
;; (use-package pabbrev
;;   :ensure t
;;   :config
;;   (setq pabbrev-idle-timer-verbose nil)
;;   (global-pabbrev-mode 1))
