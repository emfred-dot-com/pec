;;;  -*- lexical-binding: t; -*-
;;; appearance-setup.el -- Configurations for how Emacs looks
;;;

;; Use the wonderful ef-themes
(use-package ef-themes
  :config
  (setq ef-melissa-dark-palette-overrides
	'((bg-search-lazy bg-cyan-intense)
	  (fg-prompt yellow-warmer))
        ef-dream-palette-overrides
	'((prose-done green-warmer)))

  (keybinds
   "C-z t" ef-themes-select
   "C-z C-t" ef-themes-toggle)

  (setq ef-themes-to-toggle
	personal/ef-themes-to-toggle))

(require-theme 'ef-themes)

(use-package doric-themes)

(require-theme 'doric-themes)

(load-theme (cadr ef-themes-to-toggle) t)

;; Ensure fullscreen at startup
(when-mac
 (unless (frame-parameter nil 'fullscreen)
   (toggle-frame-fullscreen)))

;; Bigger font
(set-face-attribute 'default nil :height 180)

;; Easy command to change font size
(defun set-font-height-in-pts (pts)
  (set-face-attribute 'default nil :height (* 10 pts)))

(defun change-font-size ()
  (interactive)
  (let* ((common-sizes
	  (mapcar #'number-to-string '(8 10 12 14 16 18 20)))
	 (size-in-pts
	  (completing-read "Enter the new font size in pts: " common-sizes)))
    (set-font-height-in-pts (string-to-number size-in-pts))))

;; Use the wonderful "Aporetic" font
(when-mac (set-face-attribute 'default nil :family "Aporetic Sans Mono"))
(when-linux (set-face-attribute 'default nil :family "Source Code Pro"))

;; Spacious padding
(use-package spacious-padding
  :init (setq spacious-padding-subtle-mode-line nil)
  :hook ((after-init . spacious-padding-mode)
	 (server-after-make-frame . spacious-padding-mode)))

;; Truncate long lines (wrapping causes too much visual interference)
(set-default 'truncate-lines t)

;; Use `minions' to collapse all minor mode lighters into a single
;; meta-lighter (which yields a menu when clicked, in which the hidden
;; minor modes are visible). Then, go even further by setting the
;; character of that meta-lighter to the empty string, so that it's
;; not visible at all on the mode line.
;;
;; The rationale behind this is that I don't want minor mode lighters
;; taking up any space on the mode line. If I want to know which minor
;; modes are active, I can check the relevant variables via "C-h v".
(use-package minions
  :init (setq minions-mode-line-lighter "")
  :custom
  (minions-prominent-modes
   '(god-local-mode))
  :config (minions-mode 1))

;; Show the current time in the modeline
(display-time)

;; Smooth scrolling
(pixel-scroll-precision-mode 1)

;; Remove annoying inverse-video effect in certain minibuffer text
(when (facep 'read-multiple-choice-face)
  (set-face-attribute 'read-multiple-choice-face nil
                      :inverse-video nil))
