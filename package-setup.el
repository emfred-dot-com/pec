;;;  -*- lexical-binding: t; -*-
;;; package-setup.el -- Package management initialization
;;;

(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" .
	       "https://stable.melpa.org/packages/"))

(keybind "C-z p" package-list-packages)

(setq package-install-upgrade-builtin t)

(package-initialize)

(setq use-package-always-ensure t)

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)
