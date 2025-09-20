;;;  -*- lexical-binding: t; -*-
;;; ocaml-setup.el -- ocaml config with tuareg and merlin
;;;

(use-package tuareg
  :defer t)

(add-to-list 'load-path
	     "/Users/ericfrederickson/.opam/default/share/emacs/site-lisp")

(use-package ocp-indent
  :ensure t)

(require 'ocp-indent)

(let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)
    ;; To easily change opam switches within a given Emacs session, you can
    ;; install the minor mode https://github.com/ProofGeneral/opam-switch-mode
    ;; and use one of its "OPSW" menus.
    ))
