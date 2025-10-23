;;;  -*- lexical-binding: t; -*-
;;; zettelkasten-setup.el
;;; -- Setup for using the Zettelkasten note-taking system
;;;

;; denote config from Prot!
(use-package denote
  :demand t
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-z n n" . denote)
   ("C-z n r" . denote-rename-file)
   ("C-z n b" . denote-backlinks))
  :config
  (setq denote-directory (expand-file-name "~/zettelkasten/"))

  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have a literal "[D]"
  ;; followed by the file's title.  Read the doc string of
  ;; `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1))

(use-package denote-sequence
  :demand t
  :after denote
  :bind
  (("C-z n s" . denote-sequence)
   ("C-z n :" . denote-sequence-new-parent)
   ("C-z n ;" . denote-sequence-new-sibling)
   ("C-z n ," . denote-sequence-new-child)
   ("C-z n <" . denote-sequence-new-child-of-current)
   ("C-z n l" . denote-sequence-link)
   ("C-z n ?" . denote-sequence-find))
  :config
  ;; The default sequence scheme is `numeric'.
  (setq denote-sequence-scheme 'alphanumeric))

(defun zettelkasten-dired ()
  (interactive)
  (dired "~/zettelkasten/" "-Alhr")
  (dired-hide-details-mode 1))

(keybind "C-z n d" zettelkasten-dired)
