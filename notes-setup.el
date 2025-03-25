;;;  -*- lexical-binding: t; -*-
;;; notes-setup.el -- Note-taking infrastructure
;;;

;; denote config from Prot!
(use-package denote
  :demand t
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-z n n" . denote)
   ("C-z n r" . denote-rename-file)
   ("C-z n l" . denote-link)
   ("C-z n b" . denote-backlinks))
  :config
  (setq denote-directory (expand-file-name "~/files/text/notes/"))

  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have a literal "[D]"
  ;; followed by the file's title.  Read the doc string of
  ;; `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1))
