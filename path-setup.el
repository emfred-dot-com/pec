;;;  -*- lexical-binding: t; -*-
;;; path-setup.el -- Settings for load path and exec path
;;;

(mapcar
 (lambda (path)
   (add-to-list 'exec-path path))
 personal/exec-path-additions)
