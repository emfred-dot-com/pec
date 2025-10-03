;;;  -*- lexical-binding: t; -*-
;;; writing-setup.el -- Utilities for writing plaintext
;;;

(defun xah-unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'.

URL `http://xahlee.info/emacs/emacs/emacs_unfill-paragraph.html'
Version 2016-07-13"
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph)))

(keybind "M-Q" xah-unfill-paragraph)
