;;;  -*- lexical-binding: t; -*-
;;; functions-setup.el -- Custom function definitions
;;;

(defun copy-line (&optional arg)
  (interactive "P")
  (save-mark-and-excursion
    (unless arg
      (back-to-indentation))
    (push-mark)
    (move-end-of-line 1)
    (activate-mark)
    (kill-ring-save nil nil t)))



(defun set-font-height-in-pts (pts)
  (set-face-attribute 'default nil :height (* 10 pts)))

(defun change-font-size ()
  (interactive)
  (let* ((common-sizes
	  (mapcar #'number-to-string '(8 10 12 14 16 18 20)))
	 (size-in-pts
	  (completing-read "Enter the new font size in pts: " common-sizes)))
    (set-font-height-in-pts (string-to-number size-in-pts))))

(defun calc-hmsify (h-m-s-list)
  (let ((h (car h-m-s-list))
	(m (cadr h-m-s-list))
	(s (caddr h-m-s-list)))
    (concat h (string-to-list "@")
	    m (string-to-list "'")
	    s (string-to-list "\""))))

(defun convert-time-string (str)
  "Convert a time-string from colon-format to calc format, e.g. 2:30:16 ->
  2@ 30' 16\""
  (let* ((h-m-s-list
	  (-split-on (car (string-to-list ":")) (string-to-list str))))
    (mapconcat #'string (calc-hmsify h-m-s-list))))
