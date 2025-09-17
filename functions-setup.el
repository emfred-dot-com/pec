;;;  -*- lexical-binding: t; -*-
;;; functions-setup.el -- Custom function definitions
;;;

(defun edit-config ()
  "Open init.el for editing"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun consult-recent-file-other-window ()
  "Find recent file using `completing-read'."
  (interactive)
  (find-file-other-window
   (consult--read
    (or
     (mapcar #'consult--fast-abbreviate-file-name (bound-and-true-p recentf-list))
     (user-error "No recent files, `recentf-mode' is %s"
                 (if recentf-mode "enabled" "disabled")))
    :prompt "Find recent file: "
    :sort nil
    :require-match t
    :category 'file
    :state (consult--file-preview)
    :history 'file-name-history)))

(when-mac
 (defun gd (&optional arg)
   "Goto Directory"
   (interactive "P")
   (let ((target-dir (completing-read
		      "Directory: "
		      (with-temp-buffer
			(insert-file-contents "~/.gd_idx")
			(split-string (buffer-string) "\n" t)))))
     (if arg
	 (dired-other-window target-dir)
       (dired (concat "~/" target-dir))))))
(when-linux
 (defun gd (&optional arg)
   "Goto Directory"
   (interactive "P")
   (let ((target-dir (completing-read
		      "Directory: "
		      (with-temp-buffer
			(insert-file-contents "/home/eric/.gd_idx")
			(split-string (buffer-string) "\n" t)))))
     (if arg
	 (dired-other-window target-dir)
       (dired target-dir)))))

(defun change-num-at-point (change-func)
  "Replace the number under the point with (change-func number)"
  (if (number-at-point)
      (let ((new-num (funcall change-func (number-at-point))))
	(backward-sexp)
	(kill-sexp)
	(insert (number-to-string new-num)))))

(defun seek-to-num ()
  "Move the point to the next number in the buffer"
  (if (number-at-point)
      t
    (if (= (point) (buffer-end 1))
	nil
      (progn
	(forward-word)
	(seek-to-num)))))

(defun seek-to-num-and-change (change-func)
  "Go to next number in buffer and replace it with (change-func number)"
  (push-mark)
  (if (seek-to-num)
      (change-num-at-point change-func)
    (progn
      (message "No number found in buffer after point"))))

(defun incr-num-at-point ()
  (interactive)
  (seek-to-num-and-change (lambda (x) (+ x 1))))

(defun decr-num-at-point ()
  (interactive)
  (seek-to-num-and-change (lambda (x) (- x 1))))



(defun copy-line (&optional arg)
  (interactive "P")
  (save-mark-and-excursion
    (unless arg
      (back-to-indentation))
    (push-mark)
    (move-end-of-line 1)
    (activate-mark)
    (kill-ring-save nil nil t)))



(defun scroll-up-one ()
  (interactive)
  (scroll-up-command 1))

(defun scroll-down-one ()
  (interactive)
  (scroll-down-command 1))



(defun goto-line-num-at-point-in-recent-file ()
  "Modified version of `consult-recent-file' that jumps to line
number of `number-at-point' upon file selection"
  (interactive)
  (let ((line-num (number-at-point)))
    (if line-num
	(progn
	  (find-file
	   (consult--read
	    (or
	     (mapcar #'consult--fast-abbreviate-file-name (bound-and-true-p recentf-list))
	     (user-error "No recent files, `recentf-mode' is %s"
			 (if recentf-mode "enabled" "disabled")))
	    :prompt (format "Goto line %d in recent file: " line-num)
	    :sort nil
	    :require-match t
	    :category 'file
	    :state (consult--file-preview)
	    :history 'file-name-history))
	  (goto-line line-num))
      (message "No number-at-point found"))))



(defun set-font-height-in-pts (pts)
  (set-face-attribute 'default nil :height (* 10 pts)))

(defun change-font-size ()
  (interactive)
  (let* ((common-sizes
	  (mapcar #'number-to-string '(8 10 12 14 16 18 20)))
	 (size-in-pts
	  (completing-read "Enter the new font size in pts: " common-sizes)))
    (set-font-height-in-pts (string-to-number size-in-pts))))

(defun xah-unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'.

URL `http://xahlee.info/emacs/emacs/emacs_unfill-paragraph.html'
Version 2016-07-13"
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph)))

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
