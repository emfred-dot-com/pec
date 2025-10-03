;;;  -*- lexical-binding: t; -*-
;;; math-setup.el -- Handling arithmetic while editing
;;;

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

(keybinds
 "C-z C-a" incr-num-at-point
 "C-z C-x" decr-num-at-point)
