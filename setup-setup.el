;;;  -*- lexical-binding: t; -*-
;;; setup-setup.el -- Configurations to enable or simplify later configuration
;;;

;; Platform:

(defcustom myOs "linux"
  "Variable used to guard settings that only work on Linux or MacOs."
  :type 'string)

(if (eq system-type 'darwin)
    (setq myOs "mac"))

(defmacro when-mac (then)
  `(when (string-equal myOs "mac")
     ,then))

(defmacro when-linux (then)
  `(when (string-equal myOs "linux")
     ,then))

;; Macros:

(defmacro => (&rest body)
  "Turn BODY into a lambda function."
  `(lambda ()
     ,@body))

(defmacro ==> (&rest body)
  "Turn BODY into an interactively-callable lambda function."
  `(lambda ()
     (interactive)
     ,@body))

(defmacro keybind (key func)
  "Bind KEY, given as a string (e.g. \"C-x s\") to the function FUNC (given
  unquoted)."
  `(global-set-key (kbd ,key) (quote ,func)))

(defmacro keybinds (&rest keys-functions)
  "Apply the `keybind' macro pairwise through the argument list."
  (let ((key (car keys-functions))
	(func (cadr keys-functions))
	(rest (cddr keys-functions)))
    (when func
	`(progn
	   (keybind ,key ,func)
	   (keybinds ,@rest)))))

;; Leader key:

(global-unset-key (kbd "C-z"))
