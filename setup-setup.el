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

(defmacro keybind--keybind (bind-func key func)
  "Helper macro used internally by `keybind' and `keybind-local'."
  (if (listp func)
      `(,bind-func (kbd ,key) ,func)
    `(,bind-func (kbd ,key) (quote ,func))))

(defmacro keybind (key func)
  "Bind KEY, given as a string (e.g. \"C-x s\") to the function
FUNC (given as an unquoted symbol, or as a raw definition)."
  `(keybind--keybind global-set-key ,key ,func))

(defmacro keybind-local (key func)
  "Bind KEY locally, given as a string (e.g. \"C-x s\") to the
function FUNC (given as an unquoted symbol, or as a raw definition)."
  `(keybind--keybind local-set-key ,key ,func))

(defmacro keybinds (&rest keys-functions)
  "Apply the `keybind' macro pairwise through the argument list."
  (let ((key (car keys-functions))
	(func (cadr keys-functions))
	(rest (cddr keys-functions)))
    (when func
	`(progn
	   (keybind ,key ,func)
	   (keybinds ,@rest)))))

(defmacro keybinds-local (&rest keys-functions)
  "Apply the `keybind-local' macro pairwise through the argument list."
  (let ((key (car keys-functions))
	(func (cadr keys-functions))
	(rest (cddr keys-functions)))
    (when func
	`(progn
	   (keybind-local ,key ,func)
	   (keybinds-local ,@rest)))))

;; Leader key:

(global-unset-key (kbd "C-z"))
