;;;  -*- lexical-binding: t; -*-
;;; mail-setup.el -- Setup for reading and sending mail
;;;

(when-mac
 (progn
   (use-package gnus
     :defer t
     :config
     (setq
      gnus-select-method '(nnimap "imap.gmail.com")
      gnus-extra-headers '(To Cc Keywords Gcc Newsgroups X-GM-LABELS)
      gnus-always-read-dribble-file t
      gnus-summary-line-format "%U%R%z%I%(%[ %-4,30f (%L) %]%) %s\n"
      gnus-sum-thread-tree-indent "    "
      gnus-sum-opening-bracket-adopted "{"
      gnus-sum-closing-bracket-adopted "}"
      gnus-widen-article-window t)
     :bind (("C-z m" . gnus)))

   (use-package smtpmail
     :config
     (setq smtpmail-smtp-server "smtp.gmail.com"
	   smtpmail-stream-type 'starttls
	   smtpmail-auth-credentials "~/.authinfo"
	   message-send-mail-function #'smtpmail-send-it)

     (add-hook 'message-mode-hook
	       (=> (setq smtpmail-smtp-service 587))))))

(when-linux
 (progn
   ;; for reading mail
   (use-package notmuch
     :init
     (setq notmuch-fcc-dirs nil
	   notmuch-hello-thousands-separator ","
	   notmuch-show-logo nil
	   notmuch-show-empty-saved-searches nil
	   notmuch-hello-recent-searches-max 5
	   notmuch-search-oldest-first nil)
     (add-hook 'notmuch-search-mode-hook
	       (lambda ()
		 (setq-local notmuch-search-oldest-first nil))))

   ;; modeline indicator for number of unread messages
   (use-package notmuch-indicator
     :init
     (setq notmuch-indicator-args
	   '((:terms "tag:unread and tag:inbox" :label "@"))
	   notmuch-indicator-refresh-count 10
	   notmuch-indicator-force-refresh-commands
	   '(notmuch-refresh-this-buffer)
	   notmuch-indicator-hide-empty-counters nil)
     :config
     (notmuch-indicator-mode 0))

   ;; for sending mail
   (setq send-mail-function 'sendmail-send-it
	 sendmail-program (executable-find "msmtp")
	 mail-specify-envelope-from t
	 message-sendmail-envelope-from 'header
	 mail-envelope-from 'header)

   (keybind "C-z m" (lambda ()
		      (interactive)
		      (notmuch-search-by-tag "inbox")))))
