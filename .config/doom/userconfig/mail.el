;;; ../../.local/share/git/dotArch/.config/doom/userconfig/mail.el -*- lexical-binding: t; -*-

(set-email-account! "db@minikn.xyz"
  '((mu4e-sent-folder       . "/db@minikn.xyz/Sent")
    (mu4e-drafts-folder     . "/db@minikn.xyz/Drafts")
    (mu4e-trash-folder      . "/db@minikn.xyz/Trash")
    (smtpmail-smtp-user     . "db@minikn.xyz")
    (smtpmail-smtp-server   . "smtp.mailbox.org")
    (smtpmail-smtp-service  . 587)
    (mu4e-compose-signature . "Mit freundlichen Grüßen / Best regards\nDemis Balbach"))
  t)

(after! mu4e
  (setq mu4e-get-mail-command (concat "mbsync -a -c " (getenv "XDG_CONFIG_HOME") "/isync/mbsyncrc")))

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
