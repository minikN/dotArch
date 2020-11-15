;;; ../../.local/share/git/dotArch/.config/doom/userconfig/mail.el -*- lexical-binding: t; -*-

(set-email-account! "minikn.xyz"
  '((mu4e-sent-folder       . "/Sent Mail")
    (mu4e-drafts-folder     . "/Drafts")
    (mu4e-trash-folder      . "/Trash")
    (mu4e-refile-folder     . "/All Mail")
    (smtpmail-smtp-user     . "db")
    (mu4e-compose-signature . "Mit freundlichen Grüßen / Best regards\nDemis Balbach"))
  t)
(after! mu4e
  (setq mu4e-get-mail-command (concat "mbsync -a -c " (getenv "XDG_CONFIG_HOME") "/isync/mbsyncrc")))

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
