;;; ../../.local/share/git/dotArch/.config/doom/userconfig/irc.el -*- lexical-binding: t; -*-

(set-irc-server! "chat.freenode.net"
  `(:tls t
    :port 6697
    :nick "minikN"
    :sasl-username ,(+pass-get-user "IRC/freenode.net")
    :sasl-password (lambda (&rest _) (+pass-get-secret "IRC/freenode.net"))
    :channels ("#nyxt" "#emacs" "#voidlinux")))
