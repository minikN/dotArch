;;; ~/git/dotfiles/.doom.d/userconfig/pinentry.el -*- lexical-binding: t; -*-

;; Enable loopback so that pinentry will pop up in emacs
(pinentry-start)

;; Start GPG agent with SSH support
(shell-command "gpg-connect-agent /bye")
