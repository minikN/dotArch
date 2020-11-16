;;; ~/git/dotfiles/.doom.d/userconfig/pinentry.el -*- lexical-binding: t; -*-

;; Enable loopback so that pinentry will pop up in emacs
(pinentry-start)

;; Start GPG agent with SSH support
(shell-command "gpg-connect-agent /bye")

;; Reload the GPG agent if pinentry hangs
(defun db/reload-gpg-agent ()
  "Reload the GPG agent."
  (interactive)
  (shell-command "gpgconf --kill gpg-agent")
  (message "Reloaded the GPG agent"))

;; Add keybinding to reload GPG agent
(map! :leader
      :desc "Reload GPG agent" "g a" #'db/reload-gpg-agent)
