;;; ~/git/dotfiles/.doom.d/userconfig/pinentry.el -*- lexical-binding: t; -*-

;; Enable loopback so that pinentry will pop up in emacs
(setq epa-pinentry-mode 'loopback)
(pinentry-start)

;; Update the TTY for gpg-agent
;(setenv "GPG_TTY" "/dev/pts/0")
;(shell-command "gpgconf --launch-agent")
;(shell-command "gpg-connect-agent updatestartuptty /bye >/dev/null")
(defun wg/kludge-gpg-agent ()
  (interactive)
  (if
      (display-graphic-p)
      (setenv "DISPLAY"
              (terminal-name))
    (setenv "GPG_TTY"
            (terminal-name))
    (setenv "DISPLAY")))

(advice-add 'magit-push :after #'wg/kludge-gpg-agent)
;;(add-hook 'window-configuration-change-hook 'wg/kludge-gpg-agent)
