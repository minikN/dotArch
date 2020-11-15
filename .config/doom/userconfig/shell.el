;;; ../../.local/share/git/dotArch/.config/doom/userconfig/shell.el -*- lexical-binding: t; -*-

;; Set default shell
(defadvice ansi-term (before force-bash)
  (interactive (list "/bin/zsh")))
(ad-activate 'ansi-term)

;; Key binding for launching a shell buffer
(global-set-key (kbd "<s-return>") 'ansi-term)
