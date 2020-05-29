;;; ~/git/dotfiles/.doom.d/userconfig/org.el -*- lexical-binding: t; -*-

;; Setup org mode
(setq
 ;; Set todo types
 org-todo-keywords '((type "TODO(t)" "INPROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))

 org-toto-keyword-faces
 '(("TODO" :inherit 'font-lock-string-face :italic italic)
   ("DONE" :inherit 'font-lock-method-face))

 ;; Set files to scan for todos
 org-agenda-files (ignore-errors (directory-files +org-dir t "\\.org$" t)))
