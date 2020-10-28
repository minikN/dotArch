;;; ../../.local/share/git/dotArch/.config/doom/userconfig/checkers/sh.el -*- lexical-binding: t; -*-

;;; bash, sh
(flycheck-define-checker sh-shellcheck-docker
  "Use shellcheck inside a docker container"
  :command ("docker" "exec" "-i"
            (eval (concat lsp-docker-container-name "-" (number-to-string lsp-docker-container-name-suffix)))
            "shellcheck"
            "--format" "checkstyle"
            "--shell" (eval (symbol-name sh-shell))
            (option-flag "--external-sources"
                         flycheck-shellcheck-follow-sources)
            (option "--exclude" flycheck-shellcheck-excluded-warnings list
                    flycheck-option-comma-separated-list)
            "-")
  :standard-input t
  :modes sh-mode
  :error-parser flycheck-parse-checkstyle
  :error-filter (lambda (errors)
                  (flycheck-remove-error-file-names "-" errors))
  :predicate (lambda () (memq sh-shell '(bash ksh88 sh)))
  :verify
  (lambda (_)
    (let ((supported (memq sh-shell '(bash ksh88 sh))))
      (list (flycheck-verification-result-new
             :label (format "Shell %s supported" sh-shell)
             :message (if supported "yes" "no")
             :face (if supported 'success '(bold warning))))))
  :error-explainer
  (lambda (err)
    (let ((error-code (flycheck-error-id err))
          (url "https://github.com/koalaman/shellcheck/wiki/%S"))
      (and error-code `(url . ,(format url error-code))))))

;;; zsh
(flycheck-define-checker sh-zsh-docker
  "sh-zsh with docker support."
  :command ("docker" "exec" "-i"
            (eval (concat lsp-docker-container-name "-" (number-to-string lsp-docker-container-name-suffix)))
            "zshwrapper" (eval (db/get-docker-path lsp-docker-path-mappings (projectile-project-root)))
            (eval (buffer-file-name)))
  :error-patterns
  ((error line-start (file-name) ":" line ": " (message) line-end))
  :modes sh-mode
  :predicate (lambda () (eq sh-shell 'zsh))
  :next-checkers ((warning . sh-shellcheck)))

;; Disable lsp checker and enable/select above checker for sh-mode
(add-hook! sh-mode
  (add-to-list 'flycheck-checkers 'sh-shellcheck-docker)
  (add-to-list 'flycheck-checkers 'sh-zsh-docker)
  (add-to-list 'flycheck-disabled-checkers 'lsp)
  (setq flycheck-checker 'sh-shellcheck-docker))
