;;; ../../.local/share/git/dotArch/.config/doom/userconfig/checkers/dockerfile.el -*- lexical-binding: t; -*-

(flycheck-define-checker dockerfile-hadolint-docker
  "A Dockerfile syntax checker using the hadolint.
See URL `http://github.com/hadolint/hadolint/'."
  :command ("docker" "exec" "-i"
            (eval (concat lsp-docker-dockerfile-container-name "-" (number-to-string lsp-docker-container-name-suffix)))
            "hadolint" "-")
  :standard-input t
  :error-patterns
  ((error line-start
          (file-name) ":" line ":" column " " (message)
          line-end)
   (warning line-start
            (file-name) ":" line " " (id (one-or-more alnum)) " " (message)
            line-end))
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors
     (flycheck-remove-error-file-names "/dev/stdin" errors)))
  :modes dockerfile-mode)

(add-hook! dockerfile-mode
  (add-to-list 'flycheck-checkers 'sh-hadolint-docker)
  (add-to-list 'flycheck-disabled-checkers 'lsp)
  (setq flycheck-checker 'sh-hadolint-docker))
