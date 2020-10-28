;;; ~/.doom.d/userconfig/lsp.el -*- lexical-binding: t; -*-

;; Generic settings
(setq lsp-ui-peek-list-width 100)
(setq lsp-auto-configure t)
(setq lsp-ui-imenu-enable t)

;; Docker
(defvar lsp-docker-path-mappings
  '(("/home/demis/.local/share/git/dotArch" . "/projects/dotArch")
    ("/home/demis/test-repo" . "/projects/test-repo")
    ("/home/demis/.local/share/git/lsp-langserver" . "/projects/lsp-langserver"))
  "All the available project mappings for the container.")

(defvar lsp-docker-container-name "lsp-langserver"
  "The name of the docker container without suffix.")

(defvar lsp-docker-image-name lsp-docker-container-name
  "The name of the docker image.")

(defun db/get-docker-path (path-mappings project-root)
  "Returns the path inside a docker container
  for a given file using lsp-docker-path-mappings"
  (let ((current-project  ()))
    (dolist (project path-mappings)
      (if (string= (concat (car project) "/") project-root)
          (push (cons (cdr project) (car project)) current-project)))
    (concat (car (car current-project)) "/" (file-relative-name buffer-file-name project-root))))

;;; Configure and initialize lsp-docker with the settings above.
(use-package! lsp-docker
  :config
  (lsp-docker-init-clients
   :docker-image-id lsp-docker-image-name
   :docker-container-name lsp-docker-container-name
   :client-packages '(lsp-bash lsp-dockerfile lsp-php)
   :client-configs (list
                    (list :server-id 'bash-ls :docker-server-id 'bashls-docker :server-command "bash-language-server start")
                    (list :server-id 'iph :docker-server-id 'phpls-docker :server-command "intelephense --stdio")
                    (list :server-id 'dockerfile-ls :docker-server-id 'dockerfilels-docker :server-command "docker-langserver --stdio"))
   :path-mappings lsp-docker-path-mappings))

;; After lsp-docker, lets configure some new flycheck checkers and enable them.
(after! lsp-docker

  ;;; Checker for shell, bash
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

  ;;; Checker for zsh
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
    (setq flycheck-checker 'sh-shellcheck-docker)))
