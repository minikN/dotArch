;;; ~/.doom.d/userconfig/lsp.el -*- lexical-binding: t; -*-

;; Generic settings
(setq lsp-ui-peek-list-width 100)
(setq lsp-ui-imenu-enable t)
(setq lsp-ui-peek-fontify 'always)

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
  ;;; sh-mode (sh, bash, zsh)
  (load! "checkers/sh"))
