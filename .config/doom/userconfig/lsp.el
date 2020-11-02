;;; ~/.doom.d/userconfig/lsp.el -*- lexical-binding: t; -*-

;; Generic settings
(after! lsp-mode
  (setq lsp-auto-guess-root nil
        lsp-file-watch-threshold 10000))

(after! lsp-ui
  (setq lsp-ui-peek-list-width 100
        lsp-ui-peek-fontify 'always
        lsp-ui-doc-position 'top
        lsp-ui-doc-alignment 'window
        lsp-ui-doc-max-height 30
        lsp-ui-doc-max-width 90
        lsp-ui-doc-border "white"
        lsp-ui-imenu-enable nil))

;; Key bindings
(map! :leader
      :desc "Show documentation" "c K" 'lsp-ui-doc-show)

;; Docker
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
  (setq lsp-docker-container-name "lsp-langserver"
        lsp-docker-image-name lsp-docker-container-name
        lsp-docker-path-mappings
        '(("/home/demis/.local/share/git/dotArch" . "/projects/dotArch")
          ("/home/demis/.local/share/git/laravel2" . "/projects/laravel2")
          ("/home/demis/.local/share/git/lsp-langserver" . "/projects/lsp-langserver")))
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
